#' This file makes greenspace files from NLCD data and then creates 30m rasters
#' that show the minimum distance to varying sizes of greenspaces
#' using duckdb
#' duckdb seems to be much faster and more intuitive than postgis or terra for
#' this purpose. From about 10 minutes to a few hours.
#' Much faster than the days the other methods took

#  ---------------------------------------
# Make regional greenspaces for training SDM from NLCD ---------------------------------------
# ---------------------------------------

# Prep greenspace sizes ---------------------------------------
# 1,5,10,15 acre size
library(terra)
library(tidyterra)
library(sf)
library(units)
library(tidyverse)
gs.sr <- rast("data/predictors/nlcd_greenspace.tif") |>
  project("epsg:4326")


gs.sv <- as.polygons(gs.sr,
  aggregate = T,
  round = T,
  digits = 0
)

gs_area.sf <- gs.sv %>%
  st_as_sf() %>%
  st_make_valid() |>
  # st_transform(crs = 3310) %>%
  # st_transform(crs = 4326) %>%
  st_cast("POLYGON") %>%
  mutate(
    AREA_m2 = st_area(geometry),
    AREA_acres = st_area(geometry) |> set_units("acre")
  ) #|>
# mutate(geom_wkt = st_as_text(geometry))

gs_area.sf |> write_sf("data/predictor_prep/nlcd_greenspace_area.gpkg")



# Attempt with duckdb ---------------------------------------

library(sf)
library(tidyverse)
library(terra)
# library(stars)
library(duckdb)
library(tidyterra)
library(glue)
library(tictoc)
library(duckdbfs)
# package_version(sf)
# packageVersion('duckdb')

tcon <- dbConnect(duckdb("data/greenspaces/temp.duckdb"))
tcon |> dbExecute("
install spatial; load spatial;
SET memory_limit = '300GB';
SET preserve_insertion_order = false;
SET threads TO 20;")

tcon |> dbListTables()
tcon |> dbGetQuery("DESCRIBE greenspace_geo")

tcon |> dbDisconnect(shutdown = T)

## Read data to duckdb  ---------------------------------------

### Counties  ---------------------------------------
# Process counties entirely within DuckDB

st_crs(st_read("~/Data/Boundaries/Political/CA_Counties/CA_Counties_TIGER2016.shp"))$epsg

tcon |> dbExecute("
CREATE OR REPLACE TABLE baycounties AS
SELECT ST_Union_Agg(
  ST_Buffer(
    ST_SimplifyPreserveTopology(
      ST_TRANSFORM(geom, 'EPSG:3857', 'EPSG:4326', always_xy := true),
      0.05  -- ~5km tolerance in decimal degrees
    ),
    0.1  -- 0.1 degree buffer
  )
) AS geom
FROM ST_Read('~/Data/Boundaries/Political/CA_Counties/CA_Counties_TIGER2016.shp')
WHERE name IN ('Solano', 'Contra Costa', 'Alameda', 'Santa Clara',
               'San Mateo', 'San Francisco', 'Marin', 'Sonoma', 'Napa', 'Santa Cruz');

CREATE INDEX idx_baycounties_geom ON baycounties USING RTREE (geom);
")

tcon |>
  tbl("baycounties") |>
  to_sf(conn = tcon) |>
  plot()

### Greenspace  ---------------------------------------
library(tidyverse)
x <- st_read("data/predictor_prep/nlcd_greenspace_area.gpkg")

x |>
  # filter(AREA_acres < 1) |>
  nrow()


# Fix geometry and tables
tcon |> dbExecute("
CREATE OR REPLACE TABLE greenspace_geo
AS
SELECT * EXCLUDE geom,
  grn.geom,
  ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true) AS geom3310,
  ST_TRANSFORM(ST_Centroid(grn.geom), 'EPSG:4326', 'EPSG:3310', always_xy := true) AS centroid_geom3310,
  ST_Centroid(grn.geom) AS centroid_geom,
  ST_SimplifyPreserveTopology(ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true), 3000) AS simple_geom3310,
  -- ST_MakeValid(ST_Buffer(ST_SimplifyPreserveTopology(ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true), 3000), 50)) AS simple_geombuff3310,
  ST_Boundary(ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true)) AS boundary_geom3310,
  ST_SimplifyPreserveTopology(grn.geom, .05) AS simple_geom
FROM ST_READ('data/predictor_prep/nlcd_greenspace_area.gpkg') AS grn
JOIN baycounties
  ON ST_Intersects(grn.geom, baycounties.geom);

CREATE INDEX idx_grn_geom ON greenspace_geo USING RTREE (geom);
CREATE INDEX idx_grn_centroid_geom ON greenspace_geo USING RTREE (centroid_geom);
CREATE INDEX idx_grn_simple_geom ON greenspace_geo USING RTREE (simple_geom3310);
-- CREATE INDEX idx_grn_simple_geombuff ON greenspace_geo USING RTREE (simple_geombuff3310);
CREATE INDEX idx_grn_simpgeom ON greenspace_geo USING RTREE (simple_geom);
CREATE INDEX idx_grn_cent3310_geom ON greenspace_geo USING RTREE (centroid_geom3310);
")
# ST_SimplifyPreserveTopology was crucial

# Read template raster and convert to polygons
# This takes a while
cub_grid.sf <- read_stars("data/predictors/slope.tif") %>%
  # This adds proper cell values
  mutate(cell_id = 1:prod(dim(.))) |> # Add cell ID before converting to sf
  st_as_sf() |>
  # This removes the cells that are NA in the slope raster, but now we have proper cell IDs
  filter(!is.na(slope.tif))

cub_grid.sf |> write_sf("data/predictor_prep/template.gpkg")
cub_grid.sf |>
  arrange(cell_id) |>
  head(10)

# Fix geometry and tables
# always_xy := true is necessary in st_transform
# see https://github.com/duckdb/duckdb_spatial/issues/211
tcon |> dbExecute("
CREATE OR REPLACE TABLE template_grid_geo
AS
SELECT row_number() OVER () AS id,
  cell_id,
  geom,
  ST_TRANSFORM(geom, 'EPSG:4326', 'EPSG:3310', always_xy := true) AS geom3310,
  ST_Centroid(geom) AS centroid_geom,
  ST_TRANSFORM(ST_Centroid(geom), 'EPSG:4326', 'EPSG:3310', always_xy := true) AS centroid_geom3310
FROM ST_Read('data/predictor_prep/template.gpkg');

CREATE INDEX idx_template_geom ON template_grid_geo USING RTREE (geom);
CREATE INDEX idx_tmp_centroid_geom ON template_grid_geo USING RTREE (centroid_geom);
CREATE INDEX idx_template_cell_id ON template_grid_geo (cell_id);
")



## Nearest neighbor join ---------------------------------------
### Regional greenspace ---------------------------------------

# Set run parameters
tcon |> dbExecute("
SET memory_limit = '30GB';
SET preserve_insertion_order = false;
SET threads TO 20;
PRAGMA enable_progress_bar;
")

nn_query <- glue("
CREATE OR REPLACE TABLE grn_distance_test AS
WITH green AS (SELECT * FROM greenspace_geo WHERE AREA_acres >= 1),
     distances AS (
       SELECT
         template.cell_id,
         template.geom AS template_geom,
         ST_AsText(template.centroid_geom) AS geom_wkt,
         green.AREA_acres,
         ST_Distance(template.centroid_geom3310, green.simple_geom3310) AS distance_meters
       FROM template_grid_geo AS template, green
     )
SELECT
    cell_id,
    template_geom,
    geom_wkt,
    MIN(distance_meters) AS distance_to_greenspace_meters,
    arg_min(AREA_acres, distance_meters) AS nearest_greenspace_acres
FROM distances
GROUP BY cell_id, template_geom, geom_wkt;
")

tcon |> dbGetQuery(paste("EXPLAIN", nn_query))

tic()
tcon |> dbExecute(nn_query)
toc()

# tcon |>
#   tbl("grn_distance_complete") |>
#   head(10) |>
#   collect() |>
#   View()
slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])

sf_dist_complete <- tcon |>
  tbl("grn_distance_test") |>
  select(cell_id, distance_to_greenspace_meters, nearest_greenspace_acres) |>
  collect()

# Join your results back to preserve exact cell alignment

aligned_results <- tibble(cells = empty.sr |> cells()) |>
  left_join(sf_dist_complete |> rename(cells = cell_id)) |>
  # select(cells, distance_to_greenspace_meters)
  select(cells, distance_to_greenspace_meters, nearest_greenspace_acres)


# Convert directly to raster values in the right order
greenspace_nearest_dist <- empty.sr
values(greenspace_nearest_dist) <- aligned_results$distance_to_greenspace_meters

ggplot() +
  tidyterra::geom_spatraster(data = greenspace_nearest_dist)

ggsave("results/test18.pdf")

greenspace_nearest_size <- empty.sr
values(greenspace_nearest_size) <- aligned_results$nearest_greenspace_acres

ggplot() +
  tidyterra::geom_spatraster(data = greenspace_nearest_size)

ggsave("results/test2.pdf", y)

# plot(greenspace_dist130)
greenspace_dist130 |> writeRaster("data/predictors/130acre_greenspace_distance.tif", overwrite = T)










## Nearest neighbor join ---------------------------------------

nn_query <- glue("
WITH green AS
(SELECT * FROM greenspace_geo
WHERE AREA_acres >= %s)

SELECT
    template.id AS template_id,
    template.geom AS template_geom,
    ST_AsText(template.centroid_geom) AS geom_wkt,
    MIN(ST_Distance(template.centroid_geom3310, green.simple_geom3310)) AS distance_to_greenspace_meters,
    arg_min(green.AREA_acres, ST_Distance(template.centroid_geom3310, green.geom3310)) AS nearest_greenspace_acres
FROM
    template_grid_geo AS template, green
GROUP BY
    template.id, template.geom, template.centroid_geom;
")

# tcon |> dbGetQuery(paste("EXPLAIN", nn_query))
tcon |> dbGetQuery(sprintf(paste("EXPLAIN", nn_query), "130"))

# Set run parameters
tcon |> dbExecute("
SET memory_limit = '300GB';
SET preserve_insertion_order = true;
SET threads TO 20;
")


# 130
tic()
tcon |> dbExecute(sprintf(paste("CREATE OR REPLACE TABLE grn_distance_grd_%s AS", nn_query), "130", "130"))
toc()
# 790 seconds

slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])
tcon |> dbListTables()
sf_dist130 <- tcon |>
  tbl("grn_distance_grd_130") |>
  collect() |>
  st_as_sf(wkt = "geom_wkt")

greenspace_dist130 <- sf_dist130 |>
  rasterize(empty.sr,
    field = "distance_to_greenspace_meters"
  )

names(greenspace_dist130) <- "greenspace_dist130"
# plot(greenspace_dist130)
greenspace_dist130 |> writeRaster("data/predictors/130acre_greenspace_distance.tif", overwrite = T)
rast("data/predictors/130acre_greenspace_distance.tif") |> names()

# 75
tic()
tcon |> dbExecute(sprintf(paste("CREATE OR REPLACE TABLE grn_distance_grd_%s AS", nn_query), "75", "75"))
toc()
# 19 minutes

greenspace_dist75 <- tcon |>
  tbl("grn_distance_grd_75") |>
  collect() |>
  st_as_sf(wkt = "geom_wkt") |>
  rasterize(empty.sr, field = "distance_to_greenspace_meters")
names(greenspace_dist75) <- "greenspace_dist75"
plot(greenspace_dist75)
greenspace_dist75 |> writeRaster("data/predictors/75acre_greenspace_distance.tif", overwrite = T)

# 30
tic()
tcon |> dbExecute(glue(sprintf(paste("CREATE OR REPLACE TABLE grn_distance_grd_%s AS", nn_query), "30", "30")))
toc()
# 36 minutes

sf_dist30 <- tcon |>
  tbl("grn_distance_grd_30") |>
  collect() |>
  st_as_sf(wkt = "geom_wkt")

greenspace_dist30 <- sf_dist30 |>
  rasterize(empty.sr, field = "distance_to_greenspace_meters")
# greenspace_dist30 <- rast("data/predictors/30acre_greenspace_distance.tif")
names(greenspace_dist30) <- "greenspace_dist30"
plot(greenspace_dist30)
greenspace_dist30 |> writeRaster("data/predictors/30acre_greenspace_distance.tif",
  overwrite = T
)


# 10
tic()
tcon |> dbExecute(glue(sprintf(paste("CREATE OR REPLACE TABLE grn_distance_grd_%s AS", nn_query), "10", "10")))
toc()
#  minutes

sf_dist10 <- tcon |>
  tbl("grn_distance_grd_10") |>
  collect() |>
  st_as_sf(wkt = "geom_wkt")

greenspace_dist10 <- sf_dist10 |>
  rasterize(empty.sr, field = "distance_to_greenspace_meters")

# greenspace_dist10 <- rast("data/predictors/10acre_greenspace_distance.tif")
names(greenspace_dist10) <- "greenspace_dist10"
plot(greenspace_dist10)
greenspace_dist10 |> writeRaster("data/predictors/10acre_greenspace_distance.tif", overwrite = T)


# 2
tic()
tcon |> dbExecute(glue(sprintf(paste("CREATE OR REPLACE TABLE grn_distance_grd_%s AS", nn_query), "2", "2")))
toc()
#  7 hours

sf_dist2 <- tcon |>
  tbl("grn_distance_grd_2") |>
  collect() |>
  st_as_sf(wkt = "geom_wkt")

greenspace_dist2 <- sf_dist2 |>
  rasterize(empty.sr, field = "distance_to_greenspace_meters")

# greenspace_dist2 <- rast("data/predictors/2acre_greenspace_distance.tif")
names(greenspace_dist2) <- "greenspace_dist2"


plot(greenspace_dist2)
greenspace_dist2 |> writeRaster("data/predictors/2acre_greenspace_distance.tif", overwrite = T)


#  ---------------------------------------
# Make city greenspaces for prediction from Christine's data ---------------------------------------
# ---------------------------------------

# Make essential-only greenspace ---------------------------------------
# Just in the cities

# Add cities shapefile so that we can limit the grid just to cities
# City boundaries
city_boundaries <- st_read("data/city_boundaries.gpkg")

tcon |> copy_to(
  city_boundaries |> tibble() |> select(jurname, geom_wkt),
  name = "baycities",
  overwrite = T
)

# tcon |> dbExecute("CREATE INDEX baycityindx ON baycities USING RTREE (geom)")

tcon |> dbExecute("
CREATE OR REPLACE TABLE template_grid_cities
AS
SELECT *
FROM template_grid_geo, baycities
WHERE ST_Intersects(template_grid_geo.geom, ST_GeomFromText(baycities.geom_wkt));

CREATE INDEX idx_templatecities_geom ON template_grid_cities USING RTREE (geom);
CREATE INDEX idx_tmp_centroidcities_geom ON template_grid_cities USING RTREE (centroid_geom);
")

tcon |> tbl("baycities")
tic()
tcon |> dbExecute("SELECT *
FROM template_grid_geo, baycities
WHERE ST_Intersects(template_grid_geo.geom, ST_GeomFromText(baycities.geom_wkt))")
toc()



# Create a table of essential greenspaces by:
# 1. Reading the city greenspace data
# 2. Filtering for only 'Essential' greenspaces
# 3. Creating transformed geometries in EPSG:3310 (California State Plane III)
# 4. Computing centroids and simplified geometries for spatial operations
# 5. Creating spatial indexes for faster querying
tcon |> dbExecute("
CREATE OR REPLACE TABLE essential_greenspace
AS
SELECT * EXCLUDE geom,
  grn.geom,
  ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true) AS geom3310,
  ST_TRANSFORM(ST_Centroid(grn.geom), 'EPSG:4326', 'EPSG:3310', always_xy := true) AS centroid_geom3310,
  ST_Centroid(grn.geom) AS centroid_geom,
  ST_SimplifyPreserveTopology(ST_MakeValid(ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true)), 5000) AS simple_geom3310
FROM ST_READ('data/greenspaces/city_greenspace.gpkg') AS grn
WHERE Final_CLN2 = 'Essential';

CREATE INDEX idx_grn_ess_geom ON essential_greenspace USING RTREE (geom);
CREATE INDEX idx_grn_ess_centroid_geom ON essential_greenspace USING RTREE (centroid_geom);
CREATE INDEX idx_grn_ess_simple_geom ON essential_greenspace USING RTREE (simple_geom3310);
CREATE INDEX idx_grn_ess_cent3310_geom ON essential_greenspace USING RTREE (centroid_geom3310);
")


tcon |>
  tbl("essential_greenspace") |>
  select(area_acres)
# good

# Query to calculate minimum distance from each grid cell to essential greenspaces of specified size
nness_query <- glue("
WITH green AS
(SELECT * FROM essential_greenspace
WHERE area_acres >= %s)

SELECT
    template.id AS template_id,
    template.geom AS template_geom,
    ST_AsText(template.centroid_geom) AS geom_wkt,
    MIN(ST_Distance(template.centroid_geom3310, green.simple_geom3310)) AS distance_to_greenspace_meters
FROM
    template_grid_cities AS template, green
GROUP BY
    template.id, template.geom, template.centroid_geom;
")

slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])

# Function to create a distance raster showing minimum distance to essential
# greenspaces of a given minimum size using nness_query
makeEssentialDistance <- function(min_greenspace_size) {
  tic()
  tcon |> dbExecute(glue(sprintf(
    paste("CREATE OR REPLACE TABLE essential_greenspace_dist_%s AS", nness_query),
    min_greenspace_size, min_greenspace_size
  )))
  toc()

  sf_dist <- tcon |>
    tbl(paste0("essential_greenspace_dist_", min_greenspace_size)) |>
    collect() |>
    st_as_sf(wkt = "geom_wkt")

  greenspace_dist <- sf_dist |>
    rasterize(empty.sr, field = "distance_to_greenspace_meters")

  names(greenspace_dist) <- paste0("essential_greenspace_dist", min_greenspace_size)

  greenspace_dist |>
    writeRaster(sprintf(
      "data/predictors/%sacre_essential_greenspace_distance.tif",
      min_greenspace_size
    ), overwrite = T)
}

# do it for all distances
c(130, 75, 30, 10, 2) |> map(makeEssentialDistance)

# Checking
rast("data/predictors/130acre_essential_greenspace_distance.tif") |> plot(main = "130 acre essential greenspace distance")
rast("data/predictors/2acre_essential_greenspace_distance.tif") |> plot(main = "2 acre essential greenspace distance")


# City-only all greenspace ---------------------------------------
#' I need to make the greenspace distance rasters for all greenspaces in the city
#' specifically because if I just crop the regional one it will have higher values
#' at the edges. I need to measure distances just from internal greenspaces

tcon |> dbExecute("
CREATE OR REPLACE TABLE city_all_greenspace
AS
SELECT * EXCLUDE geom,
  grn.geom,
  ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true) AS geom3310,
  ST_TRANSFORM(ST_Centroid(grn.geom), 'EPSG:4326', 'EPSG:3310', always_xy := true) AS centroid_geom3310,
  ST_Centroid(grn.geom) AS centroid_geom,
  ST_SimplifyPreserveTopology(ST_MakeValid(ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true)), 5000) AS simple_geom3310
FROM ST_READ('data/greenspaces/city_greenspace.gpkg') AS grn;

CREATE INDEX idx_grn_all_geom ON city_all_greenspace USING RTREE (geom);
CREATE INDEX idx_grn_all_centroid_geom ON city_all_greenspace USING RTREE (centroid_geom);
CREATE INDEX idx_grn_all_simple_geom ON city_all_greenspace USING RTREE (simple_geom3310);
CREATE INDEX idx_grn_all_cent3310_geom ON city_all_greenspace USING RTREE (centroid_geom3310);
")


nncityall_query <- glue("
WITH green AS
(SELECT * FROM city_all_greenspace
WHERE area_acres >= %s)

SELECT
    template.id AS template_id,
    template.geom AS template_geom,
    ST_AsText(template.centroid_geom) AS geom_wkt,
    MIN(ST_Distance(template.centroid_geom3310, green.simple_geom3310)) AS distance_to_greenspace_meters
FROM
    template_grid_cities AS template, green
GROUP BY
    template.id, template.geom, template.centroid_geom;
")

makeAllCityDistance <- function(min_greenspace_size) {
  tic()
  tcon |> dbExecute(glue(sprintf(paste("CREATE OR REPLACE TABLE city_all_greenspace_dist_%s AS", nncityall_query), min_greenspace_size, min_greenspace_size)))
  toc()

  sf_dist <- tcon |>
    tbl(paste0("city_all_greenspace_dist_", min_greenspace_size)) |>
    collect() |>
    st_as_sf(wkt = "geom_wkt")

  greenspace_dist <- sf_dist |>
    rasterize(empty.sr, field = "distance_to_greenspace_meters")

  names(greenspace_dist) <- paste0("city_all_greenspace_dist", min_greenspace_size)

  greenspace_dist |>
    writeRaster(sprintf(
      "data/predictors/%sacre_city_all_greenspace_dist.tif",
      min_greenspace_size
    ), overwrite = T)
}

c(130, 75, 30, 10, 2) |> map(makeAllCityDistance)

rast(c(
  "data/predictors/130acre_city_all_greenspace_dist.tif",
  "data/predictors/2acre_city_all_greenspace_dist.tif"
)) |> plot()
# Looks good

# City-only non-essential greenspace ---------------------------------------
#' Calculate distances to non-essential greenspaces in the city
#' This follows the same pattern as essential and all greenspaces but filters for non-essential ones
slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])
# Set memory limit for non-essential greenspace calculations
tcon |> dbExecute("
SET memory_limit = '200GB';
SET preserve_insertion_order = true;
SET threads TO 20;
")


tcon |> dbExecute("
CREATE OR REPLACE TABLE city_nonessential_greenspace
AS
SELECT * EXCLUDE geom,
  grn.geom,
  ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true) AS geom3310,
  ST_TRANSFORM(ST_Centroid(grn.geom), 'EPSG:4326', 'EPSG:3310', always_xy := true) AS centroid_geom3310,
  ST_Centroid(grn.geom) AS centroid_geom,
  ST_SimplifyPreserveTopology(ST_MakeValid(ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true)), 5000) AS simple_geom3310,
  ST_Area(ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true)) / 4046.86 AS recalc_acres
FROM ST_READ('data/greenspaces/city_greenspace.gpkg') AS grn
WHERE Final_CLN2 != 'Essential';

CREATE INDEX idx_grn_noness_geom ON city_nonessential_greenspace USING RTREE (geom);
CREATE INDEX idx_grn_noness_centroid_geom ON city_nonessential_greenspace USING RTREE (centroid_geom);
CREATE INDEX idx_grn_noness_simple_geom ON city_nonessential_greenspace USING RTREE (simple_geom3310);
CREATE INDEX idx_grn_noness_cent3310_geom ON city_nonessential_greenspace USING RTREE (centroid_geom3310);
")

nncitynoness_query <- glue("
WITH green AS
(SELECT * FROM city_nonessential_greenspace
WHERE area_acres >= %s)

SELECT
    template.id AS template_id,
    template.geom AS template_geom,
    ST_AsText(template.centroid_geom) AS geom_wkt,
    MIN(ST_Distance(template.centroid_geom3310, green.simple_geom3310)) AS distance_to_greenspace_meters
FROM
    template_grid_cities AS template, green
GROUP BY
    template.id, template.geom, template.centroid_geom;
")

makeNonEssentialCityDistance <- function(min_greenspace_size) {
  tic()
  tcon |> dbExecute(glue(sprintf(paste("CREATE OR REPLACE TABLE city_nonessential_greenspace_dist_%s AS", nncitynoness_query), min_greenspace_size, min_greenspace_size)))
  toc()

  sf_dist <- tcon |>
    tbl(paste0("city_nonessential_greenspace_dist_", min_greenspace_size)) |>
    collect() |>
    st_as_sf(wkt = "geom_wkt")

  greenspace_dist <- sf_dist |>
    rasterize(empty.sr, field = "distance_to_greenspace_meters")

  names(greenspace_dist) <- paste0("city_nonessential_greenspace_dist", min_greenspace_size)

  greenspace_dist |>
    writeRaster(sprintf(
      "data/predictors/%sacre_city_nonessential_greenspace_dist.tif",
      min_greenspace_size
    ), overwrite = T)
}

c(130, 75, 30, 10, 2) |> map(makeNonEssentialCityDistance)

# Check the results
rast(c(
  "data/predictors/130acre_city_nonessential_greenspace_dist.tif",
  "data/predictors/2acre_city_nonessential_greenspace_dist.tif"
)) |> plot()

# Try more aggressive simplification to reduce streaking
# Test with 10km tolerance instead of 5km
tcon |> dbExecute("
CREATE OR REPLACE TABLE greenspace_geo_smooth AS
SELECT * EXCLUDE geom,
  grn.geom,
  ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true) AS geom3310,
  ST_TRANSFORM(ST_Centroid(grn.geom), 'EPSG:4326', 'EPSG:3310', always_xy := true) AS centroid_geom3310,
  ST_Centroid(grn.geom) AS centroid_geom,
  ST_SimplifyPreserveTopology(ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true), 10000) AS simple_geom3310,
  ST_SimplifyPreserveTopology(grn.geom, .1) AS simple_geom
FROM ST_READ('data/predictor_prep/nlcd_greenspace_area.gpkg') AS grn
JOIN baycounties
  ON ST_Intersects(grn.geom, baycounties.geom);

CREATE INDEX idx_grn_smooth_geom ON greenspace_geo_smooth USING RTREE (geom);
CREATE INDEX idx_grn_smooth_simple_geom ON greenspace_geo_smooth USING RTREE (simple_geom3310);
")

# Alternative: Try buffer-based approach to create smoother edges
tcon |> dbExecute("
CREATE OR REPLACE TABLE greenspace_geo_buffered AS
SELECT * EXCLUDE geom,
  grn.geom,
  ST_Buffer(ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true), -100) AS geom3310,
  ST_TRANSFORM(ST_Centroid(grn.geom), 'EPSG:4326', 'EPSG:3310', always_xy := true) AS centroid_geom3310,
  ST_Centroid(grn.geom) AS centroid_geom,
  ST_Buffer(
    ST_SimplifyPreserveTopology(
      ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true),
      5000
    ),
    -100  -- Small negative buffer to smooth edges
  ) AS simple_geom3310
FROM ST_READ('data/predictor_prep/nlcd_greenspace_area.gpkg') AS grn
JOIN baycounties
  ON ST_Intersects(grn.geom, baycounties.geom);

CREATE INDEX idx_grn_buff_geom ON greenspace_geo_buffered USING RTREE (geom3310);
CREATE INDEX idx_grn_buff_simple_geom ON greenspace_geo_buffered USING RTREE (simple_geom3310);
")

# Test different approaches to reduce streaking while maintaining edge distances

# Function to test smoothed geometries
test_smooth_distance <- function(min_greenspace_size = 130) {
  query <- glue("
  WITH green AS
  (SELECT * FROM greenspace_geo_smooth
  WHERE AREA_acres >= {min_greenspace_size})

  SELECT
      template.id AS template_id,
      template.geom AS template_geom,
      ST_AsText(template.centroid_geom) AS geom_wkt,
      MIN(ST_Distance(template.centroid_geom3310, green.simple_geom3310)) AS distance_to_greenspace_meters
  FROM
      template_grid_geo AS template, green
  GROUP BY
      template.id, template.geom, template.centroid_geom;
  ")

  tic()
  tcon |> dbExecute(glue("CREATE OR REPLACE TABLE test_smooth_{min_greenspace_size} AS {query}"))
  toc()

  return(glue("test_smooth_{min_greenspace_size}"))
}

# Function to test buffered geometries
test_buffered_distance <- function(min_greenspace_size = 130) {
  query <- glue("
  WITH green AS
  (SELECT * FROM greenspace_geo_buffered
  WHERE AREA_acres >= {min_greenspace_size})

  SELECT
      template.id AS template_id,
      template.geom AS template_geom,
      ST_AsText(template.centroid_geom) AS geom_wkt,
      MIN(ST_Distance(template.centroid_geom3310, green.simple_geom3310)) AS distance_to_greenspace_meters
  FROM
      template_grid_geo AS template, green
  GROUP BY
      template.id, template.geom, template.centroid_geom;
  ")

  tic()
  tcon |> dbExecute(glue("CREATE OR REPLACE TABLE test_buffered_{min_greenspace_size} AS {query}"))
  toc()

  return(glue("test_buffered_{min_greenspace_size}"))
}

# Test the approaches
smooth_table <- test_smooth_distance(130)
buffered_table <- test_buffered_distance(130)

# Convert to rasters for comparison
slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])

# Smooth geometry approach
sf_smooth <- tcon |>
  tbl(smooth_table) |>
  collect() |>
  st_as_sf(wkt = "geom_wkt")

greenspace_smooth <- sf_smooth |>
  rasterize(empty.sr, field = "distance_to_greenspace_meters")

# Buffered geometry approach
sf_buffered <- tcon |>
  tbl(buffered_table) |>
  collect() |>
  st_as_sf(wkt = "geom_wkt")

greenspace_buffered <- sf_buffered |>
  rasterize(empty.sr, field = "distance_to_greenspace_meters")

# Plot comparison
library(patchwork)
p1 <- ggplot() +
  tidyterra::geom_spatraster(data = greenspace_smooth) +
  ggtitle("Smooth (10km tolerance)")
p2 <- ggplot() +
  tidyterra::geom_spatraster(data = greenspace_buffered) +
  ggtitle("Buffered (-100m)")

comparison_plot <- p1 + p2
ggsave("results/streak_comparison.pdf", comparison_plot, width = 12, height = 6)

# More efficient alternatives to the cross-product approach

# Option 1: Use ST_DWithin to limit search radius (much faster)
efficient_nn_query_v1 <- glue("
WITH green AS (SELECT * FROM greenspace_geo WHERE AREA_acres >= %s)

SELECT
    template.id AS template_id,
    template.cell_id AS cell_id,
    template.geom AS template_geom,
    ST_AsText(template.centroid_geom) AS geom_wkt,
    MIN(ST_Distance(template.centroid_geom3310, green.simple_geom3310)) AS distance_to_greenspace_meters
FROM
    template_grid_geo AS template
JOIN green ON ST_DWithin(template.centroid_geom3310, green.centroid_geom3310, 50000)  -- 50km radius
GROUP BY
    template.id, template.cell_id, template.geom, template.centroid_geom;
")

tic()
tcon |> dbExecute(glue(sprintf(paste("CREATE OR REPLACE TABLE grn_distance_test AS", efficient_nn_query_v1), "500")))
toc()


sf_dist_complete <- tcon |>
  tbl("grn_distance_test") |>
  select(cell_id, distance_to_greenspace_meters) |>
  collect() #|>

aligned_results <- tibble(cells = empty.sr |> cells()) |>
  left_join(sf_dist_complete |> rename(cells = cell_id)) |>
  select(cells, distance_to_greenspace_meters)

greenspace_nearest_dist <- empty.sr
values(greenspace_nearest_dist) <- aligned_results$distance_to_greenspace_meters

ggplot() +
  tidyterra::geom_spatraster(data = greenspace_nearest_dist)

# Option 2: Two-stage approach - coarse then fine
efficient_nn_query_v2 <- glue("
WITH green AS (SELECT * FROM greenspace_geo WHERE AREA_acres >= %s),
     coarse_candidates AS (
       SELECT DISTINCT
         template.id,
         template.geom,
         template.centroid_geom,
         template.centroid_geom3310
       FROM template_grid_geo AS template
       JOIN green ON ST_DWithin(template.centroid_geom3310, green.centroid_geom3310, 25000)
     )

SELECT
    template.id AS template_id,
    template.cell_id AS cell_id,
    template.geom AS template_geom,
    ST_AsText(template.centroid_geom) AS geom_wkt,
    MIN(ST_Distance(template.centroid_geom3310, green.centroid_geom3310)) AS distance_to_greenspace_meters
FROM
    coarse_candidates AS template, green
GROUP BY
    template.id, template.cell_id, template.geom, template.centroid_geom;
")

tic()
tcon |> dbExecute(glue(sprintf(paste("CREATE OR REPLACE TABLE grn_distance_test AS", efficient_nn_query_v1), "500")))
toc()


sf_dist_complete <- tcon |>
  tbl("grn_distance_test") |>
  select(cell_id, distance_to_greenspace_meters) |>
  collect() #|>

aligned_results <- tibble(cells = empty.sr |> cells()) |>
  left_join(sf_dist_complete |> rename(cells = cell_id)) |>
  select(cells, distance_to_greenspace_meters)

greenspace_nearest_dist <- empty.sr
values(greenspace_nearest_dist) <- aligned_results$distance_to_greenspace_meters

ggplot() +
  tidyterra::geom_spatraster(data = greenspace_nearest_dist)

# Option 3: Spatial partitioning using grid tiles
efficient_nn_query_v3 <- glue("
WITH green AS (SELECT * FROM greenspace_geo WHERE AREA_acres >= %s),
     spatial_grid AS (
       SELECT
         template.*,
         FLOOR(ST_X(centroid_geom3310) / 10000) * 10000 AS grid_x,
         FLOOR(ST_Y(centroid_geom3310) / 10000) * 10000 AS grid_y
       FROM template_grid_geo AS template
     ),
     green_grid AS (
       SELECT
         green.*,
         FLOOR(ST_X(centroid_geom3310) / 10000) * 10000 AS grid_x,
         FLOOR(ST_Y(centroid_geom3310) / 10000) * 10000 AS grid_y
       FROM green
     )

SELECT
    template.id AS template_id,
    template.cell_id AS cell_id,
    template.geom AS template_geom,
    ST_AsText(template.centroid_geom) AS geom_wkt,
    MIN(ST_Distance(template.centroid_geom3310, green.centroid_geom3310)) AS distance_to_greenspace_meters
FROM
    spatial_grid AS template
JOIN green_grid AS green ON (
    green.grid_x BETWEEN template.grid_x - 10000 AND template.grid_x + 10000 AND
    green.grid_y BETWEEN template.grid_y - 10000 AND template.grid_y + 10000
)
GROUP BY
    template.id, template.cell_id, template.geom, template.centroid_geom;
")

# Option 4: Use convex hulls with spatial filtering
efficient_hull_query <- glue("
WITH green AS (SELECT * FROM greenspace_geo WHERE AREA_acres >= %s)

SELECT
    template.id AS template_id,
    template.cell_id AS cell_id,
    template.geom AS template_geom,
    ST_AsText(template.centroid_geom) AS geom_wkt,
    MIN(ST_Distance(template.centroid_geom3310, green.hull_geom3310)) AS distance_to_greenspace_meters
FROM
    template_grid_geo AS template
JOIN green ON ST_DWithin(template.centroid_geom3310, green.hull_geom3310, 30000)  -- 30km radius
GROUP BY
    template.id, template.cell_id, template.geom, template.centroid_geom;
")

# Test functions for each approach
test_efficient_v1 <- function(min_greenspace_size = 130) {
  query <- sprintf(efficient_nn_query_v1, min_greenspace_size)
  tic()
  tcon |> dbExecute(glue("CREATE OR REPLACE TABLE efficient_v1_{min_greenspace_size} AS {query}"))
  toc()
  cat("V1 (ST_DWithin) completed\n")
}

test_efficient_v2 <- function(min_greenspace_size = 130) {
  query <- sprintf(efficient_nn_query_v2, min_greenspace_size)
  tic()
  tcon |> dbExecute(glue("CREATE OR REPLACE TABLE efficient_v2_{min_greenspace_size} AS {query}"))
  toc()
  cat("V2 (Two-stage) completed\n")
}

test_efficient_v3 <- function(min_greenspace_size = 130) {
  query <- sprintf(efficient_nn_query_v3, min_greenspace_size)
  tic()
  tcon |> dbExecute(glue("CREATE OR REPLACE TABLE efficient_v3_{min_greenspace_size} AS {query}"))
  toc()
  cat("V3 (Spatial grid) completed\n")
}

test_efficient_hull <- function(min_greenspace_size = 130) {
  query <- sprintf(efficient_hull_query, min_greenspace_size)
  tic()
  tcon |> dbExecute(glue("CREATE OR REPLACE TABLE efficient_hull_{min_greenspace_size} AS {query}"))
  toc()
  cat("Hull approach completed\n")
}

# Run performance comparison
cat("Testing efficient approaches...\n")
# test_efficient_v1(130)  # Start with this one - should be much faster
# test_efficient_v2(130)
# test_efficient_hull(130)  # This uses your hull_geom3310 column

# Check for invalid geometries that might be causing streaks
tcon |> dbGetQuery("
SELECT
  COUNT(*) as total_geometries,
  COUNT(CASE WHEN ST_IsValid(geom3310) THEN 1 END) as valid_geom3310,
  COUNT(CASE WHEN ST_IsValid(simple_geom3310) THEN 1 END) as valid_simple_geom3310,
  COUNT(CASE WHEN ST_IsValid(simple_geombuff3310) THEN 1 END) as valid_simple_geombuff3310
FROM greenspace_geo
WHERE AREA_acres >= 500
")
