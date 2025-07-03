#' This file makes greenspace files from NLCD data and then creates 30m rasters
#' that show the minimum distance to varying sizes of greenspaces
#' using duckdb
#' duckdb seems to be much faster and more intuitive than postgis or terra for
#' this purpose. From about 10 minutes to a few hours.
#' Much faster than the days the other methods took
#'
# Produces outputs:
# - data/predictors/greenspace_nearest_dist.tif
# - data/predictors/greenspace_nearest_size.tif

#  ---------------------------------------
# Make regional greenspaces for training SDM from NLCD ---------------------------------------
# ---------------------------------------

# Prep greenspace sizes ---------------------------------------
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
CREATE OR REPLACE TABLE grn_distance_complete_nlcd AS
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

slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])

sf_dist_complete <- tcon |>
  tbl("grn_distance_complete_nlcd") |>
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
names(greenspace_nearest_dist) <- "greenspace_nearest_dist"

ggplot() +
  tidyterra::geom_spatraster(data = greenspace_nearest_dist)

greenspace_nearest_dist |> writeRaster("data/predictors/greenspace_nearest_dist.tif", overwrite = T)

# ggsave("results/test18.pdf")

greenspace_nearest_size <- empty.sr
values(greenspace_nearest_size) <- aligned_results$nearest_greenspace_acres
names(greenspace_nearest_size) <- "greenspace_nearest_size"

greenspace_nearest_size |> writeRaster("data/predictors/greenspace_nearest_size.tif", overwrite = T)

ggplot() +
  tidyterra::geom_spatraster(data = greenspace_nearest_size)

# ggsave("results/test2.pdf", y)






#  ---------------------------------------
# Make city greenspaces for prediction from Christine's data ---------------------------------------
# ---------------------------------------

# Make essential-only greenspace ---------------------------------------
# Just in the cities

# Add cities shapefile so that we can limit the grid just to cities
# City boundaries
# city_boundaries <- st_read("data/city_boundaries.gpkg")

# tcon |> copy_to(
#   city_boundaries |> tibble() |> select(jurname, geom_wkt),
#   name = "baycities",
#   overwrite = T
# )

st_crs(st_read("data/city_boundaries.gpkg"))$epsg

tcon |> dbExecute("
CREATE OR REPLACE TABLE baycities AS
SELECT *
FROM ST_Read('data/city_boundaries.gpkg');

CREATE INDEX idx_baycities_geom ON baycities USING RTREE (geom);
")

# tcon |> dbExecute("CREATE INDEX baycityindx ON baycities USING RTREE (geom)")

tcon |> dbExecute("
CREATE OR REPLACE TABLE template_grid_cities
AS
SELECT *
FROM template_grid_geo, baycities
-- WHERE ST_Intersects(template_grid_geo.geom, ST_GeomFromText(baycities.geom_wkt));
WHERE ST_Intersects(template_grid_geo.geom, baycities.geom);

CREATE INDEX idx_templatecities_geom ON template_grid_cities USING RTREE (geom);
CREATE INDEX idx_tmp_centroidcities_geom ON template_grid_cities USING RTREE (centroid_geom);
")

tcon |> tbl("template_grid_cities")


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
# nness_query <- glue("
# WITH green AS
# (SELECT * FROM essential_greenspace
# WHERE area_acres >= %s)

# SELECT
#     template.id AS template_id,
#     template.geom AS template_geom,
#     ST_AsText(template.centroid_geom) AS geom_wkt,
#     MIN(ST_Distance(template.centroid_geom3310, green.simple_geom3310)) AS distance_to_greenspace_meters
# FROM
#     template_grid_cities AS template, green
# GROUP BY
#     template.id, template.geom, template.centroid_geom;
# ")

nness_query <- glue("
CREATE OR REPLACE TABLE essential_greenspace_dist_complete AS
WITH green AS (SELECT * FROM essential_greenspace WHERE AREA_acres >= 1),
     distances AS (
       SELECT
         template.cell_id,
         template.geom AS template_geom,
         ST_AsText(template.centroid_geom) AS geom_wkt,
         green.AREA_acres,
         ST_Distance(template.centroid_geom3310, green.simple_geom3310) AS distance_meters
       FROM template_grid_cities AS template, green
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

tic()
tcon |> dbExecute(nness_query)
toc()

slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])


sf_ess_dist_complete <- tcon |>
  tbl("essential_greenspace_dist_complete") |>
  select(cell_id, distance_to_greenspace_meters, nearest_greenspace_acres) |>
  collect()

# Join your results back to preserve exact cell alignment

ess_aligned_results <- tibble(cells = empty.sr |> cells()) |>
  left_join(sf_ess_dist_complete |> rename(cells = cell_id)) |>
  select(cells, distance_to_greenspace_meters, nearest_greenspace_acres)

# Convert directly to raster values in the right order
ess_greenspace_nearest_dist <- empty.sr
values(ess_greenspace_nearest_dist) <- ess_aligned_results$distance_to_greenspace_meters
names(ess_greenspace_nearest_dist) <- "essential_greenspace_nearest_dist"

ggplot() +
  tidyterra::geom_spatraster(data = ess_greenspace_nearest_dist)

ess_greenspace_nearest_dist |> writeRaster("data/predictors/essential_greenspace_nearest_dist.tif", overwrite = T)

ess_greenspace_nearest_size <- empty.sr
values(ess_greenspace_nearest_size) <- ess_aligned_results$nearest_greenspace_acres
names(ess_greenspace_nearest_size) <- "essential_greenspace_nearest_size"

ess_greenspace_nearest_size |> writeRaster("data/predictors/essential_greenspace_nearest_size.tif", overwrite = T)

ggplot() +
  tidyterra::geom_spatraster(data = ess_greenspace_nearest_size)






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

# Calculate distances to all greenspaces in the city
nncityall_query <- glue("
CREATE OR REPLACE TABLE city_all_greenspace_dist_complete AS
WITH green AS (SELECT * FROM city_all_greenspace WHERE AREA_acres >= 1),
     distances AS (
       SELECT
         template.cell_id,
         template.geom AS template_geom,
         ST_AsText(template.centroid_geom) AS geom_wkt,
         green.AREA_acres,
         ST_Distance(template.centroid_geom3310, green.simple_geom3310) AS distance_meters
       FROM template_grid_cities AS template, green
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

tcon |> dbGetQuery(paste("EXPLAIN", nncityall_query))

tic()
tcon |> dbExecute(nncityall_query)
toc()

slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])


sf_city_all_dist_complete <- tcon |>
  tbl("city_all_greenspace_dist_complete") |>
  select(cell_id, distance_to_greenspace_meters, nearest_greenspace_acres) |>
  collect()

# Join your results back to preserve exact cell alignment

city_all_aligned_results <- tibble(cells = empty.sr |> cells()) |>
  left_join(sf_city_all_dist_complete |> rename(cells = cell_id)) |>
  select(cells, distance_to_greenspace_meters, nearest_greenspace_acres)

# Convert directly to raster values in the right order
city_all_greenspace_nearest_dist <- empty.sr
values(city_all_greenspace_nearest_dist) <- city_all_aligned_results$distance_to_greenspace_meters
names(city_all_greenspace_nearest_dist) <- "city_all_greenspace_nearest_dist"

ggplot() +
  tidyterra::geom_spatraster(data = city_all_greenspace_nearest_dist)

city_all_greenspace_nearest_dist |> writeRaster("data/predictors/city_all_greenspace_nearest_dist.tif", overwrite = T)

city_all_greenspace_nearest_size <- empty.sr
values(city_all_greenspace_nearest_size) <- city_all_aligned_results$nearest_greenspace_acres
names(city_all_greenspace_nearest_size) <- "city_all_greenspace_nearest_size"

city_all_greenspace_nearest_size |> writeRaster("data/predictors/city_all_greenspace_nearest_size.tif", overwrite = T)

ggplot() +
  tidyterra::geom_spatraster(data = city_all_greenspace_nearest_size)





# City-only non-essential greenspace ---------------------------------------
#' Calculate distances to non-essential greenspaces in the city
#' This follows the same pattern as essential and all greenspaces but filters for non-essential ones

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


nncitynoness_query <- glue("
CREATE OR REPLACE TABLE city_nonessential_greenspace_dist_complete AS
WITH green AS (SELECT * FROM city_nonessential_greenspace WHERE AREA_acres >= 1),
     distances AS (
       SELECT
         template.cell_id,
         template.geom AS template_geom,
         ST_AsText(template.centroid_geom) AS geom_wkt,
         green.AREA_acres,
         ST_Distance(template.centroid_geom3310, green.simple_geom3310) AS distance_meters
       FROM template_grid_cities AS template, green
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

tic()
tcon |> dbExecute(nncitynoness_query)
toc()

slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])

sf_city_noness_dist_complete <- tcon |>
  tbl("city_nonessential_greenspace_dist_complete") |>
  select(cell_id, distance_to_greenspace_meters, nearest_greenspace_acres) |>
  collect()

# Join your results back to preserve exact cell alignment
city_noness_aligned_results <- tibble(cells = empty.sr |> cells()) |>
  left_join(sf_city_noness_dist_complete |> rename(cells = cell_id)) |>
  select(cells, distance_to_greenspace_meters, nearest_greenspace_acres)

# Convert directly to raster values in the right order
city_noness_greenspace_nearest_dist <- empty.sr
values(city_noness_greenspace_nearest_dist) <- city_noness_aligned_results$distance_to_greenspace_meters
names(city_noness_greenspace_nearest_dist) <- "city_nonessential_greenspace_nearest_dist"

ggplot() +
  tidyterra::geom_spatraster(data = city_noness_greenspace_nearest_dist)

city_noness_greenspace_nearest_dist |> writeRaster("data/predictors/city_nonessential_greenspace_nearest_dist.tif", overwrite = T)

city_noness_greenspace_nearest_size <- empty.sr
values(city_noness_greenspace_nearest_size) <- city_noness_aligned_results$nearest_greenspace_acres
names(city_noness_greenspace_nearest_size) <- "city_nonessential_greenspace_nearest_size"

city_noness_greenspace_nearest_size |> writeRaster("data/predictors/city_nonessential_greenspace_nearest_size.tif", overwrite = T)

ggplot() +
  tidyterra::geom_spatraster(data = city_noness_greenspace_nearest_size)
