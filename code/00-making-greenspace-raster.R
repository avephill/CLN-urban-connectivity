#' This file makes greenspace files from NLCD data and then creates 30m rasters
#' that show the minimum distance to varying sizes of greenspaces
#' using duckdb
#' duckdb seems to be much faster and more intuitive than postgis or terra for
#' this purpose. From about 10 minutes to a few hours.
#' Much faster than the days the other methods took

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
  # st_transform(crs = 4326) |>
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
library(stars)
library(duckdb)
library(tidyterra)
library(glue)
library(tictoc)
library(duckdbfs)
library(dplyr)
library(fasterize)

tcon <- dbConnect(duckdb("data/greenspaces/temp.duckdb"))
tcon |> dbExecute("install spatial; load spatial")

tcon |> dbListTables()
tcon |> dbGetQuery("DESCRIBE greenspace_geo")

tcon |> dbDisconnect(shutdown = T)

## Read data to duckdb  ---------------------------------------

### Counties  ---------------------------------------
# Need to add counties so I properly filter out greenspaces that I don't care about

counties <- st_read("~/Data/Boundaries/Political/CA_Counties/CA_Counties_TIGER2016.shp") |>
  rename_with(tolower) |>
  filter(name %in% c("Solano", "Contra Costa", "Alameda", "Santa Clara", "San Mateo", "San Francisco", "Marin", "Sonoma", "Napa", "Santa Cruz")) |>
  st_transform(4326) |>
  select(name) |>
  st_union(is_coverage = T) |>
  st_buffer(.1) |>
  st_simplify(dTolerance = 5000) |>
  st_as_sf() |>
  rename(geom = x) |>
  mutate(geom_wkt = st_as_text(geom, EWKT = T))

plot(counties)

tcon |> copy_to(
  counties,
  name = "baycounties",
  overwrite = T
)

tcon |> tbl("baycounties")

### Greenspace  ---------------------------------------
# Fix geometry and tables
tcon |> dbExecute("
CREATE OR REPLACE TABLE greenspace_geo
AS
SELECT * EXCLUDE geom,
  grn.geom,
  ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true) AS geom3310,
  ST_TRANSFORM(ST_Centroid(grn.geom), 'EPSG:4326', 'EPSG:3310', always_xy := true) AS centroid_geom3310,
  ST_Centroid(grn.geom) AS centroid_geom,
  ST_SimplifyPreserveTopology(ST_TRANSFORM(grn.geom, 'EPSG:4326', 'EPSG:3310', always_xy := true), 5000) AS simple_geom3310
FROM ST_READ('data/predictor_prep/nlcd_greenspace_area.gpkg') AS grn
JOIN baycounties
  ON ST_Intersects(grn.geom, ST_GeomFromText(baycounties.geom_wkt));

CREATE INDEX idx_grn_geom ON greenspace_geo USING RTREE (geom);
CREATE INDEX idx_grn_centroid_geom ON greenspace_geo USING RTREE (centroid_geom);
CREATE INDEX idx_grn_simple_geom ON greenspace_geo USING RTREE (simple_geom3310);
CREATE INDEX idx_grn_cent3310_geom ON greenspace_geo USING RTREE (centroid_geom3310);
")
# ST_SimplifyPreserveTopology was crucial

# Read template raster and convert to polygons
# This takes a while
cub_grid.sf <- read_stars("data/predictors/slope.tif") %>%
  # So that we get meters in distance later on
  # st_transform(3310) %>%
  st_as_sf() # %>%
# mutate(geom_wkt = st_as_text(geometry))

cub_grid.sf |> write_sf("data/predictor_prep/template.gpkg")
cub_grid.sf |> head(10)

# Fix geometry and tables
# always_xy := true is necessary in st_transform
# see https://github.com/duckdb/duckdb_spatial/issues/211
tcon |> dbExecute("
CREATE OR REPLACE TABLE template_grid_geo
AS
SELECT row_number() OVER () AS id,
  geom,
  ST_TRANSFORM(geom, 'EPSG:4326', 'EPSG:3310', always_xy := true) AS geom3310,
  ST_Centroid(geom) AS centroid_geom,
  ST_TRANSFORM(ST_Centroid(geom), 'EPSG:4326', 'EPSG:3310', always_xy := true) AS centroid_geom3310
FROM ST_Read('data/predictor_prep/template.gpkg');

CREATE INDEX idx_template_geom ON template_grid_geo USING RTREE (geom);
CREATE INDEX idx_tmp_centroid_geom ON template_grid_geo USING RTREE (centroid_geom);
")


## Nearest neighbor join ---------------------------------------

nn_query <- glue("
WITH green AS
(SELECT * FROM greenspace_geo
WHERE AREA_acres >= %s)

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
    field = "distance_to_greenspace_meters",
    fun = "min"
  )
plot(greenspace_dist130)
greenspace_dist130 |> writeRaster("data/predictors/130acre_greenspace_distance.tif", overwrite = T)


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
plot(greenspace_dist30)
greenspace_dist30 |> writeRaster("data/predictors/30acre_greenspace_distance.tif", overwrite = T)

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
plot(greenspace_dist2)
greenspace_dist2 |> writeRaster("data/predictors/2acre_greenspace_distance.tif", overwrite = T)
