library(terra)
library(tidyverse)
library(units)
library(stars)

setwd("~/Projects/together-bay-area/together-bay-area/")


slope.sr <- rast("../data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])
empty.strs <- empty.sr %>%
  st_as_stars() %>%
  rename(value = prcnt_slope30)

# Mask
background_mask <-
  st_read("../data/study_area.gpkg")

# So that the edges of study area also include distance to ponds outside study
background_mask_buffed <-
  background_mask %>%
  st_buffer(10000) %>%
  st_simplify(dTolerance = 1000)

land.sf <- st_read("~/Data/Boundaries/Political/CLN2.0/CLN_Database_2_0_1/data/Base/vector/terrestrial.shp")




# First make coastline ----------------------------------------------------

coast.sf <- st_read("~/Data/Boundaries/Natural/CA_coast/ds990.gdb/", layer = "ds990")
pacific_pt.sf <-
  st_as_sf(tibble(LAT = 34.987, LON = -128.401),
    coords = c("LON", "LAT"),
    crs = 4326
  )

# Make convex hull of all points
coast_triangle.sf <- coast.sf %>%
  rename(geometry = Shape) %>%
  dplyr::select(geometry) %>%
  rbind(pacific_pt.sf %>% st_transform(crs = 3310)) %>%
  st_union() %>%
  st_cast("POINT") %>%
  st_union() %>%
  st_convex_hull()

# Split the convex hull in half with coast shape, then look at it to
# see which polygon we want to keep (the western most one)
merge_mess.sf <- coast_triangle.sf %>%
  st_difference(coast.sf) %>%
  st_sf() %>%
  st_cast("POLYGON") %>%
  mutate(
    ID = 1:n() %>% as.factor(),
    AREA = st_area(geometry)
  )


# ggplot(merge_mess.sf) +
#  geom_sf(aes(color = ID), fill = NA, linewidth = 3)
# Yep, number 8
big_coast.sf <- merge_mess.sf %>% filter(ID == 8)



# Then add coastline to bay water -----------------------------------------


# Background window
background.sf <-
  land.sf %>%
  st_bbox() %>%
  st_as_sfc()

add_counties.sf <- st_read("~/Data/Boundaries/Political/CA_Counties/CA_Counties_TIGER2016.shp") %>%
  filter(NAME %in% c(
    "Lake", "Mendocino", "San Joaquin", "Stanislaus",
    "Merced", "Sacramento", "Yolo", "San Benito",
    "Monterey", "Sutter", "Placer", "Colusa"
  )) %>%
  st_transform(3310) %>%
  st_buffer(400)

not_land.sf <-
  # add inland counties to study area
  (land.sf %>%
    st_union(add_counties.sf %>%
      st_transform(st_crs(land.sf)) %>%
      st_union())) %>%
  # crop it to study area window
  st_intersection(background.sf %>%
    st_transform(st_crs(land.sf))) %>%
  st_geometry() %>%
  # find difference between background window and all land
  st_difference(background.sf %>%
    st_transform(st_crs(land.sf)), .) %>%
  # union it to rest of coast
  st_union(big_coast.sf %>%
    st_transform(st_crs(land.sf)) %>%
    st_union())

back_buffed_final.sf <-
  background_mask_buffed %>%
  # Take difference between land and not land
  st_difference(not_land.sf %>%
    st_transform(st_crs(background_mask_buffed))) %>%
  # Cast into polygons and only take the biggest polygon (contiguous land)
  st_cast("POLYGON") %>%
  mutate(AREA = st_area(geom)) %>%
  slice_max(AREA)



# green space larger than 10 acres
gs_large.sf <- st_read("../data/predictor_prep/nlcd_greenspace10acre.gpkg")



# ggplot() +
#  geom_sf(data = back_buffed_final.sf) +
#  geom_sf(data = gs_large.sf )



# Rasterize greenspaces and assign value as .001
gs.strs <- gs_large.sf %>%
  mutate(DIST = .001) %>%
  dplyr::select(DIST) %>%
  st_transform(crs = 4326) %>%
  st_rasterize(empty.strs %>% mutate(value = 0))


# Okay we're going to make a map of saltwater and tell the distance()
# function not to touch saltwater

# We're also going to sum not_land.sf

# Burn not_land as -1 into the raster
gs_prep.strs <- st_rasterize(
  not_land.sf %>% st_sf() %>%
    mutate(DIST = -1) %>%
    st_transform(crs = 4326),
  gs.strs
)


gs_prep.sr <- rast(gs_prep.strs)

names(gs_prep.sr) <- "DIST"

# Ocean or Out of Bounds = -1
# Land = NA
# Waterr = 0
gs_class.sr <- classify(gs_prep.sr, rbind(c(0, NA), c(.001, 0)))

# Exclude NAs outside of study area to further speed up process

# First make raster of outside study area values set to -1
outside_study_area.sv <-
  (gs_class.sr %>% ext() %>% as.polygons()) - vect(background_mask)
outside_study_area.sv$DIST <- -1
outside_study_area.sr <- outside_study_area.sv %>%
  rasterize(gs_class.sr, field = "DIST")

# cover is the key. Only updates NAs of gs_class.sr with -1s from outside_study_area.sv
gs_class_studyarea.sr <- gs_class.sr %>% cover(outside_study_area.sr)


# We were getting a segfault until I write the raster to a .tif and then read it back
writeRaster(gs_class_studyarea.sr,
  "../data/predictor_prep/greenspace_classified.tif",
  overwrite = T
)

gs_class_studyarea.sr <- rast("../data/predictor_prep/greenspace_classified.tif")
gs10_distance.sr <- distance(gs_class_studyarea.sr,
  target = NA, exclude = -1
)
gs10_distance_masked.sr <- gs10_distance.sr # %>% mask(background_landmask.sf)
names(gs10_distance_masked.sr) <- "distance_to_greenspace"
writeRaster(gs10_distance_masked.sr,
  "../data/predictors/greenspace10acre_distance.tif",
  overwrite = T
)

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

# makeAcreMinGreenspace <- function(.gs_area.sf, acre_min) {
#   gs_large.sf <- .gs_area.sf %>%
#     # Greater than or equal to 10 acres
#     filter(AREA >= acre_min %>%
#       set_units("acre") %>%
#       set_units("m2"))

#   write_sf(gs_large.sf, paste0("data/predictor_prep/nlcd_greenspace", acre_min, "acre.gpkg"))
# }

# gs_area.sf |> makeAcreMinGreenspace(1)
# gs_area.sf |> makeAcreMinGreenspace(5)
# gs_area.sf |> makeAcreMinGreenspace(10)
# gs_area.sf |> makeAcreMinGreenspace(15)
# gs_area.sf |> makeAcreMinGreenspace(30)
# gs_area.sf |> makeAcreMinGreenspace(100)

# acre15 <- st_read("data/predictor_prep/nlcd_greenspace100acre.gpkg")

# acre15 |> ggplot() +
#   geom_sf(aes(fill = NLCD.Land.Cover.Class))

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

tcon <- dbConnect(duckdb("data/greenspaces/temp.duckdb"))
tcon |> dbExecute("install spatial; load spatial")

tcon |> dbListTables()
tcon |> dbGetQuery("DESCRIBE greenspace_geo")

tcon |> dbDisconnect(shutdown = T)

## Read data to duckdb  ---------------------------------------
# This came from 00-data prep.R
# cln_grn <- st_read("data/greenspaces/christine_greenspace.gpkg")
# nlcd_grn <- st_read("data/predictor_prep/nlcd_greenspace_area.gpkg")
# Okay this is done we can read it in

# plot(cln_grn |> st_geometry())

# tcon |> copy_to(nlcd_grn, name = "greenspace", overwrite = T)

# Fix geometry and tables
tcon |> dbExecute("
CREATE OR REPLACE TABLE greenspace_geo
AS
SELECT * EXCLUDE geom, geom, ST_Centroid(geom) AS centroid_geom
FROM ST_READ('data/predictor_prep/nlcd_greenspace_area.gpkg');

CREATE INDEX idx_grn_geom ON greenspace_geo USING RTREE (geom);
CREATE INDEX idx_grn_centroid_geom ON greenspace_geo USING RTREE (centroid_geom);
")

# Add buffer column
tcon |> dbExecute("
    ALTER TABLE greenspace_geo
    ADD COLUMN buffer100km2_geom GEOMETRY
")

tcon |> dbExecute("
    UPDATE greenspace_geo
    SET buffer100km2_geom = ST_SIMPLIFY(ST_Buffer(geom, 1), .25)
")

tcon |> dbExecute("
    CREATE INDEX idx_grn_buffer1002_geom ON greenspace_geo USING RTREE (buffer100km2_geom);
")

# tcon |> dbExecute("DROP INDEX temp_db.idx_grn_buff_geom;")
# tcon |> dbExecute("
# DROP INDEX temp_db.idx_grn_buff_geom;
#     ALTER TABLE greenspace_geo
#     DROP COLUMN buff_geom;
# ")

tcon |> dbGetQuery("DESCRIBE greenspace_geo")

# tcon |> dbExecute("CREATE INDEX idx_grn_centroid_geom ON greenspace_geo USING RTREE (centroid_geom);")


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
tcon |> dbExecute("
CREATE OR REPLACE TABLE template_grid_geo
AS
SELECT row_number() OVER () AS id, geom, ST_Centroid(geom) AS centroid_geom
FROM ST_Read('data/predictor_prep/template.gpkg');
")

tcon |> dbExecute("
CREATE INDEX idx_template_geom ON template_grid_geo USING RTREE (geom);
CREATE INDEX idx_tmp_centroid_geom ON template_grid_geo USING RTREE (centroid_geom);
")

tcon |> dbGetQuery("DESCRIBE template_grid_geo")
tcon |> dbGetQuery("DESCRIBE greenspace_geo")
tcon |> tbl("greenspace_geo")
tcon |> dbGetQuery("select * from duckdb_indexes;")

## Fix geoms add index ---------------------------------------

# NOTE We don't have greenspace info for outside of bay area so we'll have to leave this i guess?
# Make bay area county VIEW
# Need to buffer it to include distance to streams outside the study area
# con %>% dbExecute("
# DROP MATERIALIZED VIEW IF EXISTS bay_area_counties;

# CREATE MATERIALIZED VIEW bay_area_counties AS
# SELECT ST_SUBDIVIDE(ST_BUFFER(ST_UNION(geom), .1)) AS geom
# FROM ca_layers
# WHERE name IN ('Solano', 'Contra Costa', 'Alameda', 'Santa Clara', 'San Mateo', 'San Francisco', 'Marin', 'Sonoma', 'Napa', 'Santa Cruz')
# ;

# CREATE INDEX idx_tempbay_geom ON bay_area_counties USING gist (geom);
# ")

## Nearest neighbor join ---------------------------------------
tcon |> dbListTables()

tcon |> dbGetQuery("
SELECT * FROM greenspace_geo
WHERE AREA_acres >= 50
LIMIT 1")

tcon |> dbGetQuery("
SELECT COUNT(*) FROM greenspace_geo
WHERE AREA_acres >= 130")

# Re-run this, I didn't use buffer_geom
# nn_query <- glue("
# WITH green AS
# (SELECT * FROM greenspace_geo
# WHERE AREA_acres >= %s
# LIMIT 10)

# SELECT
#     template.id AS template_id,
#     template.geom AS template_geom,
#     -- ST_AsText(template.centroid_geom) AS geom_wkt,
#     MIN(ST_Distance(template.centroid_geom, green.geom)) AS distance_to_greenspace
# FROM
#     template_grid_geo AS template, green
#     -- WHERE ST_DWithin(template.centroid_geom, green.centroid_geom, .1)
#     WHERE ST_Intersects(template.centroid_geom, green.buffer100km_geom)
# GROUP BY
#     template.id, template.geom, template.centroid_geom;
# ")

# Expiremental
nn_query <- glue("
WITH green AS
(SELECT * FROM greenspace_geo
WHERE AREA_acres >= %s
LIMIT 5)

SELECT
    template.id AS template_id,
    template.geom AS template_geom,
    ST_AsText(template.centroid_geom) AS geom_wkt,
    dist AS distance_to_greenspace
FROM template_grid_geo AS template
CROSS JOIN LATERAL (
  SELECT ST_Distance(template.centroid_geom, green.geom) AS dist,
  template.geom
  FROM green
  WHERE ST_Intersects(template.centroid_geom, green.buffer100km2_geom)
  -- WHERE ST_DWithin(template.centroid_geom, green.centroid_geom, .1)
  ORDER BY dist
  LIMIT 1
) greenie;
")



# tcon |> dbGetQuery(paste("EXPLAIN", nn_query))
tcon |> dbGetQuery(sprintf(paste("EXPLAIN", nn_query), "130"))
# I don't see an index scan. I don't think it's using index.
# That's probably because it is in 3310 instead of 4326
tcon |> dbExecute("
SET memory_limit = '300GB';
SET preserve_insertion_order = false;
")
tic()
tcon |> dbExecute(sprintf(paste("CREATE OR REPLACE TABLE grn_distance_grd_%s100km2 AS", nn_query), "130", "130"))
toc()

tic()
tcon |> dbExecute(sprintf(paste("CREATE OR REPLACE TABLE grn_distance_grd_%s AS", nn_query), "130", "130"))
toc()
# 634.365 sec


x <- tcon |>
  tbl("grn_distance_grd_130") |>
  filter(distance_to_greenspace < .005) |>
  collect() |>
  st_as_sf(wkt = "geom_wkt")

slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])
tcon |> dbListTables()
y <- tcon |>
  tbl("grn_distance_grd_130100km2") |>
  collect() |>
  st_as_sf(wkt = "geom_wkt")

greenspace_dist <- y |> rasterize(empty.sr, field = "distance_to_greenspace")

plot(greenspace_dist)

ggplot(x) +
  geom_sf(aes(color = distance_to_greenspace))

tcon |> db("
EXPLAIN SELECT
    template.id AS template_id,
    template.geom AS template_geom,
    MIN(ST_Distance(template.geom, green.geom)) AS distance_to_greenspace
FROM
    template_grid_geo AS template
JOIN
    greenspace_geo AS green
ON
    ST_DWithin(template.geom, green.geom, 400)  -- adjust the distance threshold as needed
GROUP BY
    template.id, template.geom;
")

# Trying double run
# Need this, the spatial index on grid is critical
# IN METERS
system.time({
  con %>% dbExecute("
-- DROP TABLE IF EXISTS tempgrid;
DROP TABLE IF EXISTS tempstreams;
DROP TABLE IF EXISTS bay_area_stream_dist;

SELECT label, ST_TRANSFORM(ST_CURVETOLINE(cari_streams_raw.geom),3310) AS geom
INTO tempstreams
  FROM cari_streams_raw
  JOIN bay_area_counties
  ON ST_Intersects(cari_streams_raw.geom, bay_area_counties.geom)
  AND label = 'Fluvial Natural';

CREATE INDEX idx_tempstrms_geom ON tempstreams USING gist (geom);

-- Then do a nearest neighbor cross lateral join on grid cells and streams
SELECT gr.geom AS ggeom, streams.geom AS sgeom, dist
INTO TABLE bay_area_stream_dist
FROM cub_bay_grid AS gr
CROSS JOIN LATERAL (
  SELECT streams.geom <-> gr.geom AS dist,
	gr.geom
  FROM tempstreams AS streams
  ORDER BY dist
  LIMIT 1
) streams;

-- Clean up
-- DROP TABLE IF EXISTS tempgrid;
DROP TABLE IF EXISTS tempstreams;
")
})
# 40 minutes

streamdist.sf <- con %>% st_read(query = "
SELECT * FROM bay_area_stream_dist
")


# Now need to convert back to raster
cub_grid.strs <- read_stars("data/predictors/slope.tif")

streamdist.strs <- streamdist.sf %>%
  # It's important to be in 4326 for rasterization
  st_transform(4326) %>%
  select(dist) %>%
  st_rasterize(template = cub_grid.strs)

# Write it
streamdist.strs %>%
  rename(distance.to.natural.stream = dist) %>%
  write_stars("./data/predictors/natural-stream-dist.tif")

library(duckdb)
lll <- dbConnect(duckdb())
lll |> dbExecute("install spatial; load spatial")
lll |> dbExecute("
CREATE TABLE hmm AS
SELECT
        *
    FROM
        ST_Read('data/predictor_prep/nlcd_greenspace_area.gpkg')")
