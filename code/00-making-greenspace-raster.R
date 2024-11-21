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

## Test to see if I can reduce vertices with simplify
# tcon |> dbGetQuery("
# SELECT DISTINCT ST_NPoints(geom)
# FROM greenspace_geo
# ORDER BY ST_NPoints(geom) DESC
# LIMIT 10
# ")

# tcon |> dbGetQuery("
# WITH green AS
# (SELECT ST_SIMPLIFY(geom, .05) AS geom
# FROM greenspace_geo)

# SELECT DISTINCT ST_NPoints(geom)
# FROM green
# ORDER BY ST_NPoints(geom) DESC
# LIMIT 10")
# Can reduce geom vertices significantly

# # Add buffer column
# tcon |> dbExecute("
#     ALTER TABLE greenspace_geo
#     ADD COLUMN buffer100km_geom GEOMETRY
# ")

# tcon |> dbExecute("
#     UPDATE greenspace_geo
#     SET buffer100km_geom = ST_SIMPLIFY(ST_Buffer(geom, 1), .1)
# ")


# tcon |> dbExecute("
#     CREATE INDEX idx_grn_buffer100_geom ON greenspace_geo USING RTREE (buffer100km_geom);
# ")

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
WHERE AREA_acres >= 30")

# x <- tcon |>
#   dbGetQuery("
# SELECT ST_AsTEXT(simple_geom3310) AS geom_text
# FROM greenspace_geo
# WHERE AREA_acres >= 75;") |>
#   tibble() |>
#   st_as_sf(wkt = "geom_text")

plot(x)
View(x)

tcon |>
  tbl("greenspace_geo") |>
  filter(AREA_acres >= 75)

tcon |> tbl("greenspace_geo")

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
# I don't see an index scan. I don't think it's using index.
# That's probably because it is in 3310 instead of 4326
tcon |> dbExecute("
SET memory_limit = '300GB';
SET preserve_insertion_order = true;
SET threads TO 20;
")

# testing
tic()
tcon |> dbExecute(sprintf(paste("CREATE OR REPLACE TABLE grn_distance_grd_%s AS", nn_query), "130", "130"))
toc()
# 10 without simplified geometry: 32 sec
# 10 with simplified geometry: 28 sec
tcon |>
  tbl("grn_distance_grd_130_test") |>
  # colnames()
  distinct(distance_to_greenspace_meters) |>
  arrange(distance_to_greenspace_meters)


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

# 2
tic()
tcon |> dbExecute(glue(sprintf(paste("CREATE OR REPLACE TABLE grn_distance_grd_%s AS", nn_query), "2", "2")))
toc()
#  6.1 hours

tcon |> dbGetQuery("
WITH tab AS
(SELECT g2.geom_wkt AS d2,
       g130.geom_wkt AS d130
FROM
  grn_distance_grd_2 AS g2
LEFT JOIN
  grn_distance_grd_130 AS g130
ON g130.template_id = g2.template_id)

SELECT COUNT(*)
FROM tab
WHERE d2 != d130
")
tcon |> dbGetQuery("DESCRIBE grn_distance_grd_130")
# Hmm the values are the same. What's going wrong.
tcon |>
  tbl("grn_distance_grd_130") |>
  select(template_id, d130 = distance_to_greenspace_meters) |>
  left_join(
    tcon |> tbl("grn_distance_grd_30") |>
      select(template_id, d30 = distance_to_greenspace_meters),
    by = "template_id"
  ) |>
  filter(d30 != d130) |>
  collect()

identical(sf_dist30$distance_to_greenspace_meters, sf_dist130$distance_to_greenspace_meters)
identical(greenspace_dist130, greenspace_dist30)
x <- rast("data/predictors/130acre_greenspace_distance.tif")
y <- rast("data/predictors/30acre_greenspace_distance.tif")
hist(greenspace_dist130)
identical(values(greenspace_dist75), values(greenspace_dist30))

x <- tcon |>
  tbl("grn_distance_grd_130_test") |>
  filter(distance_to_greenspace_meters < 2000) |>
  collect() |>
  st_as_sf(wkt = "geom_wkt")



slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])
tcon |> dbListTables()
y <- tcon |>
  tbl("grn_distance_grd_130") |>
  collect() |>
  st_as_sf(wkt = "geom_wkt")

greenspace_dist <- y |> rasterize(empty.sr, field = "distance_to_greenspace_meters")

plot(greenspace_dist)

x |>
  sample_n(10000) |>
  ggplot() +
  geom_sf(aes(color = distance_to_greenspace_meters))

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







# Crawling back to postgis ---------------------------------------
# Trying double run
# Need this, the spatial index on grid is critical
# IN METERS
library(RPostgreSQL)
pcon <- dbConnect(RPostgreSQL::PostgreSQL(),
  host = "flor",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

## Read in greenspace  ---------------------------------------
grn <- st_read("data/predictor_prep/nlcd_greenspace_area.gpkg") |>
  st_transform(3310)

grn |>
  rename_with(tolower) |>
  st_write(pcon, layer = "nlcd_greenspace_area")

tic()
pcon %>% dbExecute("
DROP TABLE IF EXISTS tempgreen;
DROP TABLE IF EXISTS greenspace130_dist;

CREATE TABLE tempgreen AS
SELECT area_acres, nlcd_greenspace_area.geom
  FROM nlcd_greenspace_area
  --JOIN bay_area_counties
  --ON ST_Intersects(nlcd_greenspace_area.geom, bay_area_counties.geom)
  WHERE AREA_acres >= 130
  LIMIT 1;

CREATE INDEX idx_tempgrn_geom ON tempgreen USING gist (geom);

-- Then do a nearest neighbor cross lateral join on grid cells and streams
CREATE TABLE greenspace130_dist AS
SELECT gr.geom AS geom, greens.geom AS sgeom, dist
FROM cub_bay_grid AS gr
CROSS JOIN LATERAL (
  SELECT green.geom <-> gr.geom AS dist,
	gr.geom
  FROM tempgreen AS green
  ORDER BY dist
  LIMIT 1
) greens;
")
tic()
# 40 minutes

streamdist.sf <- pcon %>% st_read(query = "
SELECT * FROM greenspace130_dist
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
