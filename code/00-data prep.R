#  ---------------------------------------
# Species occurrence data ---------------------------------------
#  ---------------------------------------


library(sf)
library(tidyverse)
# library(rgbif)
# library(terra)
# library(stars)
library(RPostgreSQL)
# library(starsExtra)
library(terra)
library(duckdb)
library(dbplyr)

terrestrial.sf <- st_read("~/Data/Boundaries/Political/CLN2.0/CLN_Database_2_0_1/data/Base/vector/terrestrial.shp")

# sf_use_s2(T)
target_counties <- c(
  "Solano", "Contra Costa", "Alameda", "Santa Clara", "San Mateo",
  "San Francisco", "Marin", "Sonoma", "Napa", "Santa Cruz"
)
counties.sf <- st_read("~/Data/Boundaries/Political/CA_Counties/CA_Counties_TIGER2016.shp") |>
  filter(NAME %in% target_counties) |>
  st_transform(crs = 4326)
target_counties.sf <-
  counties.sf |>
  st_union() # Convert to WGS 84

# # Simplify input polygons and add some buffer zone to it
target_simple.sf <-
  target_counties.sf |>
  st_union(is_coverage = T) |>
  st_buffer(4000) |> # 1km buffer
  st_simplify(dTolerance = 4000) |> # reduce
  st_cast("POLYGON")

# write_sf(target_simple.sf[1], "../data/study_area.gpkg") # just write mainland


# Convert to a well known text format of the polygon and check validity
target.ewkt <-
  target_simple.sf |>
  st_union() |>
  st_as_text(EWKT = T)

# Now pull from local GBIF database

con <- dbConnect(duckdb(dbdir = "~/Data/Occurrences/GBIF/gbif.duckdb", read_only = T))
con |> dbExecute("install spatial; load spatial")

species_names <- c("Taricha torosa", "Callipepla californica", "Lynx rufus", "Pituophis catenifer", "Lepus californicus", "Microtus californicus")

target_spec.df <- con |>
  tbl("gbif") |>
  filter(species %in% species_names) |>
  inner_join(con |> tbl("join_lookup") |> filter(county_name %in% target_counties)) |>
  collect()

target_spec.sf <- target_spec.df |>
  select(-geom) |>
  mutate(across(where(is.list), ~ map_chr(., toString))) |>
  st_as_sf(
    coords = c("decimallongitude", "decimallatitude"),
    remove = F,
    crs = 4326,
    sf_column_name = "geom"
  )

target_spec.sf |>
  filter(species %in% c("Microtus californicus", "Lepus californicus")) |>
  # sample_n(10000) |>
  ggplot() +
  geom_sf(aes(color = species))



# con <- dbConnect(RPostgreSQL::PostgreSQL(),
#   host = "flor",
#   port = 5432,
#   user = "postgres",
#   password = "postgres"
# )

# bigquery <- paste0("
# SELECT * FROM ca_core
# INNER JOIN ca_species ON ca_species.taxonKey=ca_core.taxonKey
# INNER JOIN ca_extra ON ca_extra.gbifid=ca_core.gbifid
# WHERE ST_Intersects(ca_core.geom,'", target.ewkt, "')
# AND ca_species.taxonrank='SPECIES'
# AND ca_species.species IN ('", paste(species_names, collapse = "','"), "')")

# system.time({
#   target_spec.sf <- st_read(con, query = bigquery)
# })
# target_spec.sf |> select(where(is.list))

write_sf(target_spec.sf,
  paste0(
    "data/occurrence/",
    format(Sys.time(), "%Y-%m-%d"),
    "_target_spec.gpkg"
  ),
  layer = "spec_occ"
)


# testing
target_spec.sf <- st_read("data/occurrence/2025-04-02_target_spec.gpkg")
cities <- st_read("data/city_boundaries.gpkg")

city_name <- "San Jose"
speci <- "Lepus californicus"
city_check <- target_spec.sf |>
  filter(species %in% c(speci)) |>
  st_intersection(cities |> filter(jurname == city_name))


city_check |>
  ggplot() +
  geom_sf(data = cities |> filter(jurname == city_name)) +
  geom_sf(data = target_spec.sf |>
    filter(species %in% c(speci)) |>
    st_intersection(cities |> filter(jurname == city_name)), aes(color = species))
city_check |> count(species)

#  ---------------------------------------
# City Boundaries ---------------------------------------
#  ---------------------------------------

# City boundaries
city_boundaries_prep <- st_read("data/BayAreaCities_CLN.gpkg")

city_boundaries_sf <- city_boundaries_prep |>
  filter(jurname == "San Francisco") |>
  st_cast("POLYGON") |>
  mutate(new_area = st_area(geom)) |>
  slice_max(order_by = new_area) %>%
  st_crop(
    st_bbox(.) %>%
      (function(bb) {
        bb["ymax"] <- 37.815
        bb["xmin"] <- -122.53
        bb["xmax"] <- -122.37
        st_bbox(bb, crs = st_crs(.))
      })
  )

city_boundaries_oak <- city_boundaries_prep |>
  filter(jurname %in% c("Oakland", "Piedmont")) |>
  summarise(jurname = "Oakland_Piedmont", geom = st_union(geom))

city_boundaries <-
  bind_rows(
    city_boundaries_prep |>
      filter(jurname %in% c("San Jose")),
    city_boundaries_sf,
    city_boundaries_oak
  )

city_boundaries |> write_sf("data/city_boundaries.gpkg")

#  ---------------------------------------
# Environmental data ---------------------------------------
#  ---------------------------------------
library(terra)
library(sf)
# Most was borrowed from the CLN CUB project. Here are some additions.

# Use slope as the default raster
slope.sr <- rast("data/predictors/slope.tif")
empty.sr <- rast(slope.sr[[1]])


# NLCD Impervious surfaces ---------------------------------------
nlcd_imp_pre <- rast("~/Data/Environment/NLCD/NLCD_2019_ImperviousSurfaces/nlcd_2019_impervious_l48_20210604.img")

nlcd_imp.sr <- nlcd_imp_pre |>
  project(empty.sr) |>
  crop(empty.sr) |>
  app(fun = as.numeric)

names(nlcd_imp.sr) <- "Impervious_surfaces"
varnames(nlcd_imp.sr) <- "Impervious_surfaces"

# Does it stack?
# c(nlcd_imp.sr, empty.sr)
# plot(nlcd_imp)

writeRaster(nlcd_imp.sr, "data/predictors/nlcd_impervious.tif", overwrite = T)


# Christine's NLCD Land Cover Class ---------------------------------------
# Seems to be the same as the original NLCD Land Cover Data
nlcd_chr_pre <- rast("~/Data/Environment/NLCD/NLCD_ChristineCustom/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")

nlcd_chr_pre |> unique()
nlcd_chr_pre |> cats()

# nlcd_chr2_pre <- rast("~/Data/Environment/NLCD/NLCD_ChristineCustom/nlcd_2001_2019_change_index_l48_20210604/nlcd_2001_2019_change_index_l48_20210604.img")


# plot(nlcd_chr_pre)

nlcd <- rast("data/predictors/nlcd_landcover.tif")

# Existing categories
cats(nlcd)[[1]] |>
  select(`NLCD Land Cover Class`) |>
  filter(`NLCD Land Cover Class` != "")

# Reclassify according to Christine's bins
new_cats <- c(
  "no data" = 0,
  "undeveloped land cover classes" = 1,
  "developed: open space" = 2,
  "developed: low intensity" = 3,
  "developed: med intensity" = 4,
  "developed: high intensity" = 5
) |> enframe(name = "new_category", value = "new_value")

old_cats <- levels(nlcd)[[1]]

# Define the mapping of old categories to new categories
category_mapping <- c(
  "Unclassified" = NA,
  "Open Water" = NA, # "undeveloped land cover classes",
  "Perennial Snow/Ice" = "undeveloped land cover classes",
  "Developed, Open Space" = "developed: open space",
  "Developed, Low Intensity" = "developed: low intensity",
  "Developed, Medium Intensity" = "developed: med intensity",
  "Developed, High Intensity" = "developed: high intensity",
  "Barren Land" = "undeveloped land cover classes",
  "Deciduous Forest" = "undeveloped land cover classes",
  "Evergreen Forest" = "undeveloped land cover classes",
  "Mixed Forest" = "undeveloped land cover classes",
  "Shrub/Scrub" = "undeveloped land cover classes",
  "Herbaceous" = "undeveloped land cover classes",
  "Hay/Pasture" = "developed: open space",
  "Cultivated Crops" = "developed: open space",
  "Woody Wetlands" = "undeveloped land cover classes",
  "Emergent Herbaceous Wetlands" = "undeveloped land cover classes"
) |> enframe(name = "NLCD Land Cover Class", value = "new_category")

reclass_df <- old_cats |>
  left_join(category_mapping) |>
  left_join(new_cats)

# Assign new categories
nlcd_reclass <- classify(nlcd, reclass_df |> select(value, new_value), others = 0)

levels(nlcd_reclass) <- new_cats |> select(value = new_value, "NLCD Land Cover ReClass" = new_category)

plot(nlcd_reclass)
# Looks great

nlcd_reclass |> writeRaster("data/predictors/nlcd_reclass_landcover.tif")

# CES Pollutants and Traffic ---------------------------------------
ces.sf <- read_sf("~/Data/Environment/CalEnviroScreen4.0/calenviroscreen40gdb_F_2021.gdb/")
ces.v <- vect("~/Data/Environment/CalEnviroScreen4.0/calenviroscreen40gdb_F_2021.gdb/")

ces_crop.v <-
  ces.v |>
  makeValid() |>
  project(empty.sr) |>
  crop(empty.sr)

ces_traffic.sr <- ces_crop.v |>
  rasterize(empty.sr, "Traffic")
# plot(ces_traffic.sr)

ces_pollutant.sr <- ces_crop.v |>
  rasterize(empty.sr, "PollutionScore")
# plot(ces_pollutant.sr)


# # CIscore data is incomplete. I will fill it in with a quick and dirty 'focal'
# # TODO
# newCI.sr <- terra::focal(ces.stack["CIscore"], w = 299, # in cell size, not sure why it has to be an odd number??
#                          na.policy = "only", fun = "mean",
#                          na.rm = TRUE)

# names(newCI.sr) <- "CIscore"

# new_ces.stack <- c(newCI.sr, ces.stack["Traffic"])

writeRaster(ces_traffic.sr, "data/predictors/CES_traffic.tif", overwrite = T)
writeRaster(ces_pollutant.sr, "data/predictors/CES_pollution.tif", overwrite = T)


# Greenspaces ---------------------------------------

## Christine Greenspaces  ---------------------------------------
# At the scale of each city, not regional
og_grn <- st_read("data/greenspaces/christine/Greenspaces-2025") |>
  st_zm(drop = T, what = "ZM")

og_grn |>
  # filter(Final_CLN2 == "Urban") |>
  filter(!(Final_CLN2 %in% c(c("Essential", "Contributor", "Important")))) |>
  # filter(Final_CLN2 %in% c(c("Essential", "Contributor", "Important"))) |>
  ggplot() +
  geom_sf(aes(fill = Final_CLN2))
og_grn |> distinct(Final_CLN2)

og_grn |>
  filter(if_any(where(is.character), ~ str_detect(., "Essential")))

og_grn |>
  filter(COUNTY == "San Francisco") |>
  plot()

city_greenspace <- og_grn |>
  # filter(Final_CLN2 %in% c(c("Essential", "Contributor", "Important"))) |>
  st_transform(4326) |>
  rename(geom = geometry) |>
  st_make_valid() |>
  group_by(GreenSpace, Final_CLN2) |>
  summarize(geom = st_union(geom), original_piece_count = n()) |>
  mutate(
    area_m2 = st_area(geom),
    area_acres = st_area(geom) |> set_units("acre")
  )

city_greenspace |> write_sf("data/greenspaces/city_greenspace.gpkg")


## CLN Greenspaces  ---------------------------------------

cln_grn <- st_read("~/Data/Boundaries/Political/CLN2.0/CLN_Database_2_0_1/data/CLN/final_cln2_2019.shp") |>
  # st_transform(4326) |>
  filter(Final_CLN2 %in% c(c("Essential", "Contributor", "Important"))) |>
  st_transform(3310) |>
  st_cast("POLYGON") |>
  mutate(geom_wkt = st_as_text(geometry))

cln_grn |> write_sf("data/greenspaces/cln_greenspace.gpkg")

ggplot(cln_grn) +
  geom_sf(aes(fill = Final_CLN2))
