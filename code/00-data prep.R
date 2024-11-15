terrestrial.sf <- st_read("~/Data/Boundaries/Political/CLN2.0/CLN_Database_2_0_1/data/Base/vector/terrestrial.shp")


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

# sf_use_s2(T)
counties.sf <- st_read("~/Data/Boundaries/Political/CA_Counties/CA_Counties_TIGER2016.shp") |>
  filter(NAME %in% c(
    "Solano", "Contra Costa", "Alameda", "Santa Clara", "San Mateo",
    "San Francisco", "Marin", "Sonoma", "Napa", "Santa Cruz"
  )) |>
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
con <- dbConnect(RPostgreSQL::PostgreSQL(),
  host = "flor",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

species_names <- c("Taricha torosa")

bigquery <- paste0("
SELECT * FROM ca_core
INNER JOIN ca_species ON ca_species.taxonKey=ca_core.taxonKey
INNER JOIN ca_extra ON ca_extra.gbifid=ca_core.gbifid
WHERE ST_Intersects(ca_core.geom,'", target.ewkt, "')
AND ca_species.taxonrank='SPECIES'
AND ca_species.species IN ('", paste(species_names, collapse = "','"), "')")

system.time({
  target_spec.sf <- st_read(con, query = bigquery)
})

write_sf(target_spec.sf,
  paste0(
    "data/occurrence/",
    format(Sys.time(), "%Y-%m-%d"),
    "_target_spec.gpkg"
  ),
  layer = "spec_occ"
)


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

# nlcd_chr2_pre <- rast("~/Data/Environment/NLCD/NLCD_ChristineCustom/nlcd_2001_2019_change_index_l48_20210604/nlcd_2001_2019_change_index_l48_20210604.img")


# plot(nlcd_chr_pre)

nlcd <- rast("data/predictors/nlcd_landcover.tif")
# plot(nlcd)

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
og_grn <- st_read("data/greenspaces/christine/Greenspaces/GreenSpace_Nov2024.shp")

og_grn |>
  # filter(Final_CLN2 == "Urban") |>
  ggplot() +
  geom_sf(aes(fill = Final_CLN2))
og_grn |> distinct(Final_CLN2)

og_grn |>
  filter(if_any(where(is.character), ~ str_detect(., "Essential")))

og_grn |>
  filter(Final_CLN2 %in% c(c("Essential", "Contributor", "Important"))) |>
  st_transform(3310) |>
  st_cast("POLYGON") |>
  mutate(geom_wkt = st_as_text(geometry)) |>
  write_sf("data/greenspaces/city_greenspace.gpkg")


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
