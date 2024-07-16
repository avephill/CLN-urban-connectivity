library(stars)
library(terra)
library(MASS)
library(blockCV)
library(tidyverse)
library(CoordinateCleaner)
library(SDMtune)
library(furrr)
library(RPostgreSQL)

# bioclim_dic <- read_csv("../../../Data/Dictionaries/bioclim_dictionary.csv")
# setwd("~/Projects/together-bay-area/together-bay-area/")

terrestrial.sf <- st_read("~/Data/Boundaries/Political/CLN2.0/CLN_Database_2_0_1/data/Base/vector/terrestrial.shp")
# urban.sf <- st_read("data/urban_area.gpkg") |> rename(geometry = geom)

# sdm_train_mask.sf <- urban.sf # terrestrial.sf
sdm_train_mask.sf <- terrestrial.sf


results.dir <- paste0("results/sdm/run-",
                      format(Sys.time(), "%Y-%m-%d"),
                      "_firstshot/")

dir.create(results.dir, recursive = T)

# -------------------------------------------------------------------------

# Predictors --------------------------------------------------------------

# -------------------------------------------------------------------------

bioclim.stack <- rast("data/predictors/bioclim.tif")
# vif.out <- vifstep(bioclim.stack |> as.data.frame(), th=10)
# vif.out@excluded |> paste(collapse="','") |> paste0("c('",.,"')")
vif_excluded <- c(
  "Temperature Annual Range", "Precipitation of Wettest Quarter",
  "Temperature Seasonality", "Precipitation of Coldest Quarter",
  "Mean Temperature of Driest Quarter", "Annual Precipitation",
  "Max Temperature of Warmest Month",
  "Mean Temperature of Warmest Quarter", "Annual Mean Temperature",
  "Min Temperature of Coldest Month", "Precipitation of Wettest Month"
)

bioclim_uncorr.stack <- subset(bioclim.stack, vif_excluded, negate = T)

list.files("data/predictors")
predictor_names <- c(
  "altitude", "slope", "solarradiation",
  "vegetation",
  "nlcd_landcover", "nlcd_treecover",
  "wetland_fresh_distance", "greenspace10acre_distance", "natural-stream-dist",
  "ces"
  # "soils_suborder" # incomplete data
)
predictors.sr <-
  rast(list.files("data/predictors", full.names = T) |>
    str_subset(".tif$") |>
    str_subset(paste(predictor_names, collapse = "|"))) |>
  c(bioclim_uncorr.stack) |>
  mask(sdm_train_mask.sf |> st_transform(4326))

# names(predictors.sr) <- names(predictors.sr) |> str_replace_all(" ", "_")

name_change.df <-
  tibble(RAWNAME = names(predictors.sr)) |>
  mutate(DERNAME = case_when(
    RAWNAME == "dem30m_ba_reg" ~ "Elevation",
    RAWNAME == "distance_to_greenspace" ~ "Distance to 10acre Greenspace",
    RAWNAME == "Tree Cover" ~ "NLCD Percent Tree Cover",
    RAWNAME == "prcnt_slope30" ~ "Percent Slope Grade",
    RAWNAME == "aspect" ~ "Slope Aspect",
    RAWNAME == "CLN2_VEGETATION." ~ "Eveg Vegetation Class",
    RAWNAME == "DIST" ~ "Distance to Standing Freshwater",
    RAWNAME == "natural-stream-dist" ~ "Distance to Freshwater Stream",
    T ~ RAWNAME
  )) |>
  mutate(DERNAME = str_replace_all(DERNAME, " ", "_"))

# Put new names in predictors
names(predictors.sr) <- name_change.df$DERNAME

# fix categorical layers. Need to go in and fix categorical table
# TODO come back later and clean this up

# Eveg
target.layer <- which(name_change.df$DERNAME == "Eveg_Vegetation_Class")
cat_table <- cats(predictors.sr)[[target.layer]]
names(cat_table) <- cats(predictors.sr)[[target.layer]] |>
  names() |>
  str_replace_all("NAME", "Eveg_Vegetation_Class")

cat_table <- cat_table |> dplyr::select(-CLN2_VEGETATION.)

set.cats(predictors.sr,
  layer = target.layer,
  value = cat_table
)

# Fix NLCD
target.layer <- which(name_change.df$DERNAME == "NLCD_Land_Cover_Class")
cat_table <- cats(predictors.sr)[[target.layer]]
names(cat_table) <- cats(predictors.sr)[[target.layer]] |>
  names() |>
  str_replace_all("NLCD Land Cover Class", "NLCD_Land_Cover_Class")

set.cats(predictors.sr,
  layer = target.layer,
  value = cat_table
)


# Write the godforsaken thing
writeRaster(predictors.sr, paste0(results.dir, "predictors.tif"),
  overwrite = T, progress = T
)



# # Look at some correlation
# predictor.df <- predictors.sr  |> as.data.frame()
# predictor.tb <- predictor.df |> tibble() |>
# 	select_if(is.numeric) |>
# 	drop_na()
# cor.test(predictor.tb)
# pred_cor <- cor(predictor.tb, method = c("pearson"))
#
# pred_cor |> write.csv("../results/sdm/predictor_coll.csv")


# -------------------------------------------------------------------------

# Cleaning Occurrence Data ------------------------------------------------

# -------------------------------------------------------------------------


spec.sf <- st_read("data/occurrence/2024-07-16_target_spec.gpkg")

spec_intersect.sf <- spec.sf %>%
  st_intersection(sdm_train_mask.sf %>% st_transform(st_crs(spec.sf)))

spec_cleaned.sf <-
  spec_intersect.sf %>%
  as_tibble() %>%
  # Necessary to let the defaults work
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  # Identify Invalid lat/lon Coordinates
  cc_val() %>%
  # Identify Records with Identical lat/lon
  cc_equ() %>%
  # Identify Coordinates in Vicinity of Country Capitals.
  cc_cap() %>%
  # Identify Coordinates in Vicinity of Country and Province Centroids
  cc_cen() %>%
  # Identify Coordinates Outside their Reported Country
  # cc_coun(iso3 = "USA")%>%
  # Identify Records Assigned to GBIF Headquarters
  cc_gbif() %>%
  # Identify Records in the Vicinity of Biodiversity Institutions
  cc_inst() %>%
  # Identify Non-terrestrial Coordinates
  # cc_sea() %>%
  # Identify Zero Coordinates
  cc_zero() %>%
  # Identify Geographic Outliers in Species Distributions
  # cc_outl() %>%
  # Identify Duplicated Records
  # cc_dupl()
  rename(decimallongitude = decimalLongitude,
         decimallatitude = decimalLatitude) %>% 
  filter(coordinateuncertaintyinmeters <= 250 | is.na(coordinateuncertaintyinmeters)) %>%
  filter(basisofrecord == "HUMAN_OBSERVATION" |
    basisofrecord == "OBSERVATION" |
    basisofrecord == "PRESERVED_SPECIMEN") %>%
  filter(individualcount > 0 | is.na(individualcount)) %>%
  filter(individualcount < 99 | is.na(individualcount)) %>%
  st_as_sf(
    # coords = c("decimallongitude", "decimallatitude"),
    # crs = st_crs(spec.sf),
    sf_column_name = "geom"
  )

# Removed point count
nrow(spec_intersect.sf) - nrow(spec_cleaned.sf)

write_sf(spec_cleaned.sf, paste0(results.dir, "spec_obs.gpkg"))


# -------------------------------------------------------------------------

# Background Data ---------------------------------------------------------

# -------------------------------------------------------------------------
