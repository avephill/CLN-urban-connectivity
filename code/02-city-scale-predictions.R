# City-scale Species Distribution Model (SDM) Predictions
# This script processes greenspace data and generates SDM predictions for urban areas
# It creates two scenarios:
# 1. Essential greenspaces only
# 2. All greenspaces

# Load required libraries
library(sf) # for spatial data handling
library(terra) # for raster operations
library(tidyverse) # for data manipulation
library(maxnet) # for SDM modeling
library(glmnet) # for SDM modeling
library(SDMtune) # for SDM tuning

# Define paths and load data
sdm_results_path <- "results/sdm/run-2025-03-18_new-greenspace-preds/"
city_boundaries_prep <- st_read("data/city_boundaries.gpkg")
og_predictors <- rast(paste0(sdm_results_path, "predictors.tif"))

# Process city-scale predictors --------------------------------------------

# Crop predictors to city boundaries
city_predictors_full <- og_predictors |> crop(city_boundaries, mask = T)

# Extract greenspace-related layer names
sdm_gn_names <- city_predictors_full |>
  names() |>
  str_subset("_Greenspace")

# Load essential greenspace data
essential_greenspace_names <- list.files("data/predictors", full.names = T) |>
  str_subset(".tif$") |>
  str_subset("essential_greenspace")

essentials.sr <- rast(essential_greenspace_names) |> crop(city_boundaries, mask = T)

# Load all greenspace data
all_city_greenspace_names <- list.files("data/predictors", full.names = T) |>
  str_subset(".tif$") |>
  str_subset("city_all_greenspace")

all_city_greenspace.sr <- rast(all_city_greenspace_names) |> crop(city_boundaries, mask = T)

# Extract non-greenspace predictors
non_greenspace_names <- city_predictors_full |>
  names() |>
  str_subset("_Greenspace")
non_greenspace <- city_predictors_full |> subset(non_greenspace_names, negate = T)

# Create mapping between different greenspace naming conventions
# This ensures SDM can correctly identify greenspace layers during prediction
renaming_df <- names(essentials.sr) |>
  tibble() |>
  rename(ess_name = 1) |>
  mutate(grn_size = str_extract(ess_name, "[0-9]+")) |>
  left_join(names(all_city_greenspace.sr) |>
    tibble() |>
    rename(all_name = 1) |>
    mutate(grn_size = str_extract(all_name, "[0-9]+"))) |>
  left_join(sdm_gn_names |>
    tibble() |>
    rename(sdm_name = 1) |>
    mutate(grn_size = str_extract(sdm_name, "[0-9]+")))

# Create and save essential greenspace scenario ----------------------------

# Combine non-greenspace predictors with essential greenspace data
city_predictors_essential <- non_greenspace |> c(essentials.sr)

# Rename layers to match SDM expectations
names(city_predictors_essential) <-
  names(city_predictors_essential) |>
  tibble() |>
  rename(ess_name = 1) |>
  left_join(renaming_df) |>
  mutate(new_name = case_when(
    is.na(sdm_name) ~ ess_name,
    T ~ sdm_name
  )) |>
  pull(new_name)

# Save essential greenspace scenario
city_predictors_essential |> writeRaster(paste0(sdm_results_path, "city_predictors_essential.tif"), overwrite = T)

# Create and save all greenspace scenario ---------------------------------

# Combine non-greenspace predictors with all greenspace data
city_allgrn_predictors <- non_greenspace |> c(all_city_greenspace.sr)

# Rename layers to match SDM expectations
names(city_allgrn_predictors) <-
  names(city_allgrn_predictors) |>
  tibble() |>
  rename(all_name = 1) |>
  left_join(renaming_df) |>
  mutate(new_name = case_when(
    is.na(sdm_name) ~ all_name,
    T ~ sdm_name
  )) |>
  pull(new_name)

# Save all greenspace scenario
city_allgrn_predictors |> writeRaster(paste0(sdm_results_path, "city_allgrn_predictors.tif"), overwrite = T)

# Generate SDM predictions for each species --------------------------------
library(maxnet)
species <- c("Callipepla_californica", "Lynx_rufus", "Pituophis_catenifer")

map(species, function(x) {
  # Load species-specific SDM model
  sdm_mod <- readRDS(paste0(sdm_results_path, x, "/sdm_model.rds"))

  # Generate predictions for all greenspace scenario
  sdm_allgreen_prediction <- predict(sdm_mod,
    data = city_allgrn_predictors,
    type = "cloglog",
    cores = 5
  )

  # Generate predictions for essential greenspace scenario
  sdm_essential_prediction <- predict(sdm_mod,
    data = city_predictors_essential,
    type = "cloglog",
    cores = 5
  )

  # Save predictions
  sdm_essential_prediction |> writeRaster(paste0(sdm_results_path, x, "/grncity_essential_prediction.tif"), overwrite = T)
  sdm_allgreen_prediction |> writeRaster(paste0(sdm_results_path, x, "/grncity_all_prediction.tif"), overwrite = T)
})
