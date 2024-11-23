# Creates local greenspaces and sdm predictions just for the cities.
# This will later be used to turn into cost resistance maps for the two scenarios
# where we consider all greenspaces and only essential ones




sdm_results_path <- "results/sdm/run-2024-11-22_addgreenspace/"

# City boundaries
city_boundaries_prep <- st_read("data/BayAreaCities_CLN.gpkg")

city_boundaries <- city_boundaries_prep |>
  filter(jurname %in% c("Oakland", "Piedmont")) |>
  summarise(jurname = "Oakland_Piedmont", geom = st_union(geom)) |>
  bind_rows(city_boundaries_prep |>
    filter(jurname %in% c("San Jose", "San Francisco")))

og_predictors <- rast(paste0(sdm_results_path, "predictors.tif"))



# Make city only essential greenspace ---------------------------------------

city_predictors_full <- og_predictors |> crop(city_boundaries, mask = T)
city_predictors_full |>
  subset("Elevation") |>
  plot()

essential_greenspace_names <- list.files("data/predictors", full.names = T) |>
  str_subset(".tif$") |>
  str_subset("essential_greenspace")

essentials.sr <-
  rast(essential_greenspace_names) |> crop(city_boundaries, mask = T)

all_greenspace_names <- city_predictors_full |>
  names() |>
  str_subset("_Greenspace")

non_greenspace <- city_predictors_full |> subset(all_greenspace_names, negate = T)

# This is because i need to rename the essential greenspaces so that the sdm knows
# which is which when it predicts weights from sdm
renaming_df <- names(essentials.sr) |>
  tibble() |>
  rename(ess_name = 1) |>
  mutate(grn_size = str_extract(ess_name, "[0-9]+")) |>
  left_join(all_greenspace_names |>
    tibble() |>
    rename(all_name = 1) |>
    mutate(grn_size = str_extract(all_name, "[0-9]+")))

renaming_df

# Stack into all variables
city_predictors_essential <- non_greenspace |> c(essentials.sr)

# Now rename
names(city_predictors_essential) <-
  names(city_predictors_essential) |>
  tibble() |>
  rename(ess_name = 1) |>
  left_join(renaming_df) |>
  mutate(new_name = case_when(
    is.na(all_name) ~ ess_name,
    T ~ all_name
  )) |>
  pull(new_name)

city_predictors_essential |> writeRaster(paste0(sdm_results_path, "city_predictors_essential.tif"), overwrite = T)

# Make city greenspace using all preds ---------------------------------------
# But use the greenspace distances that were city-specific

non_greenspace

cityall_greenspace_names <- list.files("data/predictors", full.names = T) |>
  str_subset(".tif$") |>
  str_subset("city_all_greenspace")

cityall.sr <-
  rast(cityall_greenspace_names) |> crop(city_boundaries, mask = T)

# This is because i need to rename the greenspaces so that the sdm knows
# which is which when it predicts weights from sdm
renaming_cityall_df <- names(cityall.sr) |>
  tibble() |>
  rename(cityall_name = 1) |>
  mutate(grn_size = str_extract(cityall_name, "[0-9]+")) |>
  left_join(all_greenspace_names |>
    tibble() |>
    rename(standard_name = 1) |>
    mutate(grn_size = str_extract(standard_name, "[0-9]+")))

# Stack into all variables
city_allgrn_predictors <- non_greenspace |> c(cityall.sr)

# Now rename
names(city_allgrn_predictors) <-
  names(city_allgrn_predictors) |>
  tibble() |>
  rename(cityall_name = 1) |>
  left_join(renaming_cityall_df) |>
  mutate(new_name = case_when(
    is.na(standard_name) ~ cityall_name,
    T ~ standard_name
  )) |>
  pull(new_name)

# Looks good
city_allgrn_predictors |> writeRaster(paste0(sdm_results_path, "city_allgrn_predictors.tif"), overwrite = T)
names(city_allgrn_predictors)

# Now make predictions ---------------------------------------
library(maxnet)
library(glmnet)
library(SDMtune)

sdm_mod <- readRDS(paste0(sdm_results_path, "Taricha_torosa/sdm_model.rds"))

sdm_allgreen_prediction <-
  predict(sdm_mod,
    data = city_allgrn_predictors,
    type = "cloglog",
    cores = 5
  )

sdm_essential_prediction <-
  predict(sdm_mod,
    data = city_predictors_essential,
    type = "cloglog",
    cores = 5
  )

plot(sdm_essential_prediction)
plot(sdm_allgreen_prediction)

sdm_essential_prediction |> writeRaster(paste0(sdm_results_path, "grncity_essential_prediction.tif"), overwrite = T)
sdm_allgreen_prediction |> writeRaster(paste0(sdm_results_path, "grncity_all_prediction.tif"), overwrite = T)
