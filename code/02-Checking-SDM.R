library(stars)
library(tidyverse)
library(sf)
library(terra)
library(patchwork)
library(maxnet)
library(glmnet)
library(SDMtune)

study_cities <- st_read("data/city_boundaries.gpkg")

# results.dir <- "results/sdm/run-2025-03-18_new-greenspace-preds/"
results.dir <- "results/sdm/run-2025-03-29_city-limits/"
species <- c("Callipepla californica", "Pituophis catenifer")


# species <- c("Callipepla californica", "Lynx rufus", "Pituophis catenifer")

process_sdm_results <- function(spec) {
  spec_pn <- spec |> str_replace_all(" ", "_")

  # Regional SDM map -----------------------------------------------
  tt_pred.strs <- read_stars(paste0(results.dir, spec_pn, "/prediction.tif"))

  tt_pts.sf <- st_read(paste0(results.dir, "spec_obs_thinned.gpkg")) |>
    filter(species == spec)
  # browser()
  tt_pts.sf |> distinct(species)
  # st_read(paste0(results.dir, "spec_obs_thinned.gpkg")) |> select(species)

  tt.g <-
    ggplot() +
    geom_stars(
      data = tt_pred.strs, aes(fill = prediction.tif),
      na.action = na.omit
    ) +
    scale_fill_viridis_c() +
    geom_sf(data = tt_pts.sf %>% st_geometry(), color = "white") +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.text = element_blank(),
      legend.key.size = unit(1.5, "cm")
    )

  ggsave(paste0(results.dir, spec_pn, "/regionmap.pdf"), tt.g)

  # Map by city ---------------------------------------
  city_name <- case_when(
    spec == "Pituophis catenifer" ~ "San Jose",
    spec == "Callipepla californica" ~ "San Francisco",
    spec == "Lynx rufus" ~ "Oakland_Piedmont",
    TRUE ~ NA # default case
  )
  city <- study_cities |> filter(jurname == city_name)

  city_pred <- rast(tt_pred.strs) |>
    crop(city) |>
    mask(city) |>
    st_as_stars()

  pts.sf <- tt_pts.sf |> st_crop(city_pred |> rast())

  citymap <- ggplot() +
    geom_stars(
      data = city_pred, aes(fill = prediction.tif),
      na.action = na.omit
    ) +
    scale_fill_viridis_c() +
    geom_sf(data = pts.sf %>% st_geometry(), color = "white") +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.text = element_blank(),
      legend.key.size = unit(1.5, "cm")
    )

  ggsave(paste0(results.dir, spec_pn, "/citymap.pdf"), citymap)

  # Variable importance ---------------------------------------
  varimp <- read_csv(paste0(results.dir, spec_pn, "/var_imp.csv")) |> arrange(desc(Permutation_importance))
  # X and Y explained Z% of the total variation explained by the variables.

  varimp.plot <- ggplot(
    varimp,
    aes(forcats::fct_reorder(Variable, Permutation_importance, .desc = T), Permutation_importance)
  ) +
    geom_col(fill = "#EDB183") +
    labs(x = "", y = "Relative Variable Importance") +
    coord_flip() +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12),
      axis.title.x = element_text(size = 20, face = "bold")
    )

  ggsave(paste0(results.dir, spec_pn, "/varimp.pdf"), varimp.plot)

  # Accuracy Metrics ---------------------------------------
  # browser()
  sdm_mod <- readRDS(paste0(results.dir, spec_pn, "/sdm_model.rds"))
  spec_auc <- auc(sdm_mod)
  spec_tss <- tss(sdm_mod)
  spec_occ <- sdm_mod@data@pa |> sum(na.rm = T)
  # spec_aicc <- ?SDMtune::aicc()
  spec_acc <- tibble(
    species = spec, auc = spec_auc, tss = spec_tss,
    occurrences = spec_occ
  )

  spec_acc |>
    write_csv(paste0(results.dir, spec_pn, "/sdm_stats.csv"))
}

# Process all species
walk(species, process_sdm_results)
