library(stars)
library(tidyverse)
library(sf)
library(terra)
library(patchwork)



study_cities_pre <- vect("data/BayAreaCities_CLN.gpkg")
oak_pied <- study_cities_pre[study_cities_pre$jurname %in% c("Piedmont", "Oakland"), ] |>
  aggregate(dissolve = TRUE)
oak_pied$jurname <- "Oakland"
study_cities <- study_cities_pre[!study_cities_pre$jurname %in% c("Piedmont", "Oakland"), ] |>
  rbind(oak_pied)



# # CA Newt total map -----------------------------------------------

results.dir <- "results/sdm/run-2025-01-21_new-species/"
tt_pred.strs <- read_stars(paste0(results.dir, "Callipepla_californica/prediction.tif"))

tt_pts.sf <- st_read(paste0(results.dir, "spec_obs_thinned.gpkg")) %>%
  filter(species == "Callipepla californica")

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

tt.g

# Map by city ---------------------------------------


city_pred.ls <- map(1:nrow(study_cities), .f = function(cityrow) {
  city <- study_cities[cityrow, ]
  predcity <- rast(tt_pred.strs) |>
    crop(city) |>
    mask(city) |>
    st_as_stars()

  # names(predcity) <- city$jurname
  return(predcity)
})

city_maps.ls <- map(city_pred.ls, .f = function(pred.strs) {
  pts.sf <- tt_pts.sf |> st_crop(pred.strs |> rast())
  # browser()
  ggplot() +
    geom_stars(
      data = pred.strs, aes(fill = prediction.tif),
      na.action = na.omit
    ) +
    scale_fill_viridis_c() +
    geom_sf(data = pts.sf %>% st_geometry(), color = "white") +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.text = element_blank(),
      legend.key.size = unit(1.5, "cm")
    ) #+
  # labs(title = names(pred.strs))
})

patchmap <- (city_maps.ls[[3]] + city_maps.ls[[1]]) +
  (city_maps.ls[[2]])

ggsave(paste0(results.dir, "3citymaps.png"), patchmap, width = 12, height = 6)

# Variable importance ---------------------------------------
# "/results/sdm/run-2024-07-24_newvars/Taricha_torosa/var_imp.csv"
tt_varimp <- read_csv(paste0(results.dir, "Taricha_torosa/var_imp.csv"))

varimp.plot <- ggplot(
  tt_varimp,
  aes(
    forcats::fct_reorder(Variable, Permutation_importance, .desc = T),
    Permutation_importance
  )
) +
  geom_col(fill = "#EDB183") +
  geom_errorbar(
    aes(
      ymin = Permutation_importance - sd,
      ymax = Permutation_importance + sd
    ),
    width = .2
  ) +
  labs(x = "", y = "Relative Variable Importance") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 20, face = "bold")
  )

varimp.plot


# Accuracy Metrics ---------------------------------------
library(maxnet)
library(glmnet)
library(SDMtune)
sdm_mod <- readRDS(paste0(results.dir, "Callipepla_californica/sdm_model.rds"))
auc(sdm_mod)
tss(sdm_mod)
