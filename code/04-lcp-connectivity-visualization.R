library(tidyverse)
library(terra)
library(tidyterra)
library(patchwork)
library(sf)
library(units)


# results_path <- "results/connectivity/2025-01-20_checking-problems/"


target_species <- c("Callipepla_californica", "Lynx_rufus")

spec_plots <- map(target_species, function(spec) {
  results_path <- sprintf("results/connectivity/2025-02-24_lcp_addspec/%s", spec)

  # City boundaries
  city_boundaries_prep <- st_read("data/city_boundaries.gpkg")

  lcp_allgreen <- st_read(sprintf("./results/connectivity/2025-02-24_lcp_addspec/%s/paths_allgreen.gpkg", spec))
  lcp_esssential <- st_read(sprintf("./results/connectivity/2025-02-24_lcp_addspec/%s/paths_essential.gpkg", spec))

  lcp_dens_allgreen <- rast(sprintf("./results/connectivity/2025-02-24_lcp_addspec/%s/path_density_allgreen.tif", spec))
  lcp_dens_essential <- rast(sprintf("./results/connectivity/2025-02-24_lcp_addspec/%s/path_density_essential.tif", spec))

  agg_lcp <- bind_rows(
    lcp_allgreen |> mutate(greenspace = "all"),
    lcp_esssential |> mutate(greenspace = "essential")
  ) |>
    mutate(path_length = st_length(geom)) |>
    filter(path_length > (0 |> set_units("m")))

  bb_pathcost.plt <- agg_lcp |> ggplot() +
    geom_boxplot(aes(x = greenspace, y = cost))

  bb_pathlength.plt <- agg_lcp |> ggplot() +
    geom_boxplot(aes(x = greenspace, y = path_length))

  max_dens_val <- max(c(
    values(lcp_dens_allgreen),
    values(lcp_dens_essential)
  ), na.rm = T)

  # LCP Density
  all_map.plt <- ggplot() +
    geom_spatraster(data = lcp_dens_allgreen) +
    # geom_sf(data = suitable_cents, color = "white") +
    scale_fill_continuous(limits = c(0, max_dens_val)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank()
    ) +
    labs(title = "All Greenspaces", fill = "Path density")

  ess_map.plt <- ggplot() +
    geom_spatraster(data = lcp_dens_essential) +
    # geom_sf(data = suitable_cents, color = "white") +
    theme_minimal() +
    theme(axis.text = element_blank()) +
    scale_fill_continuous(limits = c(0, max_dens_val)) +
    labs(title = "Essential Only", fill = "Path density")

  return(list(
    bb_pathcost.plt = bb_pathcost.plt,
    bb_pathlength.plt = bb_pathlength.plt,
    all_map.plt = all_map.plt,
    ess_map.plt = ess_map.plt
  ))
})


names(spec_plots) <- target_species

spec_plots$Callipepla_californica$bb_pathcost.plt
spec_plots |>
  list_flatten() |>
  wrap_plots(ncol = 4, widths = c(1, 1, 2, 2))

spec_plots$Callipepla_californica |>
  list_flatten() |>
  wrap_plots(nrol = 4, heights = 1)

p <- all_plt + ess_plt
ggsave(paste0(results_dir, "/fpplot.png"), plot = p)














essential_curr <- rast(paste0(results_path, "/essential_only/cum_currmap.tif"))
# essential_curr <- rast(paste0(results_path, "essential_only/normalized_cum_currmap.tif"))
# names(essential_curr) <- "cum_currmap"
# essential_curr |>
#   values() |>
#   mean(na.rm = T)

allgrn_curr <- rast(paste0(results_path, "/all_greenspace/cum_currmap.tif"))
# allgrn_curr <- rast(paste0(results_path, "all_greenspace/normalized_cum_currmap.tif"))
# names(allgrn_curr) <- "cum_currmap"
# allgrn_curr |>
#   values() |>
#   mean(na.rm = T)

# hist(allgrn_curr, main = "allgrn")
# hist(essential_curr)

sum(allgrn_curr |> values(), na.rm = T) - sum(essential_curr |> values(), na.rm = T)


cutAndProcessCurrs <- function(x, .curr) {
  # browser()
  city_feature <- city_boundaries[x, ]
  name <- city_feature |> pull(jurname)
  .curr |>
    crop(city_feature, mask = T) |>
    trim() |>
    mutate(
      sqrtval = case_when(
        !is.na(cum_currmap) ~ sqrt(cum_currmap),
        T ~ NA
      )
    )
}

# Essential
ess_curr_list <- map(
  seq_len(nrow(city_boundaries)),
  function(x) cutAndProcessCurrs(x, essential_curr)
)
names(ess_curr_list) <- city_boundaries |> pull(jurname)


# All green
allgrn_curr_list <- map(
  seq_len(nrow(city_boundaries)),
  function(x) cutAndProcessCurrs(x, allgrn_curr)
)
names(allgrn_curr_list) <- city_boundaries |> pull(jurname)

# allgrn_curr_list |> map(plot)


# Convert to dfs
raster_to_df <- function(raster, name) {
  as.data.frame(raster, xy = TRUE) %>%
    rename(Value = 3) %>% # Rename the third column to "Value"
    mutate(City = name) # Add a column for facet labels
}

# Apply the conversion to the list
allgrn_df <- imap(allgrn_curr_list, raster_to_df) |>
  bind_rows() |>
  mutate(Greenspaces = "All")
ess_df <- imap(ess_curr_list, raster_to_df) |>
  bind_rows() |>
  mutate(Greenspaces = "Essential Only")

both_greencats_df <- bind_rows(allgrn_df, ess_df) |> tibble()

centered_geography <- both_greencats_df |>
  group_by(City) %>%
  mutate(
    x_centered = x - mean(x, na.rm = TRUE),
    y_centered = y - mean(y, na.rm = TRUE)
  ) %>%
  ungroup()

full.plot <- ggplot(centered_geography, aes(x = x_centered, y = y_centered, color = sqrtval)) +
  geom_point() +
  scale_fill_viridis_c() + # Better color scale for continuous data
  facet_grid(vars(City), vars(Greenspaces)) +
  coord_equal() +
  theme_bw() +
  labs(
    title = "Raster Facet Plot",
    x = "Longitude",
    y = "Latitude",
    fill = "Value"
  )

ggsave(paste0(results_path, "all_comp.png"), full.plot, width = 6, height = 8)


both_greencats_df |>
  sample_n(10000) |>
  ggplot() +
  geom_boxplot(aes(x = Greenspaces, y = Value), outlier.shape = NA) +
  # coord_cartesian(ylim = c(0, 200)) +
  facet_grid(vars(City), scales = "free_y")

both_greencats_df |>
  group_by(City, Greenspaces) |>
  summarise(
    mean_val = mean(Value, na.rm = TRUE),
    sum_val = sum(Value, na.rm = TRUE)
  )

# Checking resistance plots ---------------------------------------
resistance_ras <- list(
  res_all = rast(paste0(results_path, "resistance_allgrn.tif")),
  res_ess = rast(paste0(results_path, "resistance_essential.tif"))
)

resistance_df <- resistance_ras |>
  map_df(function(x) {
    # browser()
    # x@res
    x |>
      as_tibble() |>
      filter(!is.na(lyr.1)) |>
      mutate(source = basename(sources(x))) |>
      rename(resistance = lyr.1)
  })


# Essential
ess_resistance_list <- map(
  seq_len(nrow(city_boundaries)),
  function(x) cutAndProcessCurrs(x, resistance_ras[[2]])
)
names(ess_resistance_list) <- city_boundaries |> pull(jurname)


resistance_df |>
  sample_n(10000) |>
  ggplot() +
  geom_boxplot(aes(x = source, y = resistance), outlier.shape = NA) # +
# coord_cartesian(ylim = c(0, 3000)) +
# facet_grid(vars(City), scales = "free_y")
