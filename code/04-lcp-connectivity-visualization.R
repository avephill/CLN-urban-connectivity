library(tidyverse)
library(terra)
library(tidyterra)
library(patchwork)
library(sf)
library(units)



results_path <- "results/connectivity/2025-03-18_lcp-new-greenspace"


target_species <- c("Callipepla_californica", "Lynx_rufus", "Pituophis_catenifer")


violins_n_means <- map(target_species, function(spec) {
  result_path <- sprintf("%s/%s", results_path, spec)

  essential_color <- "#296218"
  all_color <- "#A8C8D1"

  # City boundaries
  city_boundaries_prep <- st_read("data/city_boundaries.gpkg")
  # browser()
  lcp_allgreen <- st_read(sprintf("%s/paths_allgreen.gpkg", result_path))
  lcp_esssential <- st_read(sprintf("%s/paths_essential.gpkg", result_path))

  agg_lcp <- bind_rows(
    lcp_allgreen |> mutate(greenspace = "all"),
    lcp_esssential |> mutate(greenspace = "essential")
  ) |>
    mutate(path_length = st_length(geom)) |>
    filter(path_length > (0 |> set_units("m"))) |> 
    mutate(path_length = path_length |> as.vector())
  
  # Calculate p-values
  cost_test <- wilcox.test(cost ~ greenspace, data = agg_lcp)
  length_test <- wilcox.test(path_length ~ greenspace, data = agg_lcp)

  pval_text <- function(p) {
    if (p < 0.001) return("p < 0.001")
    return(sprintf("p = %.3f", p))
  }

  bb_pathcost.plt <- agg_lcp |> ggplot() +
    geom_violin(aes(x = greenspace, y = cost, fill = greenspace)) +
    scale_fill_manual(values = c("all" = all_color, "essential" = essential_color)) +
    guides(fill = "none") +
    stat_summary(aes(x = greenspace, y = cost),
      fun = mean,
      geom = "point",
      color = "red",
      size = 3,
      shape = 18
    ) +
    stat_summary(aes(x = greenspace, y = cost),
      fun.data = mean_cl_normal,
      geom = "errorbar",
      width = 0.2,
      color = "red"
    ) +
    annotate("text", 
             x = 1.5, 
             y = max(agg_lcp$cost, na.rm = TRUE) * 0.9,
             label = pval_text(cost_test$p.value),
             vjust = -0.5,
             size = 3) +
    scale_x_discrete(labels = c("all" = "All Greenspaces", "essential" = "Essential Only")) +
    labs(x = "Greenspace Type",
         y = "Path Cost",
         title = "Path Cost") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )

  bb_pathlength.plt <- agg_lcp |> ggplot() +
    geom_violin(aes(x = greenspace, y = path_length, fill = greenspace)) +
    scale_fill_manual(values = c("all" = all_color, "essential" = essential_color)) +
    guides(fill = "none") +
    stat_summary(aes(x = greenspace, y = path_length),
      fun = mean,
      geom = "point",
      color = "red",
      size = 3,
      shape = 18
    ) +
    stat_summary(aes(x = greenspace, y = path_length),
      fun.data = mean_cl_normal,
      geom = "errorbar",
      width = 0.2,
      color = "red"
    ) +
    annotate("text", 
             x = 1.5, 
             y = max(agg_lcp$path_length, na.rm = TRUE) * 0.9,
             label = pval_text(length_test$p.value),
             vjust = -0.5,
             size = 3) +
    scale_x_discrete(labels = c("all" = "All Greenspaces", "essential" = "Essential Only")) +
    labs(x = "Greenspace Type",
         y = "Path Length (m)",
         title = "Path Length") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )

  # Get path density from rasters
  lcp_dens_allgreen <- rast(sprintf("%s/path_density_allgreen.tif", result_path))
  lcp_dens_essential <- rast(sprintf("%s/path_density_essential.tif", result_path))

  agg_density <- bind_rows(
    lcp_dens_allgreen |> as_tibble() |> mutate(greenspace = "all"),
    lcp_dens_essential |> as_tibble() |> mutate(greenspace = "essential")
  ) |>
    rename(path_density = crosses) |>
    filter(!is.na(path_density), path_density > 0)

    density_test <- wilcox.test(path_density ~ greenspace, data = agg_density)

  bb_pathdensity.plt <- agg_density |> ggplot() +
    geom_violin(aes(x = greenspace, y = path_density, fill = greenspace)) +
    scale_fill_manual(values = c("all" = all_color, "essential" = essential_color)) +
    guides(fill = "none") +
    stat_summary(aes(x = greenspace, y = path_density),
      fun = mean,
      geom = "point",
      color = "red",
      size = 3,
      shape = 18
    ) +
    stat_summary(aes(x = greenspace, y = path_density),
      fun.data = mean_cl_normal,
      geom = "errorbar",
      width = 0.2,
      color = "red"
    ) +
    annotate("text",
      x = 1.5,
      y = max(agg_density$path_density, na.rm = TRUE) * 0.9,
      label = pval_text(density_test$p.value),
      vjust = -0.5,
      size = 3
    ) +
    scale_x_discrete(labels = c("all" = "All Greenspaces", "essential" = "Essential Only")) +
    labs(
      x = "Greenspace Type",
      y = "Path Density",
      title = "Path Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  # Calculate summary statistics
  cost_stats <- agg_lcp |>
    group_by(greenspace) |>
    summarise(
      mean_cost = mean(cost, na.rm = TRUE),
      median_cost = median(cost, na.rm = TRUE),
      n_cost = n()
    )

  length_stats <- agg_lcp |>
    group_by(greenspace) |>
    summarise(
      mean_length = mean(path_length, na.rm = TRUE),
      median_length = median(path_length, na.rm = TRUE),
      n_length = n()
    )

  density_stats <- agg_density |>
    group_by(greenspace) |>
    summarise(
      mean_density = mean(path_density, na.rm = TRUE),
      median_density = median(path_density, na.rm = TRUE),
      n_density = n()
    )

  # Create stats tibble with proper row alignment
  stats_tibble <- tibble(
    species = spec,
    metric = c("cost", "path_length", "path_density"),
    p_value = c(
      cost_test$p.value,
      length_test$p.value,
      density_test$p.value
    ),
    test_statistic = c(
      cost_test$statistic,
      length_test$statistic,
      density_test$statistic
    )
  )

  # Add means
  stats_tibble$mean_all <- c(
    cost_stats$mean_cost[cost_stats$greenspace == "all"],
    length_stats$mean_length[length_stats$greenspace == "all"],
    density_stats$mean_density[density_stats$greenspace == "all"]
  )
  stats_tibble$mean_essential <- c(
    cost_stats$mean_cost[cost_stats$greenspace == "essential"],
    length_stats$mean_length[length_stats$greenspace == "essential"],
    density_stats$mean_density[density_stats$greenspace == "essential"]
  )

  # Add medians
  stats_tibble$median_all <- c(
    cost_stats$median_cost[cost_stats$greenspace == "all"],
    length_stats$median_length[length_stats$greenspace == "all"],
    density_stats$median_density[density_stats$greenspace == "all"]
  )
  stats_tibble$median_essential <- c(
    cost_stats$median_cost[cost_stats$greenspace == "essential"],
    length_stats$median_length[length_stats$greenspace == "essential"],
    density_stats$median_density[density_stats$greenspace == "essential"]
  )

  # Add sample sizes
  stats_tibble$n_all <- c(
    cost_stats$n_cost[cost_stats$greenspace == "all"],
    length_stats$n_length[length_stats$greenspace == "all"],
    density_stats$n_density[density_stats$greenspace == "all"]
  )
  stats_tibble$n_essential <- c(
    cost_stats$n_cost[cost_stats$greenspace == "essential"],
    length_stats$n_length[length_stats$greenspace == "essential"],
    density_stats$n_density[density_stats$greenspace == "essential"]
  )

  return(list(plots = list(
    bb_pathcost.plt = bb_pathcost.plt,
    bb_pathlength.plt = bb_pathlength.plt,
    bb_pathdensity.plt = bb_pathdensity.plt
  ),
  stats = stats_tibble))
})

violins_n_means.comp <- violins_n_means |>
  map("plots") |>
  list_flatten() |>
  wrap_plots(ncol = 3, widths = c(1, 1,1)) +
  plot_annotation(tag_levels = list(target_species |> map(~ c(.x, "","")) |> flatten_chr())) +
  theme(plot.tag = element_text(angle = 90, size = 2))

violins_n_means.comp

# save to results_dir
ggsave(paste0(results_path, "/violins_n_means.pdf"), plot = violins_n_means.comp)

violins_n_means |>
  map("stats") |>
  bind_rows() |> write_csv(paste0(results_path, "/violins_n_means_stats.csv"))



# This is old and includes maps
boxplots_n_maps <- map(target_species, function(spec) {
  result_path <- sprintf("%s/%s", results_path, spec)

  # City boundaries
  city_boundaries_prep <- st_read("data/city_boundaries.gpkg")
# browser()
  lcp_allgreen <- st_read(sprintf("%s/paths_allgreen.gpkg", result_path))
  lcp_esssential <- st_read(sprintf("%s/paths_essential.gpkg", result_path))

  lcp_dens_allgreen <- rast(sprintf("%s/path_density_allgreen.tif", result_path))
  lcp_dens_essential <- rast(sprintf("%s/path_density_essential.tif", result_path))

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

# Full plot
spec_plots |>
  list_flatten() |>
  wrap_plots(ncol = 4, widths = c(1, 1, 2, 2))

dens_list <- map(spec_plots, ~ list(
  all_map.plt = .x$all_map.plt,
  ess_map.plt = .x$ess_map.plt
))
  

  dens_list[[c(1, 2)]] |> list_flatten() |> 
  wrap_plots(ncol = 2)

p <- all_plt + ess_plt
ggsave(paste0(results_dir, "/fpplot.png"), plot = p)




spec_plots |> list









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
