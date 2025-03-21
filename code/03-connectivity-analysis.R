library(leastcostpath)
library(terra)
library(sf)
library(tidyterra)
library(tidyverse)
library(units)
library(glue)
library(furrr)
library(SDMtune)
library(maxnet)

source("code/03-helper-functions.R")

city_boundaries <- st_read("data/city_boundaries.gpkg")

target_species <- c("Callipepla_californica", "Lynx_rufus", "Pituophis_catenifer")
city_spec <- tibble(species = target_species, city = c("San Francisco", "Oakland_Piedmont", "San Jose"))

# results_dir_parent <- paste0(
#   "./results/connectivity/",
#   format(Sys.time(), "%Y-%m-%d"),
#   "_lcp-new-greenspace"
# )
# dir.create(
#   results_dir_parent,
#   recursive = T
# )
results_dir_parent <- "results/connectivity/2025-03-18_lcp-new-greenspace"



# Important functions ---------------------------------------
writeResults <- function(results, type, results_dir) {
  results$lcp_density |> writeRaster(sprintf("%s/path_density_%s.tif", results_dir, type), overwrite = T)
  results$lcp_paths |> write_sf(sprintf("%s/paths_%s.gpkg", results_dir, type), overwrite = T)
}

# Function for calculating leastcostpath
calc_lcp <- function(conductance, suitable_cents, city_suitability) {
  # browser()
  # lower values is lower conductance, higher values is higher conductance
  cost_mat <- create_cs(
    conductance,
    neighbours = 16,
    dem = NULL,
    max_slope = NULL,
    exaggeration = FALSE
  )

  plan(multicore, workers = 10)
  lcp_paths <- future_map(1:nrow(suitable_cents), function(i) {
    lcp <- create_lcp(
      x = cost_mat,
      origin = suitable_cents[i, ],
      destination = suitable_cents[-i, ],
      cost_distance = T,
      check_locations = F
    )
  }) |>
    bind_rows()
  plan(sequential)

  lcp_density <- create_lcp_density(city_suitability, lcp_paths, rescale = FALSE)

  return(list(lcp_density = lcp_density, lcp_paths = lcp_paths, cost_mat = cost_mat))
}

# Define the function for reclassification
costFunction <- function(HSI, threshold = maxtss_thresh) {
  # Use ifelse for vectorized operations
  ifelse(
    HSI > threshold, 1, # First condition: optimal habitat
    exp(log(0.001) / threshold * HSI) * 10^4 # Second condition: non-habitat/matrix
  )
}

# Run it ---------------------------------------
target_species <- "Pituophis_catenifer"
map(target_species, function(spec) {
  # browser()
  print(sprintf("Working on %s...", spec))
  # Load the habitat suitability raster using terra
  sdm_results_path <- paste0("./results/sdm/run-2025-03-18_new-greenspace-preds/", spec)
  habitat_suitability <- rast(paste0(sdm_results_path, "/prediction.tif"))

  # New results
  results_dir <- sprintf("%s/%s", results_dir_parent, spec)
  dir.create(results_dir, showWarnings = F)

  city <- city_spec |>
    filter(species == spec) |>
    pull(city)
  city_bounds <- city_boundaries |>
    filter(jurname == city)
  print(sprintf("...in %s...", city))

  ## Thresholds ---------------------------------------
  print("Calculating threshold...")
  sdm <- readRDS(paste0(sdm_results_path, "/sdm_model.rds"))

  thresh.df <-
    map(
      sdm@models,
      function(mod) {
        SDMtune::thresholds(mod, type = "cloglog")
      }
    ) %>%
    bind_rows() %>%
    group_by(Threshold) %>%
    summarise_all("mean")

  maxtss_thresh <- thresh.df |>
    filter(Threshold == "Maximum training sensitivity plus specificity") |>
    pull(`Cloglog value`) |>
    as.numeric()

  # Conductance ---------------------------------------
  print("Calculating conductance...")

  # Apply the function to essential greenspace prediction
  essential_prediction <- rast(paste0(sdm_results_path, "/grncity_essential_prediction.tif"))
  essential_resistance <- terra::app(essential_prediction,
    fun = function(x) costFunction(x, maxtss_thresh)
  ) |>
    crop(city_bounds, mask = T) |>
    trim()

  # Create resistance for all greenspace prediction
  city_allgrn_prediction <- rast(paste0(sdm_results_path, "/grncity_all_prediction.tif"))
  allgrn_resistance <- terra::app(city_allgrn_prediction,
    fun = function(x) costFunction(x, maxtss_thresh)
  ) |>
    crop(city_bounds, mask = T) |>
    trim()

  # Write them
  allgrn_resistance |>
    writeRaster(
      paste0(results_dir, "/resistance_allgrn.tif"),
      overwrite = T
    )

  city_allgrn_prediction |>
    crop(city_bounds, mask = T) |>
    trim() |>
    mutate(mean = 1 / costFunction(mean, maxtss_thresh)) |> # inverse of costfunction
    writeRaster(
      paste0(results_dir, "/conductance_allgrn.tif"),
      overwrite = T
    )

  essential_resistance |>
    writeRaster(
      paste0(results_dir, "/resistance_essential.tif"),
      overwrite = T
    )

  essential_prediction |>
    crop(city_bounds, mask = T) |>
    trim() |>
    mutate(mean = 1 / costFunction(mean, maxtss_thresh)) |> # inverse of costfunction
    writeRaster(
      paste0(results_dir, "/conductance_essential.tif"),
      overwrite = T
    )

  # Origins and destinations ---------------------------------------
  city_suitability <- habitat_suitability |>
    crop(city_bounds, mask = T) |>
    trim()

  suitable_patches <- city_suitability |>
    filter(mean > maxtss_thresh) |>
    patches()

  patch_sizes <- cellSize(suitable_patches, unit = "m") |>
    zonal(suitable_patches, sum)

  large_suitable_patches <- suitable_patches |>
    filter(patches %in% (patch_sizes |>
      filter(area > 15000) |>
      pull(patches)))

  large_suitable_patches |> writeRaster(paste0(results_dir, "/source_large_patches.tif"), overwrite = T)

  # leastcostpath ---------------------------------------

  suitable_cents <- large_suitable_patches |>
    as.polygons(dissolve = TRUE) |>
    st_as_sf() |>
    st_centroid()

  # gopher snake has 268 sites so need to make it smaller
  if (nrow(suitable_cents) > 20) {
    # browser()

    # Compute pairwise distances
    dist_matrix <- as.matrix(st_distance(suitable_cents))

    # Greedy selection to maximize spacing
    select_evenly_spaced <- function(dist_matrix, num_select) {
      selected_idx <- integer(num_select)
      selected_idx[1] <- sample(1:nrow(dist_matrix), 1) # Start with a random point

      2:num_select |> reduce(function(selected, i) {
        remaining_idx <- setdiff(1:nrow(dist_matrix), selected)
        min_distances <- remaining_idx |>
          map_dbl(~ min(dist_matrix[.x, selected]))
        c(selected, remaining_idx[which.max(min_distances)])
      }, .init = selected_idx[1])
    }

    selected_indices <- select_evenly_spaced(dist_matrix, 20)
    selected_points <- suitable_cents[selected_indices, ]


    # ggplot() +
    #   geom_sf(data = suitable_cents, color = "gray", alpha = 0.5) +
    #   geom_sf(data = selected_points, color = "blue", size = 3) +
    #   theme_minimal()
    suitable_cents <- selected_points
  }
  print(sprintf("Suitable habitat centroids = %s", nrow(suitable_cents)))
  suitable_cents |> write_sf(paste0(results_dir, "/suitable_centroids.gpkg"))

  allgrn_conductance <- rast(paste0(results_dir, "/conductance_allgrn.tif"))
  essential_conductance <- rast(paste0(results_dir, "/conductance_essential.tif"))
  print("Calculating leastcostpaths for all greenspaces...")
  lcp_allgreen <- calc_lcp(allgrn_conductance,
    suitable_cents = suitable_cents,
    city_suitability = city_suitability
  )
  print("Calculating leastcostpaths for all only essential greenspaces...")
  lcp_essential <- calc_lcp(essential_conductance,
    suitable_cents = suitable_cents,
    city_suitability = city_suitability
  )

  # Write results ---------------------------------------
  print("Writing results...")

  lcp_allgreen |> writeResults("allgreen", results_dir = results_dir)
  lcp_essential |> writeResults("essential", results_dir = results_dir)
})


# Visualize ---------------------------------------

agg_lcp <- bind_rows(
  lcp_allgreen$lcp_paths |> mutate(greenspace = "all"),
  lcp_essential$lcp_paths |> mutate(greenspace = "essential")
) |>
  mutate(path_length = st_length(geometry)) |>
  filter(path_length > (0 |> set_units("m")))

agg_lcp |> ggplot() +
  geom_boxplot(aes(x = greenspace, y = cost))

agg_lcp |> ggplot() +
  geom_boxplot(aes(x = greenspace, y = path_length))

agg_lcp |>
  group_by(greenspace) |>
  count()

max_dens_val <- max(c(
  values(lcp_allgreen$lcp_density),
  values(lcp_essential$lcp_density)
), na.rm = T)

# LCP Density
all_plt <- ggplot() +
  geom_spatraster(data = lcp_allgreen$lcp_density) +
  geom_sf(data = suitable_cents, color = "white") +
  scale_fill_continuous(limits = c(0, max_dens_val)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank()
  ) +
  labs(title = "All Greenspaces") +
  ylim(37.71, 37.82) +
  xlim(-122.53, -122.37)

ess_plt <- ggplot() +
  geom_spatraster(data = lcp_essential$lcp_density) +
  geom_sf(data = suitable_cents, color = "white") +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  scale_fill_continuous(limits = c(0, max_dens_val)) +
  labs(title = "Essential Only") +
  ylim(37.71, 37.82) +
  xlim(-122.53, -122.37)

p <- all_plt + ess_plt
ggsave(paste0(results_dir, "/fpplot.png"), plot = p)


# Try Omniscape ---------------------------------------
# [Defunct!]
# library(JuliaCall)
# # Initialize Julia and load Circuitscape
# # Okay this doesn't work, need to update JuliaCall eventually
# # see https://github.com/Non-Contradiction/JuliaCall/issues/234

# # install.packages("JuliaCall")
# Sys.setenv(LD_LIBRARY_PATH = "")
# # Sys.setenv(DYLD_LIBRARY_PATH = " ")
# Sys.setenv(LD_PRELOAD = "/home/ahill/.julia/juliaup/julia-1.11.3+0.x64.linux.gnu/lib/julia/libunwind.so.8")
# Sys.setenv(JULIA_NUM_THREADS = 20)
# # julia_setup() # This sets up Julia in R
# julia_setup(rebuild = TRUE, force = TRUE)
# julia_install_package_if_needed("Omniscape")
# julia_library("Omniscape") # Load Circuitscape.jl in Julia
# julia_command("Threads.nthreads()")

# # Essential connectivity
# ini_path_city1 <- create_omniscape_ini(
#   output_path = paste0(results_dir |> normalizePath(), "/essential_only"),
#   resistance_file = paste0(results_dir, "/resistance_essential.tif"),
#   source_file = paste0(results_dir, "/source_large_patches.tif"),
#   ini_path = paste0(results_dir, "/_config_essential.ini"),
#   dispersal_distance_in_meters = 500
# )

# # julia_call("run_omniscape", ini_path_city1)

# # All greenspace connectivity
# ini_path_all <- create_omniscape_ini(
#   output_path = paste0(results_dir |> normalizePath(), "/all_greenspace"),
#   resistance_file = paste0(results_dir, "/resistance_allgrn.tif"),
#   source_file = paste0(results_dir, "/source_large_patches.tif"),
#   ini_path = paste0(results_dir, "/_config_allgreen.ini"),
#   dispersal_distance_in_meters = 500
# )


# # To do it manually
# # julia -t 20
# # using Omniscape
# # run_omniscape("/home/ahill/Projects/cln-urban-connectivity/results/connectivity/2025-02-20_add_1000_to_essential_resistance/Callipepla_californica/_config_allgreen.ini")
# # run_omniscape("/home/ahill/Projects/cln-urban-connectivity/results/connectivity/2025-02-20_add_1000_to_essential_resistance/Callipepla_californica/_config_essential.ini")
