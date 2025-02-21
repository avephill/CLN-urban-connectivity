library(leastcostpath)
library(terra)
library(sf)
library(tidyterra)
library(tidyverse)
library(units)
library(glue)
# library(future)
library(vscDebugger)



source("code/03-helper-functions.R")

# Load the habitat suitability raster using terra
spec <- "Callipepla_californica"
sdm_results_path <- paste0("results/sdm/run-2025-01-21_new-species/", spec)
habitat_suitability <- rast(paste0(sdm_results_path, "/prediction.tif"))

# spec records
# all_occ <- st_read("data/occurrence/2025-01-21_target_spec.gpkg")
# spec_occ <- all_occ |> filter(species == spec)

# City boundaries
city_boundaries_prep <- st_read("data/BayAreaCities_CLN.gpkg")

city_boundaries_sf <- city_boundaries_prep |>
  filter(jurname == "San Francisco") |>
  st_cast("POLYGON") |>
  # rowwise() |>
  mutate(new_area = st_area(geom)) |>
  slice_max(order_by = new_area)

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


# Plot to visualize
# plot(habitat_suitability)
# plot(city_boundaries, add = T)

city <- "San Francisco"
city_bounds <- city_boundaries |>
  filter(jurname == city)

# city_occ <- newt_occ |>
#   st_intersection(city_bounds)


# plot(city_suitability)

# New results
results_dir <- paste0(
  "./results/connectivity/",
  format(Sys.time(), "%Y-%m-%d"),
  "_conductance_lcp_boostall/",
  spec
)
dir.create(
  results_dir,
  recursive = T
)
# results_dir <- sprintf("results/connectivity/2025-02-19_newspec-sf/%s", spec)


# # # # # #
# Thresholds
library(SDMtune)
library(maxnet)
# library(glmnet)
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


# Cost surface


# Define the function for reclassification
cost_function <- function(HSI, threshold) {
  # Use ifelse for vectorized operations
  ifelse(
    HSI > threshold, 1, # First condition: optimal habitat
    exp(log(0.001) / threshold * HSI) * 10^4 # Second condition: non-habitat/matrix
  )
}

# Apply the function to essential greenspace prediction
essential_prediction <- rast(paste0(sdm_results_path, "/grncity_essential_prediction.tif"))
essential_resistance <- terra::app(essential_prediction,
  fun = function(x) cost_function(x, maxtss_thresh)
) |>
  crop(city_bounds, mask = T) |>
  trim()
# essential_resistance <- essential_resistance |>
#   mutate(lyr.1 = lyr.1 + 8000)

# Create resistance for all greenspace prediction
city_allgrn_prediction <- rast(paste0(sdm_results_path, "/grncity_all_prediction.tif"))
allgrn_resistance <- terra::app(city_allgrn_prediction,
  fun = function(x) cost_function(x, maxtss_thresh)
) |>
  crop(city_bounds, mask = T) |>
  trim()



# Plot the cost raster
plot(allgrn_resistance, main = "Cost Surface all greenspace")
plot(essential_resistance, main = "Cost Surface essential greenspace")

# Write them
allgrn_resistance |>
  writeRaster(
    paste0(results_dir, "/resistance_allgrn.tif"),
    overwrite = T
  )

city_allgrn_prediction |>
  crop(city_bounds, mask = T) |>
  trim() |>
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
  writeRaster(
    paste0(results_dir, "/conductance_essential.tif"),
    overwrite = T
  )

## Origins and destinations ---------------------------------------
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

plot(large_suitable_patches)

large_suitable_patches |> writeRaster(paste0(results_dir, "/source_large_patches.tif"), overwrite = T)

# gDistance ---------------------------------------
# library(gdistance)
# library(raster)
# library(furrr)

# suitable_cents <- large_suitable_patches |>
#   as.polygons(dissolve = TRUE) |>
#   st_as_sf() |>
#   st_centroid()

# conductance <- raster(paste0(results_dir, "/conductance_essential.tif"))

# tr <- transition(conductance, transitionFunction = mean, directions = 8)
# # Apply geoCorrection to account for diagonal movement
# tr <- geoCorrection(tr, type = "c")

# # Compute least-cost paths using future_map for parallelization
# lcp_paths <- map(1:nrow(suitable_cents), function(i) {
#   browser()
#   origin <- suitable_cents[i, ] |> as("Spatial")
#   destinations <- suitable_cents[-i, ] |> as("Spatial") # Exclude origin itself

#   # Compute cost distance from origin to all destinations
#   cost_dist <- costDistance(tr, origin, destinations)

#   # Compute least-cost paths
#   lcp <- shortestPath(tr, origin, destinations, output = "SpatialLines")

#   # Extract total costs from the raster along the paths
#   total_costs <- extract(resistance, lcp, fun = sum, na.rm = TRUE)

#   # Create a data frame for storing results
#   df <- data.frame(
#     origin_id = i,
#     destination_id = setdiff(1:nrow(suitable_cents), i),
#     cost_distance = as.vector(cost_dist),
#     total_cost = total_costs
#   )

#   return(df)
# }) |> bind_rows()

# # Switch back to sequential processing
# plan(sequential)


# LCA ---------------------------------------
library(furrr)
library(patchwork)

suitable_cents <- large_suitable_patches |>
  as.polygons(dissolve = TRUE) |>
  st_as_sf() |>
  st_centroid()

# All Green

calc_lcp <- function(conductance) {
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

# calc_lcp <- function(conductance) {
#   # browser()
#   cost_mat <- create_cs(
#     conductance,
#     neighbours = 16,
#     dem = NULL,
#     max_slope = NULL,
#     exaggeration = FALSE
#   )

#   plan(multicore, workers = 10)
#   lcp_paths <- future_map(1:nrow(suitable_cents), function(i) {
#     map(1:nrow(suitable_cents), function(j) {
#       if (i != j) {
#         print(paste(i, j))
#         create_lcp(
#           x = cost_mat,
#           origin = suitable_cents[i, ],
#           destination = suitable_cents[j, ],
#           cost_distance = TRUE,
#           check_locations = FALSE
#         )
#       } else {
#         NULL
#       }
#     }) |>
#       compact() |>
#       bind_rows()
#   }) |> bind_rows()
#   plan(sequential)

#   print("Calculating density...")
#   lcp_density <- create_lcp_density(city_suitability, lcp_paths, rescale = FALSE)

#   return(list(lcp_density = lcp_density, lcp_paths = lcp_paths, cost_mat = cost_mat))
# }

allgrn_conductance <- rast(paste0(results_dir, "/conductance_allgrn.tif"))
essential_conductance <- rast(paste0(results_dir, "/conductance_essential.tif"))
lcp_allgreen <- calc_lcp(allgrn_conductance |> mutate(mean = (.5 + mean)^2))
lcp_essential <- calc_lcp(essential_conductance)


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

# max_mat_val <- max(c(
#   values(lcp_allgreen$cost_mat),
#   values(lcp_essential$cost_mat)
# ), na.rm = T)
# # Cost Matrix
# allcost_plt <- ggplot() +
#   geom_spatraster(data = lcp_allgreen$cost_mat) +
#   scale_fill_continuous(limits = c(0, 150)) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.text = element_blank()
#   ) +
#   labs(title = "All Greenspaces") +
#   ylim(37.71, 37.82) +
#   xlim(-122.53, -122.37)

# esscost_plt <- ggplot() +
#   geom_spatraster(data = lcp_essential$cost_mat) +
#   theme_minimal() +
#   theme(axis.text = element_blank()) +
#   scale_fill_continuous(limits = c(0, 150)) +
#   labs(title = "Essential Only") +
#   ylim(37.71, 37.82) +
#   xlim(-122.53, -122.37)

# plot(lcp_allgreen$cost_mat) + plot(lcp_essential$cost_mat)

# Try Omniscape ---------------------------------------
library(JuliaCall)
# Initialize Julia and load Circuitscape
# Okay this doesn't work, need to update JuliaCall eventually
# see https://github.com/Non-Contradiction/JuliaCall/issues/234

# install.packages("JuliaCall")
Sys.setenv(LD_LIBRARY_PATH = "")
# Sys.setenv(DYLD_LIBRARY_PATH = " ")
Sys.setenv(LD_PRELOAD = "/home/ahill/.julia/juliaup/julia-1.11.3+0.x64.linux.gnu/lib/julia/libunwind.so.8")
Sys.setenv(JULIA_NUM_THREADS = 20)
# julia_setup() # This sets up Julia in R
julia_setup(rebuild = TRUE, force = TRUE)
julia_install_package_if_needed("Omniscape")
julia_library("Omniscape") # Load Circuitscape.jl in Julia
julia_command("Threads.nthreads()")

# Essential connectivity
ini_path_city1 <- create_omniscape_ini(
  output_path = paste0(results_dir |> normalizePath(), "/essential_only"),
  resistance_file = paste0(results_dir, "/resistance_essential.tif"),
  source_file = paste0(results_dir, "/source_large_patches.tif"),
  ini_path = paste0(results_dir, "/_config_essential.ini"),
  dispersal_distance_in_meters = 500
)

# julia_call("run_omniscape", ini_path_city1)

# All greenspace connectivity
ini_path_all <- create_omniscape_ini(
  output_path = paste0(results_dir |> normalizePath(), "/all_greenspace"),
  resistance_file = paste0(results_dir, "/resistance_allgrn.tif"),
  source_file = paste0(results_dir, "/source_large_patches.tif"),
  ini_path = paste0(results_dir, "/_config_allgreen.ini"),
  dispersal_distance_in_meters = 500
)


# To do it manually
# julia -t 20
# using Omniscape
# run_omniscape("/home/ahill/Projects/cln-urban-connectivity/results/connectivity/2025-02-20_add_1000_to_essential_resistance/Callipepla_californica/_config_allgreen.ini")
# run_omniscape("/home/ahill/Projects/cln-urban-connectivity/results/connectivity/2025-02-20_add_1000_to_essential_resistance/Callipepla_californica/_config_essential.ini")


rast(paste0(results_dir, "resistance_essential.tif")) |>
  values() |>
  mean(na.rm = T)

rast(paste0(results_dir, "resistance_allgrn.tif")) |>
  values() |>
  mean(na.rm = T)
