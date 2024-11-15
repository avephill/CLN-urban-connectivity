library(leastcostpath)
library(terra)
library(sf)
library(tidyterra)
library(tidyverse)
library(units)
# library(future)

sdm_results_path <- "./results/sdm/run-2024-07-24_newvars/"

source("code/03-helper-functions.R")

# Load the habitat suitability raster using terra
habitat_suitability <- rast(paste0(sdm_results_path, "Taricha_torosa/prediction.tif"))

# newt records
newt_occ <- st_read("data/occurrence/2024-07-16_target_spec.gpkg")

# City boundaries
city_boundaries_prep <- st_read("data/BayAreaCities_CLN.gpkg")

city_boundaries <- city_boundaries_prep |>
  filter(jurname %in% c("Oakland", "Piedmont")) |>
  summarise(jurname = "Oakland_Piedmont", geom = st_union(geom)) |>
  bind_rows(city_boundaries_prep |>
    filter(jurname %in% c("San Jose", "San Francisco")))


# Plot to visualize
plot(habitat_suitability)
plot(city_boundaries, add = T)

city <- "San Francisco"
city_bounds <- city_boundaries |>
  filter(jurname == city)
city_suitability <- habitat_suitability |>
  crop(city_bounds, mask = T) |>
  trim()

city_occ <- newt_occ |>
  st_intersection(city_bounds)


plot(city_suitability)





results_dir <- "./results/connectivity/2024-10-31_circuitscape/"

# New results
dir.create(
  paste0(
    "./results/connectivity/",
    format(Sys.time(), "%Y-%m-%d"),
    "_circuitscape"
  ),
  recursive = T
)

# Cost surface
# Ensure habitat suitability values are between 0 and 1
# If necessary, rescale the raster
city_suitability_rs <- city_suitability #|> rescale01()

# Create cost raster (inverse of suitability)
cost_raster <- 1 - (city_suitability_rs)

# Plot the cost raster
plot(cost_raster, main = "Cost Surface")

cost_raster |> writeRaster(
  paste0(results_dir, "resistance.tif"),
  overwrite = T
)

# Origins and destinations
# suitable_patches <- city_suitability |>
#   filter(mean >= .8) |>
#   patches()

# suitable_patches |> writeRaster(paste0(results_dir, "source.tif"))

# plot(suitable_patches)

# Greenspace ---------------------------------------
# prepare greenspaces
all_greenspace <- st_read("data/greenspaces/original/GreenSpace_NDVI_SF_SJ_OakPied.shp") |>
  st_transform(4326) |>
  st_make_valid()

city_greenspace <- all_greenspace |>
  st_intersection(city_bounds) %>%
  mutate(area = st_area(.)) |>
  filter(area > set_units(100000, "m^2"))


city_greenspace_raster <- rasterize(city_greenspace,
  city_suitability,
  field = "GreenSpace"
)

plot(city_greenspace_raster)

# Need to filter by essential non-essential eventually

city_greenspace_raster |>
  writeRaster(paste0(results_dir, "all_greenspace.tif"),
    overwrite = T
  )


# LCA ---------------------------------------

cost_mat <- create_cs(
  cost_raster,
  neighbours = 16,
  dem = NULL,
  max_slope = NULL,
  exaggeration = FALSE
)

plot(cost_mat)

# Origins and destinations
suitable_patches <- city_suitability |>
  filter(mean >= .7) |>
  patches() |>
  filter(patches >= 25) |>
  as.polygons(dissolve = TRUE) |>
  st_as_sf()

suitable_cents <- suitable_patches |>
  st_make_valid() |>
  mutate(geometry = st_centroid(geometry))

og_dest_occs <- city_occ |> filter(year > 2000)

ggplot() +
  geom_spatraster(data = cost_raster) +
  geom_sf(data = og_dest_occs, fill = NA, color = "red") +
  geom_sf(data = suitable_cents, color = "blue")

# LCC

lcp <- create_FETE_lcps_custom(
  x = cost_mat,
  locations = suitable_cents,
  cost_distance = TRUE
)

lcp_density <- create_lcp_density(city_suitability, lcp, rescale = FALSE)
plot()
plot(lcp_density)

# Try Omniscape ---------------------------------------
library(JuliaCall)
# Initialize Julia and load Circuitscape
# Okay this doesn't work, need to update JuliaCall eventually
# see https://github.com/Non-Contradiction/JuliaCall/issues/234

# install.packages("JuliaCall")
# Sys.setenv(LD_LIBRARY_PATH = " ")
Sys.setenv(JULIA_NUM_THREADS = 20)
julia_setup() # This sets up Julia in R
julia_install_package_if_needed("Omniscape")
julia_library("Omniscape") # Load Circuitscape.jl in Julia
julia_command("Threads.nthreads()")

# I'll do it manually for now
# Example usage
ini_path_city1 <- create_omniscape_ini(
  output_path = results_dir,
  resistance_file = paste0(results_dir, "resistance.tif"),
  source_file = paste0(results_dir, "all_greenspace.tif")
)


julia_call("run_omniscape", ini_path_city1)


# Explore results ---------------------------------------

x <- rast(paste0(results_dir, "results/connectivity/2024-10-31_circuitscape/results/connectivity/2024-10-31_circuitscape/omniscape/cum_currmap.tif"))
plot(x)

x <- rast("results/connectivity/2024-10-31_circuitscape/results/connectivity/2024-10-31_circuitscape/omniscape/cum_currmap.tif")
plot(x)
habsuit <- rast("results/connectivity/2024-10-31_circuitscape/resistance.tif")
plot(habsuit)
grn <- rast("results/connectivity/2024-10-31_circuitscape/all_greenspace.tif")
fp <- rast("results/connectivity/2024-10-31_circuitscape/results/connectivity/2024-10-31_circuitscape/omniscape/flow_potential.tif")
ncp <- rast("results/connectivity/2024-10-31_circuitscape/results/connectivity/2024-10-31_circuitscape/omniscape/normalized_cum_currmap.tif")

x |>
  as_tibble() |>
  ggplot() +
  geom_boxplot(aes(y = cum_currmap))
