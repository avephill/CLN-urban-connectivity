rescale01 <- function(x) {
  val <- values(x)

  values(x) <- (val - min(val, na.rm = TRUE)) / (max(val, na.rm = TRUE) - min(val, na.rm = TRUE))

  x
}



# create_FETE_lcps_custom <- function(x, locations, cost_distance = FALSE, ncores = 1) {
#   check_locations(x, locations)

#   loc_vect <- inherits(locations, "SpatVector")

#   myCluster <- parallel::makeCluster(ncores)
#   doParallel::registerDoParallel(myCluster)

#   nlocs <- nrow(locations)

#   if (loc_vect) {
#     locations <- sf::st_as_sf(locations)
#   }

#   lcp_network <- foreach::foreach(
#     i = 1:nlocs, .errorhandling = "remove",
#     .combine = "rbind", .packages = c("sf", "terra")
#     # ) %dopar% {
#   ) %do% {
#     lcp <- create_lcp(
#       x = x,
#       origin = locations[i, , drop = FALSE],
#       destination = locations[-i, , drop = FALSE],
#       cost_distance = cost_distance
#     )

#     lcp$origin_ID <- i
#     lcp$destination_ID <- (1:nlocs)[-i]

#     return(lcp)
#   }

#   parallel::stopCluster(myCluster)

#   lcp_network <- lcp_network[!is.na(sf::st_is_valid(lcp_network)), ]

#   empty_lcps <- sf::st_is_empty(lcp_network)

#   lcp_network <- lcp_network[!empty_lcps, ]
#   lcp_network <- lcp_network[order(lcp_network$origin_ID), ]
#   rownames(lcp_network) <- 1:nrow(lcp_network)

#   if ((nlocs * nlocs - nlocs) - nrow(lcp_network) != 0) {
#     message((nlocs * nlocs - nlocs) - nrow(lcp_network), " least-cost paths could not be calculated due to duplicate locations.")
#   }

#   if (sum(empty_lcps) != 0) {
#     message(sum(empty_lcps), " least-cost paths could not calculated due to being unreachable. If so, check via check_locations()")
#   }

#   if (loc_vect) {
#     lcp_network <- terra::vect(lcp_network)
#   }

#   return(lcp_network)
# }


# Omniscape ini file ---------------------------------------
create_omniscape_ini <- function(output_path, resistance_file, source_file, ini_path, dispersal_distance_in_meters) {
  norm_output_path <- output_path |>
    normalizePath() |>
    paste0("/")
  write_path <- output_path |> normalizePath()
  # paste0(format(Sys.time(), "%Y-%m-%d_%H"), "_omniscape")
  # browser()
  # write_path |> dir.create(recursive = T)

  # Divide by 30 because each grid cell is ~30m
  dispersal_radius <- round(dispersal_distance_in_meters / 30, 0)

  # write_fullpath <- write_path |>
  # normalizePath()

  ini_content <- glue(sprintf(
    "
[Required]
resistance_file = %s
radius = %s
block_size = 1
project_name = %s
source_file = %s


[General options]
source_from_resistance = false
r_cutoff = 50 # Not used if source_from_resistance is false
calc_normalized_current = true
calc_flow_potential = true

parallelize = true
parallel_batch_size = 20

[Output options]
write_raw_currmap = true
  ",
    resistance_file |> normalizePath(),
    dispersal_radius,
    write_path, # project_name
    source_file |> normalizePath()
  ))
  # radius in pixels!
  # browser()

  # Write the content to an .ini file
  # ini_path <- paste0(norm_output_path, ini_name)
  writeLines(ini_content, ini_path)
  return(ini_path)
}
