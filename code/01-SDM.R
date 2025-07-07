library(stars)
library(terra)
library(MASS)
library(blockCV)
library(tidyverse)
library(CoordinateCleaner)
library(SDMtune)
library(furrr)
library(sf)
# .vsc.listen()

# bioclim_dic <- read_csv("../../../Data/Dictionaries/bioclim_dictionary.csv")
# setwd("~/Projects/together-bay-area/together-bay-area/")

terrestrial.sf <- st_read("~/Data/Boundaries/Political/CLN2.0/CLN_Database_2_0_1/data/Base/vector/terrestrial.shp")
# urban.sf <- st_read("data/urban_area.gpkg") |> rename(geometry = geom)

# sdm_train_mask.sf <- urban.sf # terrestrial.sf
sdm_train_mask.sf <- terrestrial.sf


# results.dir <- paste0("results/sdm/run-",
#                       format(Sys.time(), "%Y-%m-%d"),
#                       "_newvars/")

city_boundaries <- st_read("data/city_boundaries.gpkg")

results.dir <- "results/sdm/run-2025-07-01_fix-greenspace/"

dir.create(results.dir, recursive = TRUE)

# -------------------------------------------------------------------------

# Predictors --------------------------------------------------------------

# -------------------------------------------------------------------------

# bioclim.stack <- rast("data/predictors/bioclim.tif")
# # vif.out <- vifstep(bioclim.stack |> as.data.frame(), th=10)
# # vif.out@excluded |> paste(collapse="','") |> paste0("c('",.,"')")
# vif_excluded <- c(
#   "Temperature Annual Range", "Precipitation of Wettest Quarter",
#   "Temperature Seasonality", "Precipitation of Coldest Quarter",
#   "Mean Temperature of Driest Quarter", "Annual Precipitation",
#   "Max Temperature of Warmest Month",
#   "Mean Temperature of Warmest Quarter", "Annual Mean Temperature",
#   "Min Temperature of Coldest Month", "Precipitation of Wettest Month"
# )

# bioclim_uncorr.stack <- subset(bioclim.stack, vif_excluded, negate = T)

list.files("data/predictors")
predictor_names <- c(
  "altitude", "slope", # "solarradiation",
  "vegetation",
  # "nlcd_landcover",
  "nlcd_reclass_landcover",
  "nlcd_treecover",
  "wetland_fresh_distance", "natural-stream-dist",
  # "2acre_greenspace_distance", "10acre_greenspace_distance",
  # "30acre_greenspace_distance", "75acre_greenspace_distance", "130acre_greenspace_distance",
  "/greenspace_nearest_dist.tif", "/greenspace_nearest_size.tif",
  "nlcd_impervious", "CES_traffic", "CES_pollution"
  # "ces"
  # "soils_suborder" # incomplete data
)


predictors.sr <-
  rast(list.files("data/predictors", full.names = T) |>
    str_subset(".tif$") |>
    str_subset(paste(predictor_names, collapse = "|"))) |>
  # c(bioclim_uncorr.stack) |>
  mask(sdm_train_mask.sf |> st_transform(4326))

# names(predictors.sr) <- names(predictors.sr) |> str_replace_all(" ", "_")

name_change.df <-
  tibble(RAWNAME = names(predictors.sr)) |>
  mutate(DERNAME = case_when(
    RAWNAME == "dem30m_ba_reg" ~ "Elevation",
    # RAWNAME == "greenspace_dist2" ~ "Min Distance to 2acre Greenspace",
    # RAWNAME == "greenspace_dist10" ~ "Min Distance to 10acre Greenspace",
    # RAWNAME == "greenspace_dist30" ~ "Min Distance to 30acre Greenspace",
    # RAWNAME == "greenspace_dist75" ~ "Min Distance to 75acre Greenspace",
    # RAWNAME == "greenspace_dist130" ~ "Min Distance to 130acre Greenspace",
    RAWNAME == "Tree Cover" ~ "NLCD Percent Tree Cover",
    RAWNAME == "prcnt_slope30" ~ "Percent Slope Grade",
    RAWNAME == "aspect" ~ "Slope Aspect",
    RAWNAME == "CLN2_VEGETATION." ~ "Eveg Vegetation Class",
    RAWNAME == "DIST" ~ "Min Distance to Standing Freshwater",
    RAWNAME == "natural-stream-dist" ~ "Min Distance to Freshwater Stream",
    RAWNAME == "greenspace_nearest_dist" ~ "Nearest Greenspace Distance",
    RAWNAME == "greenspace_nearest_size" ~ "Nearest Greenspace Size",
    T ~ RAWNAME
  )) |>
  mutate(DERNAME = str_replace_all(DERNAME, " ", "_"))

# Put new names in predictors
names(predictors.sr) <- name_change.df$DERNAME

# fix categorical layers. Need to go in and fix categorical table
# TODO come back later and clean this up

# Eveg
target.layer <- which(name_change.df$DERNAME == "Eveg_Vegetation_Class")
cat_table <- cats(predictors.sr)[[target.layer]]
names(cat_table) <- cats(predictors.sr)[[target.layer]] |>
  names() |>
  str_replace_all("NAME", "Eveg_Vegetation_Class")

cat_table <- cat_table |> dplyr::select(-CLN2_VEGETATION.)

set.cats(predictors.sr,
  layer = target.layer,
  value = cat_table
)

# Fix NLCD
target.layer <- which(name_change.df$DERNAME == "NLCD_Land_Cover_ReClass")
cat_table <- cats(predictors.sr)[[target.layer]]
names(cat_table) <- cats(predictors.sr)[[target.layer]] |>
  names() |>
  str_replace_all("NLCD Land Cover ReClass", "NLCD_Land_Cover_ReClass")

set.cats(predictors.sr,
  layer = target.layer,
  value = cat_table
)

# # Take a look at collinearity of all predictors
# # Compute the correlation matrix (using cell values)
# cor_matrix <- cor(values(predictors.sr), use = "pairwise.complete.obs")

# # View the correlation matrix
# print(cor_matrix)
# # Computing correlation matrix with p-values
# corrp.mat <- cor_pmat(cor_matrix)
# ggcorrplot::ggcorrplot(cor_matrix)

# Write the godforsaken thing
writeRaster(predictors.sr, paste0(results.dir, "predictors.tif"),
  overwrite = T, progress = T
)



# -------------------------------------------------------------------------

# Cleaning Occurrence Data ------------------------------------------------

# -------------------------------------------------------------------------


spec.sf <- st_read("./data/occurrence/2025-04-02_target_spec.gpkg")

spec.sf |>
  slice_max(eventdate) |>
  pull(eventdate)

spec_intersect.sf <- spec.sf |>
  st_intersection(sdm_train_mask.sf |> st_transform(st_crs(spec.sf)))

# spec_intersect.sf |>
#   filter(species == "Callipepla californica") |>
#   nrow()

spec_cleaned.sf <-
  spec_intersect.sf |>
  as_tibble() |>
  # Necessary to let the defaults work
  rename(
    decimalLongitude = decimallongitude,
    decimalLatitude = decimallatitude
  ) |>
  # Identify Invalid lat/lon Coordinates
  cc_val() |>
  # Identify Records with Identical lat/lon
  cc_equ() |>
  # Identify Coordinates in Vicinity of Country Capitals.
  cc_cap() |>
  # Identify Coordinates in Vicinity of Country and Province Centroids
  cc_cen() |>
  # Identify Coordinates Outside their Reported Country
  # cc_coun(iso3 = "USA")|>
  # Identify Records Assigned to GBIF Headquarters
  cc_gbif() |>
  # Identify Records in the Vicinity of Biodiversity Institutions
  cc_inst() |>
  # Identify Non-terrestrial Coordinates
  # cc_sea() |>
  # Identify Zero Coordinates
  cc_zero() |>
  # Identify Geographic Outliers in Species Distributions
  # cc_outl() |>
  # Identify Duplicated Records
  # cc_dupl()
  rename(
    decimallongitude = decimalLongitude,
    decimallatitude = decimalLatitude
  ) |>
  filter(coordinateuncertaintyinmeters <= 250 | is.na(coordinateuncertaintyinmeters)) |>
  filter(basisofrecord == "HUMAN_OBSERVATION" |
    basisofrecord == "OBSERVATION" |
    basisofrecord == "PRESERVED_SPECIMEN") |>
  filter(individualcount > 0 | is.na(individualcount)) |>
  filter(individualcount < 99 | is.na(individualcount)) |>
  st_as_sf(
    # coords = c("decimallongitude", "decimallatitude"),
    # crs = st_crs(spec.sf),
    sf_column_name = "geom"
  ) |>
  filter(year > 2000)

# spec_cleaned.sf |>
#   filter(species == "Callipepla californica") |>
#   nrow()

# Percent of records removed
100 * (nrow(spec_intersect.sf) - nrow(spec_cleaned.sf)) / nrow(spec_intersect.sf)

write_sf(spec_cleaned.sf, paste0(results.dir, "spec_obs.gpkg"))


# -------------------------------------------------------------------------

# Background Data ---------------------------------------------------------

# -------------------------------------------------------------------------

# Should I thin points before or after making the bias maps?
# I think before

# Read in predictors and obs from previous sections
predictors.sr <- rast(paste0(results.dir, "predictors.tif"))
spec_cleaned.sf <- st_read(paste0(results.dir, "spec_obs.gpkg"))

# THis is thinned for all occurrences, but need to thin later for each species
spec_thinned.sf <- spec_cleaned.sf |>
  # Extract cell numbers for each point
  mutate(cell = cellFromXY(predictors.sr, st_coordinates(spec_cleaned.sf))) |>
  group_by(cell) |>
  # slice(1) |>
  sample_n(1) |>
  ungroup() |>
  st_as_sf()

# write_sf(spec_thinned.sf, paste0(results.dir, "spec_obs_thinned.gpkg"))


# Simplify input polygons and add some buffer zone to it
background_landmask.sf <-
  sdm_train_mask.sf |>
  dplyr::select(geometry) |>
  st_transform(4326)

# Make sampling bias map

empty.sr <- rast(predictors.sr[[1]])
coords <- st_coordinates(spec_thinned.sf)

# Create kernel density map
point.dens <- kde2d(coords[, 1], coords[, 2],
  n = c(nrow(empty.sr), ncol(empty.sr)),
  lims = ext(empty.sr) |> as.vector()
)

# Turn KDE into SpatRaster and mask it to study area
point_dens.df <- expand.grid(x = point.dens$x, y = point.dens$y, KEEP.OUT.ATTRS = FALSE)
point_dens.df$z <- point.dens$z |> as.vector()
point_dens.sr <- rast(point_dens.df) |> mask(background_landmask.sf)

point_dens.sr |> writeRaster(paste0(results.dir, "point_dens.tif"))


plot(point_dens.sr)
plot(background_landmask.sf, add = T)

# We're going to make background points within cities now

# -------------------------------------------------------------------------

# Modeling ----------------------------------------------------------------

# -------------------------------------------------------------------------


# ggplot() +
#   geom_sf(data = sdm_train_mask.sf) +
#   geom_sf(data = bg.df |> st_as_sf(coords = c(1, 2), crs = 4326))

spec_cleaned.sf <- st_read(paste0(results.dir, "spec_obs.gpkg"))
point_dens.sr <- rast(paste0(results.dir, "point_dens.tif"))

# species <- spec_cleaned.sf$species |>
#   unique() |>
#   str_subset("Taricha|Lynx|Microtus", negate = T)

species <- c("Pituophis catenifer", "Callipepla californica", "Lepus californicus")
# species <- "Lepus californicus"


# for(spec in species){
trainSDM <- function(spec, input_obs) {
  # browser()
  print(spec)
  spec_dir <- paste0(results.dir, spec |> str_replace(" ", "_"), "/")

  # See if model exists, if overwrite = F, then skip it
  sdm_fp <- paste0(spec_dir, "sdm_model.rds")
  file.exists(sdm_fp)
  overwrite <- F
  if (!file.exists(sdm_fp) | overwrite == T) {
    # run it

    train_N <- "all"
    n_replicates <- 5

    predictor.stack <- rast(paste0(results.dir, "predictors.tif"))
    names(predictor.stack) <- names(predictor.stack) |> str_replace_all(" ", "_")
    # browser()

    # Crop and mask predictors and input obs to city boundaries
    # Comment this section out if you don't want to crop to city boundaries
    # Why did I do this? I don't think this is right
    # city_name <- case_when(
    #   spec == "Pituophis catenifer" ~ "Oakland_Piedmont",
    #   spec == "Callipepla californica" ~ "San Francisco",
    #   spec == "Lynx rufus" ~ "Oakland_Piedmont",
    #   spec == "Microtus californicus" ~ "Oakland_Piedmont",
    #   spec == "Lepus californicus" ~ "San Jose",
    #   TRUE ~ NA # default case
    # )

    # city_boundary <- city_boundaries |>
    #   filter(jurname == city_name)

    # predictor.stack <- predictor.stack |>
    #   crop(city_boundary) |>
    #   mask(city_boundary)
    # browser()
    # input_obs_backup <- input_obs
    input_obs_spec <- input_obs |> filter(species == spec)


    input_obs_spec_thinned <- input_obs_spec |>
      # Extract cell numbers for each point
      mutate(cell = cellFromXY(predictor.stack, st_coordinates(input_obs_spec))) |>
      group_by(cell) |>
      sample_n(1) |>
      ungroup() |>
      st_as_sf()


    sprintf("Removed %s points while thinning", nrow(input_obs_spec) - nrow(input_obs_spec_thinned))

    # plot(predictor.stack[[1]])
    # input_obs_spec_thinned <- input_obs_spec_thinned |>
    #   st_intersection(city_boundary)

    pos_pts.df <- input_obs_spec_thinned |>
      filter(species == spec) |>
      st_coordinates()

    # ggplot() +
    #   geom_sf(data = city_boundary) +
    #   geom_sf(data = input_obs_spec_thinned |> filter(species == spec))
    # browser()

    region_pointdens.sr <- point_dens.sr #|>
    # crop(city_boundary) |>
    # mask(city_boundary)

    # Sample randomly using point density
    bg.df <- as.data.frame(
      xyFromCell(
        region_pointdens.sr,
        sample(which(!is.na(values(region_pointdens.sr))),
          size = 10000, # 20*nrow(presvals),
          prob = values(region_pointdens.sr)[!is.na(values(region_pointdens.sr))]
        )
      )
    ) |> dplyr::select(x, y)
    ##

    # write.csv(bg.df, paste0(results.dir, "background_points.csv"))
    abs_pts.df <- bg.df

    # OK we want to only have one presence per predictor cell
    # Since these data are at such a small resolution, we can't resample because
    # it'll drown out the presences that are in the predictor cell

    # We want a presence in the predictor size cell if there is just one spec occ within
    # So we're going to convert to 0 (abs), NA (not possible to grow here), and 1 (presence)
    # and then bilinear resample and say every value > 0 is a presence

    # Use all points unless a smaller N is set
    if (train_N == "all") {
      # These points will be used to train the model
      pres_sp.df <- pos_pts.df

      # only do this if there are absence pts to sample from
      if (nrow(abs_pts.df) > 1) {
        abs_sp.df <- abs_pts.df
      }
    } else {
      pres_sp.df <- data.frame(
        pos_pts.df[sample(nrow(pos_pts.df), train_N), ]
      )

      if (length(abs_pts.df) > 1) {
        abs_sp.df <- data.frame(
          abs_pts.df[sample(nrow(abs_pts.df), train_N), ]
        )
        print("if this throws an error then decrease N")
      }
    }

    print(paste0("thinned training presence points: ", nrow(pres_sp.df)))
    if (length(abs_pts.df) > 1) {
      print(paste0("training absence points: ", nrow(abs_sp.df)))
    } else {
      abs_sp.df <- NA
      print("No absence points")
    }

    # Concat pres and absence
    pre.sf <- bind_rows(
      pres_sp.df |> data.frame() |>
        rename(x = X, y = Y) |> mutate(Y = 1),
      abs_sp.df |> tibble() |> mutate(Y = 0)
    )

    block_sp.sf <- st_as_sf(pre.sf, coords = c("x", "y"), crs = crs(predictor.stack))


    # It's important that pre.sf and block_sp.sf are in same order
    sp_extract.df <-
      terra::extract(predictor.stack,
        block_sp.sf |> dplyr::select(-Y),
        df = T, ID = F, xy = T
      ) |>
      mutate(Y = pre.sf$Y) |>
      # This removes like 3000 points, should investigate
      na.omit()

    sp_extract.sf <- sp_extract.df |>
      st_as_sf(coords = c("x", "y"), crs = st_crs(block_sp.sf))

    # browser()

    # Check spatial autocorrelation to inform block size
    # cv_spatial_autocor(
    #   x = sp_extract.sf,
    #   column = "Y"
    # )
    # cv_block_size(x = sp_extract.sf, column = "Y")
    # browser()

    scv1 <- cv_spatial(
      x = sp_extract.sf,
      column = "Y", # the response column (binary or multi-class)
      r = predictor.stack[names(sp_extract.sf |>
        data.frame() |>
        dplyr::select(-Y, -geometry))],
      k = n_replicates, # number of folds
      # size = 5000, # size of the blocks in metres
      selection = "random", # random blocks-to-fold
      iteration = 50, # find evenly dispersed folds
      progress = T, # trun off progress bar
      # biomod2 = TRUE, # also create folds for biomod2
      raster_colors = terrain.colors(10, rev = TRUE) # options from cv_plot for a better colour contrast
    )




    # SDMTune

    # Create SWD object
    data <- prepareSWD(
      species = spec,
      p = sp_extract.sf |> filter(Y == 1) |> st_coordinates(),
      a = sp_extract.sf |> filter(Y == 0) |> st_coordinates(),
      env = predictor.stack,
      categorical = c(
        "NLCD_Land_Cover_ReClass",
        # "soil_suborder",
        "Eveg_Vegetation_Class"
      )
    )


    # Train a Maxnet model
    # but train others if there's an error

    print("Training model...")
    model <- train(method = "Maxnet", data = data, folds = scv1)

    sdm_prediction <-
      predict(model,
        data = predictor.stack,
        type = "cloglog",
        cores = 5
      )
    vi_maxnet <- varImp(model,
      permut = 5
    )


    # Write
    dir.create(spec_dir)
    print(paste0("Saving model at... ", spec_dir, "sdm_model.rds"))
    saveRDS(model, file = paste0(spec_dir, "sdm_model.rds"))
    writeRaster(sdm_prediction, paste0(spec_dir, "prediction.tif"))
    write.csv(vi_maxnet, paste0(spec_dir, "var_imp.csv"))
  } else {
    print("already run")
  }
}

# for (spec in species) {
#   trainSDM(spec, input_obs = spec_cleaned.sf)
# }


# For parallel training of multiple species if needed
safeSDM <- safely(.f = trainSDM)

plan(sequential)


map(species, ~ trainSDM(., input_obs = spec_cleaned.sf))

# map("Callipepla californica", ~ trainSDM(., input_obs = spec_cleaned.sf))
plan(multicore, workers = 3)
system.time({
  future_map(species, ~ safeSDM(., input_obs = spec_cleaned.sf))
})




# Optimize models ---------------------------------------
# Optionally, you can optimize the models using a GA that finds optimal hyperparameters

optimizeModels <- function(spec) {
  # browser()
  spec_dir <- paste0(results.dir, spec |> str_replace(" ", "_"), "/")
  sdm_mod <- readRDS(paste0(spec_dir, "sdm_model.rds"))

  dir.create(paste0(spec_dir, "optimized/"), showWarnings = F)

  # Define the hyperparameters to test
  h <- list(
    reg = seq(0.2, 5, 0.2),
    fc = c("l", "lq", "lh", "lp", "lqp", "lqph")
  )

  # h <- list(
  #   reg = c(.2, .4),
  #   fc = c("l", "lq")
  # )
  library(maxnet)
  opt_mods <- SDMtune::optimizeModel(sdm_mod, metric = "auc", hypers = h)
  opt_mods@results |>
    write_csv(paste0(spec_dir, "optimized/optimization_results.csv"))

  opt_mod <- opt_mods@models[[1]]

  # Make prediction
  predictor.stack <- rast(paste0(results.dir, "predictors.tif"))
  names(predictor.stack) <- names(predictor.stack) |> str_replace_all(" ", "_")

  # Crop and mask predictors and input obs to city boundaries
  # Comment this section out if you don't want to crop to city boundaries
  city_name <- case_when(
    spec == "Pituophis catenifer" ~ "Oakland_Piedmont",
    spec == "Callipepla californica" ~ "San Francisco",
    spec == "Lynx rufus" ~ "Oakland_Piedmont",
    spec == "Microtus californicus" ~ "Oakland_Piedmont",
    spec == "Lepus californicus" ~ "San Jose",
    TRUE ~ NA # default case
  )

  # city_boundary <- city_boundaries |>
  #   filter(jurname == city_name)

  # predictor.stack <- predictor.stack |>
  #   crop(city_boundary) |>
  #   mask(city_boundary)

  # Make prediction

  sdm_prediction <-
    predict(opt_mod,
      data = predictor.stack,
      type = "cloglog",
      cores = 5
    )
  vi_maxnet <- varImp(opt_mod,
    permut = 5
  )

  # Save the optimized model
  saveRDS(opt_mod, paste0(spec_dir, "optimized/sdm_model.rds"))
  writeRaster(sdm_prediction, paste0(spec_dir, "optimized/prediction.tif"))
  write.csv(vi_maxnet, paste0(spec_dir, "optimized/var_imp.csv"))
}

# species <- c("Callipepla californica", "Lepus californicus")
species <- c("Pituophis catenifer", "Callipepla californica", "Lepus californicus")

# for (spec in species) {
#   optimizeModels(spec)
# }

# For parallel training of multiple species if needed
safeOptimize <- safely(.f = optimizeModels)

# plan(sequential)
plan(multicore, workers = 3)
# plan(sequential))

system.time({
  # future_map(species, safeOptimize, seed = T)
  future_map(species, optimizeModels)
})
