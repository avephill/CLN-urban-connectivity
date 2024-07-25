library(stars)
library(terra)
library(MASS)
library(blockCV)
library(tidyverse)
library(CoordinateCleaner)
library(SDMtune)
library(furrr)
library(RPostgreSQL)
library(vscDebugger)
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

results.dir <- "results/sdm/run-2024-07-24_newvars/"

dir.create(results.dir, recursive = TRUE)

# -------------------------------------------------------------------------

# Predictors --------------------------------------------------------------

# -------------------------------------------------------------------------

bioclim.stack <- rast("data/predictors/bioclim.tif")
# vif.out <- vifstep(bioclim.stack |> as.data.frame(), th=10)
# vif.out@excluded |> paste(collapse="','") |> paste0("c('",.,"')")
vif_excluded <- c(
  "Temperature Annual Range", "Precipitation of Wettest Quarter",
  "Temperature Seasonality", "Precipitation of Coldest Quarter",
  "Mean Temperature of Driest Quarter", "Annual Precipitation",
  "Max Temperature of Warmest Month",
  "Mean Temperature of Warmest Quarter", "Annual Mean Temperature",
  "Min Temperature of Coldest Month", "Precipitation of Wettest Month"
)

bioclim_uncorr.stack <- subset(bioclim.stack, vif_excluded, negate = T)

list.files("data/predictors")
predictor_names <- c(
  "altitude", "slope", "solarradiation",
  "vegetation",
  "nlcd_landcover", "nlcd_treecover",
  "wetland_fresh_distance", "greenspace10acre_distance", "natural-stream-dist",
  "nlcd_impervious", "CES_traffic", "CES_pollution"
  # "ces"
  # "soils_suborder" # incomplete data
)
predictors.sr <-
  rast(list.files("data/predictors", full.names = T) |>
    str_subset(".tif$") |>
    str_subset(paste(predictor_names, collapse = "|"))) |>
  c(bioclim_uncorr.stack) |>
  mask(sdm_train_mask.sf |> st_transform(4326))

# names(predictors.sr) <- names(predictors.sr) |> str_replace_all(" ", "_")

name_change.df <-
  tibble(RAWNAME = names(predictors.sr)) |>
  mutate(DERNAME = case_when(
    RAWNAME == "dem30m_ba_reg" ~ "Elevation",
    RAWNAME == "distance_to_greenspace" ~ "Distance to 10acre Greenspace",
    RAWNAME == "Tree Cover" ~ "NLCD Percent Tree Cover",
    RAWNAME == "prcnt_slope30" ~ "Percent Slope Grade",
    RAWNAME == "aspect" ~ "Slope Aspect",
    RAWNAME == "CLN2_VEGETATION." ~ "Eveg Vegetation Class",
    RAWNAME == "DIST" ~ "Distance to Standing Freshwater",
    RAWNAME == "natural-stream-dist" ~ "Distance to Freshwater Stream",
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
target.layer <- which(name_change.df$DERNAME == "NLCD_Land_Cover_Class")
cat_table <- cats(predictors.sr)[[target.layer]]
names(cat_table) <- cats(predictors.sr)[[target.layer]] |>
  names() |>
  str_replace_all("NLCD Land Cover Class", "NLCD_Land_Cover_Class")

set.cats(predictors.sr,
  layer = target.layer,
  value = cat_table
)


# Write the godforsaken thing
writeRaster(predictors.sr, paste0(results.dir, "predictors.tif"),
  overwrite = T, progress = T
)



# # Look at some correlation
# predictor.df <- predictors.sr  |> as.data.frame()
# predictor.tb <- predictor.df |> tibble() |>
# 	select_if(is.numeric) |>
# 	drop_na()
# cor.test(predictor.tb)
# pred_cor <- cor(predictor.tb, method = c("pearson"))
#
# pred_cor |> write.csv("../results/sdm/predictor_coll.csv")


# -------------------------------------------------------------------------

# Cleaning Occurrence Data ------------------------------------------------

# -------------------------------------------------------------------------


spec.sf <- st_read("data/occurrence/2024-07-16_target_spec.gpkg")

spec_intersect.sf <- spec.sf |>
  st_intersection(sdm_train_mask.sf |> st_transform(st_crs(spec.sf)))

spec_cleaned.sf <-
  spec_intersect.sf |>
  as_tibble() |>
  # Necessary to let the defaults work
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) |> 
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
  rename(decimallongitude = decimalLongitude,
         decimallatitude = decimalLatitude) |> 
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
  )

# Removed point count
nrow(spec_intersect.sf) - nrow(spec_cleaned.sf)

write_sf(spec_cleaned.sf, paste0(results.dir, "spec_obs.gpkg"))

# spec_cleaned.sf <- st_read(paste0(results.dir, "spec_obs.gpkg"))

# Filter to keep only one unique point per raster cell
spec_thinned.sf <- spec_cleaned.sf |>
  # Extract cell numbers for each point
  mutate(cell = cellFromXY(predictors.sr, st_coordinates(spec_cleaned.sf))) |>
  group_by(cell) |>
  # slice(1) |>
  sample_n(1) |> 
  ungroup() |>
  st_as_sf()

nrow(spec_cleaned.sf) - nrow(spec_thinned.sf)

write_sf(spec_thinned.sf, paste0(results.dir, "spec_obs_thinned.gpkg"))


# -------------------------------------------------------------------------

# Background Data ---------------------------------------------------------

# -------------------------------------------------------------------------

# Should I thin points before or after making the bias maps?
# I think before

# Read in predictors and obs from previous sections
predictors.sr <- rast(paste0(results.dir, "predictors.tif"))
spec_thinned.sf <- st_read(paste0(results.dir, "spec_obs_thinned.gpkg"))

# ggplot() +
# geom_sf(data = background_landmask.sf) +
# geom_sf(data = spec_cleaned.sf)


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


plot(point_dens.sr)
plot(background_landmask.sf, add = T)


# Sample randomly using point density
bg.df <- as.data.frame(
  xyFromCell(
    point_dens.sr,
    sample(which(!is.na(values(point_dens.sr))),
      size = 20000, # 20*nrow(presvals),
      prob = values(point_dens.sr)[!is.na(values(point_dens.sr))]
    )
  )
)

write.csv(bg.df, paste0(results.dir, "background_points.csv"))

# -------------------------------------------------------------------------

# Modeling ----------------------------------------------------------------

# -------------------------------------------------------------------------

bg.df <- read.csv(paste0(results.dir, "background_points.csv")) |>
  dplyr::select(x, y)


ggplot() +
  geom_sf(data = sdm_train_mask.sf) +
  geom_sf(data = bg.df |> st_as_sf(coords = c(1, 2), crs = 4326))

spec_thinned.sf <- st_read(paste0(results.dir, "spec_obs_thinned.gpkg"))

species <- spec_thinned.sf$species |> unique()


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

    pos_pts.df <- input_obs |>
      filter(species == spec) |>
      st_coordinates()
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
    # cv_spatial_autocor(x = sp_extract.sf,
    #                    column = "Y")
    # cv_block_size(x = sp_extract.sf, column = "Y")

    scv1 <- cv_spatial(
      x = sp_extract.sf,
      column = "Y", # the response column (binary or multi-class)
      r = predictor.stack[names(sp_extract.sf |>
        data.frame() |>
        dplyr::select(-Y, -geometry))],
      k = n_replicates, # number of folds
      size = 52000, # size of the blocks in metres
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
        "NLCD_Land_Cover_Class",
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

trainSDM(species, spec_thinned.sf)

# For parallel training of multiple species if needed
# safeSDM <- safely(.f = trainSDM)

# plan(multisession, workers = 3)

# system.time({
#   print(future_map(species, safeSDM))
# })