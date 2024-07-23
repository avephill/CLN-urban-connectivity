library(stars)


results.dir <- "results/sdm/run-2024-07-16_firstshot/"
x <- read_stars(paste0(results.dir, "Taricha_torosa/prediction.tif"))
plot(x)
