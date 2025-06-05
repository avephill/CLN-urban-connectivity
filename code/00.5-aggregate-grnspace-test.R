# This is a test to see how to aggregate the greenspaces by name, and how the
# greenspace area was not correct originally for christine's greenspaces because
# it was broken in pieces
# After running this i fixed it in 00-data prep.R

library(tidyverse)
library(sf)
library(patchwork)

# Let's see how to aggregate Christine's greenspaces and get area right
grnsp <- st_read("data/greenspaces/city_greenspace.gpkg")

# How many greenspaces are named but are in parts?
grnsp_cnt <- grnsp |>
  group_by(GreenSpace) |>
  summarize(n = n())

grnsp_cnt |>
  filter(n > 1) |>
  arrange(desc(n))
grnsp_cnt$n |> hist()

# Group and stitch together the parts
grnsp_agg <- grnsp |>
  group_by(GreenSpace) |>
  summarize(geom = st_union(geom), n = n())

grnsp_agg |> arrange(desc(n))

# Check the visuals
agg_plot <- grnsp_agg |>
  filter(GreenSpace == "Sunset Boulevard") |>
  ggplot() +
  geom_sf()

unagg_plot <- grnsp |>
  filter(GreenSpace == "Sunset Boulevard") |>
  ggplot() +
  geom_sf()

agg_plot + unagg_plot

# Check the area
grnsp_agg |>
  filter(GreenSpace == "Sunset Boulevard") |>
  st_area()

grnsp |>
  filter(GreenSpace == "Sunset Boulevard") |>
  st_area() |>
  sum()

# Ok cool so I should aggregate the greenspaces by name
