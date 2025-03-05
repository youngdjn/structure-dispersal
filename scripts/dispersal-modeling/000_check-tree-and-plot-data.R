# Exploratory data analysis of the seedling and plot data. 
# Before exporting it for use in Stan dispersal modeling. 
# This script 
# - plots tree and plot data together to check that they're aligned ok
# - creates a tree density raster plus a "distance from tree" raster for all trees and individual species
# - checks whether the density of all seedlings, or seedlings of particular species, are correlated with distance

library(sf)
library(terra)
library(tidyterra)
library(dplyr)
library(ggplot2)
library(here)

#data_dir = readLines(here("data_dir.txt"), n = 1)
data_dir = "/ofo-share" # set directory to base of filetree because plot data & tree data in different folders

tree_data_dir = file.path(data_dir, "str-disp_drone-data-v2/predicted-treecrowns-w-predicted-species")
plot_data_dir = file.path(data_dir, "str-disp_data/regen-plots-standardized")

## Load overstory tree data and convert to centroid
# Also filter out trees < 10m tall
#trees_lassic = st_read(file.path(tree_data_dir, "lassic.geojson"))
trees_valley = st_read(file.path(tree_data_dir, "valley.geojson")) |>
  filter(Z >= 10) |> 
  st_centroid()
trees_delta = st_read(file.path(tree_data_dir, "delta.geojson")) |>
  filter(Z >= 10) |> 
  st_centroid()
trees_chips = st_read(file.path(tree_data_dir, "chips.geojson")) |>
  filter(Z >= 10) |> 
  st_centroid()

# Load seedling plot data
# NOTE: lassic missing from the plot data; crater missing from the tree data 
#trees_crater = st_read(file.path(tree_data_dir, "crater.geojson"))
plots_valley = st_read(file.path(plot_data_dir, "valley.gpkg")) 
plots_delta = st_read(file.path(plot_data_dir, "delta.gpkg"))
plots_chips = st_read(file.path(plot_data_dir, "chips.gpkg")) 

# Reproject plot data to the CRS of the tree data 
plots_valley = st_transform(plots_valley, st_crs(trees_valley)) 
plots_delta = st_transform(plots_delta, st_crs(trees_delta)) 
plots_chips = st_transform(plots_chips, st_crs(trees_chips))

#### Map tree and plot locations to check they look ok ####

# Choose which data to plot 
tree_data = trees_delta %>% 
  dplyr::filter(pred_class_ID == "PIPJ") |>
  dplyr::rename(tree_height = highres_chm_height)

plot_data = plots_delta |>
  mutate(seedling_count = count_PIPJ)

# Make the plot
ggplot() +
  geom_sf(data = tree_data, aes(color = tree_height), size = 0.5) +  # Plot tree points
  geom_sf(data = plot_data, aes(fill = seedling_count), size = 2, shape = 21, color = "black") +  # Plot plot locations
  scale_color_viridis_c(name = "Tree Height") +
  scale_fill_gradient(low = "darkblue", high = "yellow") +
  theme_minimal()

# Note: PIPJ for Delta looks promising -- seemingly a declining pattern from edge, good number of trees (5064), most plots have a seedling
# Decent number of CADE and PSME seedlings, not a lot of ABCOs though. 
dim(tree_data)
dim(plot_data)
sum(plot_data$count_PIPJ>0)
sum(plot_data$count_CADE>0)
sum(plot_data$count_ABCO>0)
sum(plot_data$count_PSME>0)


#### Rasterize tree density and compare to plot data ####
  
tree_data = trees_delta 
plot_data = plots_delta

# Optionally filter y species 
tree_data = tree_data |>
  filter(pred_class_ID %in% c("PIPJ"))

template_rast = rast(tree_data, res = 30, extent = ext(tree_data), nlyrs = 1, vals = 0)
tree_rast = terra::rasterize(tree_data, template_rast, field = "treeID", fun='count')
plot(tree_rast)

# Set cells with no more than a minimum number of trees to zero 
tree_rast = tree_rast |>
  mutate(seedwall = ifelse(count >= 2, 1, NA))

#### Calculate distance from raster cells to trees ####
plot_data = plots_delta
distance_rast = distance(template_rast, tree_data)
plot(distance_rast)

# Test association of seedling counts with distance 
plot_data$distance_to_tree = extract(distance_rast, plot_data, raw = TRUE, ID = FALSE)
ggplot(plot_data, aes(x = distance_to_tree, y = count_PIPJ)) + geom_point() + theme_minimal()

# LOOKS PRETTY GOOD! Maybe longer distance dispersal from PIPJ than from CADE? 