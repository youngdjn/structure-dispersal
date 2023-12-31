# Make maps of observed data (overstory trees and regen plots) at each site
# NOTE: The file paths here are broken since we split out the drone data from the other field data;
# will need to be updated to run again

library(tidyverse)
library(here)
library(sf)
library(terra)
library(tidyterra)
library(ggnewscale)
library(ggspatial)

data_dir = readLines(here("data_dir.txt"), n=1)
data_dir = file.path(data_dir, "cross-site/")

site = "valley"

overstory_tree_filepath = paste0("ttops-live/", site, ".gpkg")
seedling_plot_filepath = paste0("regen-plots-standardized/", site, ".gpkg")
boundary_filepath = paste0("boundaries/", site, ".gpkg")
ortho_filepath = paste0("orthos-crop-agg/", site, ".tif")
map_notrees_out_filepath = paste0("figures/maps/", site, "_notrees.png")
map_trees_out_filepath = paste0("figures/maps/", site, "_trees.png")

plots = st_read(file.path(data_dir,seedling_plot_filepath))
trees = st_read(file.path(data_dir, overstory_tree_filepath))
boundary = st_read(file.path(data_dir, boundary_filepath))
ortho = rast(file.path(data_dir, ortho_filepath))
plotRGB(ortho)
plot(trees)


## need to set the zero seedling counts to nonzero for log scale

# get the smallest nonzero number
min = min(plots$observed_count[plots$observed_count > 0])
zero_val = min/2
plots = plots |>
  mutate(observed_count_nonzero = ifelse(observed_count == 0, zero_val, observed_count))

p = ggplot() +
  geom_spatraster_rgb(data = ortho) +
  geom_sf(data = plots, aes(color = observed_count_nonzero)) +
  scale_color_viridis_c(option = "magma", trans = "log", breaks = c(1, 2, 5, 10, 20, 50, 100), name = "Seedling\ncount") +
  annotation_scale(pad_x = unit(0.0,"cm"),
                   pad_y = unit(0.5,"cm"), location = "tr",text_cex = 1) +
  theme_void()

png(file.path(data_dir, map_notrees_out_filepath), width = 3000, height = 2000, res = 300, bg = "transparent")
p
dev.off()

p = p +
  new_scale_color() +
  geom_sf(data = trees, aes(color = Z), size = 0.3) +
  scale_color_gradient(low = "white", high = "chartreuse3", name = "Tree\nheight") +
  new_scale_color() +
  geom_sf(data = plots, aes(color = observed_count_nonzero)) +
  scale_color_viridis_c(option = "magma", trans = "log", breaks = c(1, 2, 5, 10, 20, 50, 100), name = "Seedling\ncount")


png(file.path(data_dir, map_trees_out_filepath), width = 3000, height = 2000, res = 300, bg = "transparent")
p
dev.off()
