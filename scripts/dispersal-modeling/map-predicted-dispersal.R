# Make maps of dispersal from model-fitted dispersal kernels at each site

library(tidyverse)
library(here)
library(sf)
library(terra)
library(tidyterra)
library(ggnewscale)
library(ggspatial)

data_dir = readLines(here("data_dir.txt"), n=1)
data_dir = file.path(data_dir, "cross-site")

site = "valley"

overstory_tree_filepath = paste0("ttops-live/", site, ".gpkg")
seedling_plot_filepath = paste0("regen-plots-standardized/", site, ".gpkg")
boundary_filepath = paste0("boundaries/", site, ".gpkg")
ortho_filepath = paste0("orthos-crop-agg/", site, ".tif")

pred_kernel = paste0("regen-prediction-maps/", site, "_kernel.tif")
pred_gaus = paste0("regen-prediction-maps/", site, "_gaus-fixed.tif")
pred_nearest = paste0("regen-prediction-maps/", site, "_min-dist.tif")

map_kernel_out_filepath = paste0("figures/dispersal-maps/", site, "_kernel.png")
map_nearest_out_filepath = paste0("figures/dispersal-maps/", site, "_nearest.png")
map_gaus_out_filepath = paste0("figures/disperal-maps/", site, "_gaus.png")

plots = st_read(file.path(data_dir,seedling_plot_filepath))
trees = st_read(file.path(data_dir, overstory_tree_filepath))
boundary = st_read(file.path(data_dir, boundary_filepath))
ortho = rast(file.path(data_dir, ortho_filepath))
kernel = rast(file.path(data_dir, pred_kernel))
gaus = rast(file.path(data_dir, pred_gaus))
nearest = rast(file.path(data_dir, pred_nearest))
nearest = -nearest


make_disp_map = function(model_type) {
  
  disp_rast = get(model_type)
  
  disp_rast = disagg(disp_rast, fact = 2, method = "bilinear")
  disp_rast = disagg(disp_rast, fact = 2, method = "bilinear")
  disp_rast = disagg(disp_rast, fact = 2, method = "bilinear")
  
  # get a mask to put behind the trees to focus on high-severity area
  treed_area = trees |> st_transform(3310) |> st_buffer(40) |> st_union() |> st_buffer(-20)
  
  # cap the dispersal color scale to the 50th percentile
  cap = stats::quantile(values(disp_rast), .8, na.rm = TRUE)
  disp_rast[disp_rast > cap] = cap
  disp_rast = mask(disp_rast, boundary)
  
  p = ggplot() +
    geom_spatraster(data = disp_rast) +
    scale_fill_viridis_c(na.value=NA, guide = FALSE) +
    new_scale_fill() +
    geom_sf(data = treed_area, fill = "sienna") +
    geom_sf(data = trees, aes(fill = Z), color = "black", size = 2, pch = 21) +
    scale_fill_gradient(low = "white", high = "chartreuse3", name = "Tree\nheight", guide = FALSE) +
    #geom_sf(data = plots, aes(color = observed_count_nonzero)) +
    annotation_scale(pad_x = unit(0.0,"cm"),
                     pad_y = unit(0.5,"cm"), location = "tr",text_cex = 1) +
    theme_void()
  print(p)
  
  map_out_filepath = paste0("figures/dispersal-maps/", site, "_", model_type, ".png")

  png(file.path(data_dir, map_out_filepath), width = 3000, height = 2000, res = 300, bg = "transparent")
  print(p)
  dev.off()
}


make_disp_map(model_type = "kernel")
make_disp_map(model_type = "gaus")
make_disp_map(model_type = "nearest")
