# Get seedling density values from fitted dispersal kernels and visualize them

library(tidyverse)
library(here)

data_dir = readLines(here("data_dir.txt"), n=1)
data_dir = file.path(data_dir, "cross-site/")

## Convenience functions ####
source(here("scripts/convenience_functions.R"))
# ^ This defines the function 'datadir', which takes a string argument and prepends it with the path to the data directory.
#   It allows you to make all the paths in the script be relative to the data directory.

# The main functions used by this script. For parameter definitions, see this file.
source(here("scripts/dispersal-modeling/dispersal-kernel-modeling/05_examine-fitted-kernel_functions.R"))


### Compute summarized (median and credible interval) dispersal kernels for the fitted models ###
# These functions return a list of two data frames: one called "kernel" which is the fitted dispersal kernel value at a range of distances
# And the other called "shadow" which is the seed shadow from a tree of an average size for the site (combines kernel and fecundity)

fitted_2Dt = get_fitted_kernel(dataset_name = "valley-allsp-height-01",
                                      disp_mod = "2Dt",
                                      err_mod = "pois")

fitted_exppow = get_fitted_kernel(dataset_name = "valley-allsp-height-01",
                                          disp_mod = "exppow",
                                          err_mod = "pois")

## Combine them so they can be plotted together
kern_summary_comb = bind_rows(fitted_2Dt$kernel, fitted_exppow$kernel)

## Plot them together
ggplot(data = kern_summary_comb, aes(x = r, y = fit, color=disp_mod, fill=disp_mod)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha=0.3, color=NA) +
  geom_line(linewidth=1) +
  theme_bw(20) +
  scale_color_viridis_d(begin=0.3,end=0.7, name="Kernel") +
  scale_fill_viridis_d(begin=0.3,end=0.7, name="Kernel") +
  labs(x="Distance (m)", y = "Kernel density")

ggsave(datadir("temp/kern.png"), width=8, height=5)




# 
# #### Get predictions (fits) for each field plot ####
# 
# ## Choose the tree/seedling dataset that was fitted, the dispersal model, and the count error model
# dataset_name = "crater-pipj-height-01"
# disp_mod = "2Dt"
# err_mod = "pois"
# 
# ## Load the dataset specified
# prepped_data_dir = datadir(paste0("prepped-for-stan/", dataset_name))
# overstory_tree_size = read_lines(paste0(prepped_data_dir, "/overstory-tree-size.txt")) %>% as.numeric
# seedling_counts = read_lines(paste0(prepped_data_dir, "/seedling-counts.txt")) %>% as.numeric
# r = read.table(paste0(prepped_data_dir,"/dist-mat.txt")) %>% as.matrix # rows are plots, columns are trees
# colnames(r) = NULL
# 
# ## Load the fitted model and extract the parameter samples
# model_filename = paste0(datadir("stan-models/"), "stanmod_", dataset_name,"_",disp_mod, "_", err_mod,".rds")
# model_fit = readRDS(model_filename)
# samples = extract(model_fit)
# 
# 
# ## First as a demo, get pred for a single plot
# plot_index = 10
# tree_plot_dists = r[plot_index, ]
# seedl_at_plot = predict_seedl_plot(samples = samples, tree_plot_dists = tree_plot_dists, overstory_tree_size = overstory_tree_size)
# 
# # Repeat, but using only the median estimate for each param so it's faster
# samples_median = map(samples,median)
# seedl_at_plot = predict_seedl_plot(samples = samples_median, tree_plot_dists = tree_plot_dists, overstory_tree_size = overstory_tree_size)
# 
# 
# ## To run across all plots, need to make a list of tree_plot_dists with one list item per plot, containing the distances to each tree for that plot
# tree_dists_by_plot = apply(r,1,FUN=c, simplify=FALSE)
# 
# ## Make predictions across all plots, including uncertainty (i.e. make a prediction for each model parameter sample)
# plan(multisession(workers=8))
# plot_seedl_preds = future_map_dfr(tree_dists_by_plot, predict_seedl_plot, samples = samples, overstory_tree_size = overstory_tree_size)
# row.names(plot_seedl_preds) = NULL
# 
# # Run it again faster by using just the point estimate of each parameter
# plan(multisession(workers=8))
# plot_seedl_preds = future_map_dfr(tree_dists_by_plot, predict_seedl_plot, samples = samples_median, overstory_tree_size = overstory_tree_size)
# row.names(plot_seedl_preds) = NULL
# 
# 
# ### Compare to observed seedling data
# 
# ## Combine with observed seedl
# fitted_observed_plot_seedl = bind_cols(obs = seedling_counts,plot_seedl_preds)
# 
# plot_size_ha = 0.09
# 
# plot_fitted_observed(fitted_observed_plot_seedl, plot_size_ha = plot_size_ha)
# 
# 
# 
# 



dataset_name = "chips-allsp-height-01"
disp_mod = "2Dt"
err_mod = "pois"
plot_size_ha = 0.0113
zero_value = 50

load_fit_and_plot(dataset_name = dataset_name, disp_mod = disp_mod, err_mod = err_mod, plot_size_ha = plot_size_ha, zero_value = zero_value, ylim = c(NA, NA))













ggsave(datadir("temp/fit_obs_exppow.png"), width=6,height=5)


## compare against nearest seed tree, gaussian smooth











#####!!!! This is old broken code that needs to be updated.
# Need to adapt the prep-data script so that it works on rasters as well, treating each grid cell as a "plot"

### Make raster of predictions

# For every 30 cell in this landscape, make a "plot", calc mat of distances from trees, predict

library(terra)

# make a "negative buffer" of the focal area
foc = st_read(datadir("temp/plots_buff2.gpkg"))
bbox = st_bbox(foc) %>% st_as_sfc
bbox = st_buffer(bbox,300)
foc_inv = st_difference(bbox,foc)
st_write(foc_inv,datadir("temp/foc_inv.gpkg"), delete_dsn = TRUE)

grid = rast(resolution = 30, ext = ext(foc) , crs = "EPSG:26911")
values(grid) = 1:ncell(grid)

## make the cells into points
pts = as.points(grid) %>% st_as_sf
coords = st_coordinates(pts)
pts$x = coords[,1]
pts$y = coords[,2]
st_geometry(pts) = NULL


# Calculate tree-point distance matrix
d2min <- 0.01

dist_sq <- outer(pts$x, tree_data$x, "-")^2 + outer(pts$y, tree_data$y, "-")^2
dist_sq[dist_sq < d2min] <- d2min

r <- sqrt(dist_sq)

plan(multisession, workers=8)

plot_seedl_preds = future_map_dfr(1:nrow(pts), predict_seedl_plot)
row.names(plot_seedl_preds) = NULL

pts = bind_cols(pts,plot_seedl_preds)

values(grid) = pts$predicted_seedl_fit

writeRaster(grid,datadir("temp/pred_seedl_rast.tif"), overwrite=TRUE)

# ## just for cartography/viz: buffer of within 10 m of a tree
# 
# st_write(ttops %>% filter(height > 10), datadir("temp/ttops_foc.gpkg"), delete_dsn=TRUE)
# 
# tree_buff = st_buffer(ttops,30) %>% st_union
# tree_buff = nngeo::st_remove_holes(tree_buff)
# tree_buff = st_buffer(tree_buff,-10)
# st_write(tree_buff,datadir("temp/tree_buff.gpkg"), delete_dsn=TRUE)
# 
# ## compare BA to GNN layer





#### Other potentially useful snippets

## Load the dataset for the specified model (tree size)
seedling_plot_area = read_file(paste0(prepped_data_dir,"/plot-area.txt")) %>% as.numeric
r = read.table(paste0(prepped_data_dir,"/dist-mat.txt")) %>% as.matrix
colnames(r) = NULL
overstory_tree_size = read_lines(paste0(prepped_data_dir, "/overstory-tree-size.txt")) %>% as.numeric
seedling_counts = read_lines(paste0(prepped_data_dir, "/seedling-counts.txt")) %>% as.numeric