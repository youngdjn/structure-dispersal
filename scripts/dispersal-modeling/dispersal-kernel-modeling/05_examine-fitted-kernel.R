# Get seedling density values from fitted dispersal kernels and visualize them

library(tidyverse)
library(here)
library(mgcv)

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



fitted_2Dt = get_fitted_kernel(dataset_name = "chips-allsp-height-01",
                               disp_mod = "2Dt",
                               err_mod = "pois")

fitted_exppow = get_fitted_kernel(dataset_name = "chips-allsp-height-01",
                                  disp_mod = "exppow",
                                  err_mod = "pois")



fitted_2Dt = get_fitted_kernel(dataset_name = "delta-allsp-height-01",
                               disp_mod = "2Dt",
                               err_mod = "pois")

fitted_exppow = get_fitted_kernel(dataset_name = "delta-allsp-height-01",
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
  labs(x="Distance (m)", y = "Kernel density") +
  coord_cartesian(ylim = c(0, 0.0001),
                  xlim = c(0, 250))

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



dataset_name = "crater-pipj-height-01"
disp_mod = "2Dt"
err_mod = "pois"
plot_size_ha = 0.09113


dataset_name = "delta-allsp-height-01"
disp_mod = "2Dt"
err_mod = "pois"
plot_size_ha = 0.02


###!!!! make this automatically set  the zero value
load_fit_and_plot(dataset_name = dataset_name, disp_mod = disp_mod, err_mod = err_mod, plot_size_ha = plot_size_ha, ylim = c(NA, NA))




#### For distance to nearest

## Load the dataset specified
prepped_data_dir = datadir(paste0("prepped-for-stan/", dataset_name))
overstory_tree_size = read_lines(paste0(prepped_data_dir, "/overstory-tree-size.txt")) %>% as.numeric
seedling_counts = read_lines(paste0(prepped_data_dir, "/seedling-counts.txt")) %>% as.numeric
r = read.table(paste0(prepped_data_dir,"/dist-mat.txt")) %>% as.matrix # rows are plots, columns are trees
colnames(r) = NULL

dist_to_nearest = apply(r, 1, min)
d = data.frame(obs = seedling_counts, dist_to_nearest)
plot(seedling_counts ~ dist_to_nearest, data = d)

m = gam(obs ~ s(dist_to_nearest, k = 3), data = d, method = "REML", family = "poisson")
summary(m)
plot(m)
d$fit = fitted(m, type = "response")

plot_fitted_observed(d, 1, 1, c(NA, NA))





## Gaussian smooth

## Estimate tree BA and then seed output

a = 1.4721
b = 0.6848
dbh = function(h) {
  (h/a)^(1/b)
}

overstory_tree_ba =  (dbh(overstory_tree_size)/2)^2

overstory_tree_fecundity = (0.0107*(  0.1226  ^-0.58)*((113000*( overstory_tree_ba ^0.855))^1.08))

# define gaussian kernel
gaus_kern = function(x) {
  dnorm(x, mean = 0, sd = 45)
}

# apply gaussian kernel
rel_output = gaus_kern(r)

# multiply by fecundity
fecundity_matrix = matrix(rep(overstory_tree_fecundity, times = nrow(rel_output)), nrow = nrow(rel_output), byrow = TRUE)
seed_reaching_plot = fecundity_matrix * rel_output
gaus_seeds = rowSums(seed_reaching_plot)

d$gaus_seeds = gaus_seeds

plot(seedling_counts ~ gaus_seeds, data = d)

m = gam(obs ~ 0 + s(gaus_seeds, k = 3), data = d, method = "REML", family = "poisson")
summary(m)
plot(m)
d$fit = fitted(m, type = "response")

plot(fit ~ obs, data = d)

plot_fitted_observed(d, 1, 1, c(NA, NA))




####### Make a map of predicted density

# Need to adapt the prep-data script so that it works on rasters as well, treating each grid cell as a "plot"

### Make raster of predictions

# For every 30 cell in this landscape, make a "plot", calc mat of distances from trees, predict

library(terra)
site_name = "crater" # move this up higher
boundary = st_read(datadir(paste0("boundaries/", site_name, ".gpkg")))
grid = rast(resolution = 10, ext = ext(boundary) , crs = "EPSG:3310")
values(grid) = 1:ncell(grid)
#grid = mask(grid, boundary)

## make the cells into points
pts = as.points(grid) %>% st_as_sf
coords = st_coordinates(pts)
pts$x = coords[,1]
pts$y = coords[,2]
st_geometry(pts) = NULL

## load the tree coords
trees = st_read(datadir(paste0("ttops-live/", site_name, ".gpkg"))) |> st_transform(3310)
tree_coords = st_coordinates(trees)
trees$x = tree_coords[,1]
trees$y = tree_coords[,2]
tree_data = trees
st_geometry(tree_data) = NULL

# Calculate tree-point distance matrix
d2min <- 0.01

dist_sq <- outer(pts$x, tree_data$x, "-")^2 + outer(pts$y, tree_data$y, "-")^2
dist_sq[dist_sq < d2min] <- d2min

r <- sqrt(dist_sq)









overstory_tree_size = read_lines(paste0(prepped_data_dir, "/overstory-tree-size.txt")) %>% as.numeric


## Load the fitted model and extract the parameter samples
model_filename = paste0(datadir("stan-models/"), "stanmod_", dataset_name,"_",disp_mod, "_", err_mod,".rds")
model_fit = readRDS(model_filename)
samples = rstan::extract(model_fit)

# Summarize across the samples, dropping uncertainty (faster predictions)
samples_median = map(samples,median)
#samples_median= samples

## To run across all plots, need to make a list of tree_plot_dists with one list item per plot, containing the distances to each tree for that plot
tree_dists_by_plot = apply(r,1,FUN=c, simplify=FALSE)

# Make predictions across all plots, but using just the point estimate of each parameter (no uncertainty)
plan(multisession(workers=8))
plot_seedl_preds = future_map_dfr(tree_dists_by_plot, predict_seedl_plot, samples = samples_median, overstory_tree_size = overstory_tree_size)
row.names(plot_seedl_preds) = NULL

pts = bind_cols(pts,plot_seedl_preds)

values(grid) = pts$fit

grid = mask(grid, boundary)

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