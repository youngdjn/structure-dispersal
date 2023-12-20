# Get seedling density values from fitted dispersal kernels and visualize them

library(tidyverse)
library(here)
library(mgcv)
library(terra)
library(rstan)

#data_dir = readLines(here("data_dir.txt"), n=1)
# for Andrew's local work
data_dir = "/Users/latimer/Library/CloudStorage/Box-Box/str-disp_data"


# The main functions used by this script. For parameter definitions, see this file.
source(here("scripts/dispersal-modeling/dispersal-kernel-modeling/05_examine-fitted-kernel_functions.R"))


### Compute summarized (median and credible interval) dispersal kernels for the fitted models ###
# These functions return a list of two data frames: one called "kernel" which is the fitted dispersal kernel value at a range of distances
# And the other called "shadow" which is the seed shadow from a tree of an average size for the site (combines kernel and fecundity)


site_name = "chips"
species = "allsp"
plot_size_ha = 0.0113  # 0.09 for crater, 0.0113 for Chips, 0.0201 for others

fitted_2Dt_height = get_fitted_kernel(dataset_name = paste0(site_name, "-", species, "-height-01"),
                                      disp_mod = "2Dt",
                                      err_mod = "pois", 
                                     fecund_mod = "multiplier_exponent")

fitted_exppow_height = get_fitted_kernel(dataset_name = paste0(site_name, "-", species, "-height-01"),
                                          disp_mod = "exppow",
                                          err_mod = "pois", 
                                          fecund_mod = "multiplier")
fitted_2Dt_noheight = get_fitted_kernel(dataset_name = paste0(site_name, "-", species, "-height-01"),
                               disp_mod = "2Dt",
                               err_mod = "pois", 
                               fecund_mod = "multiplier_exponent_noheight")


# Calculate and compare looic 
loo_2Dt_height = loo(fitted_2Dt_height$model)
loo_2Dt_noheight = loo(fitted_2Dt_noheight$model)
#loo_exppow = loo(fitted_exppow$model)
loo::loo_compare(loo(fitted_2Dt_height$model), loo(fitted_2Dt_noheight$model))

# Some Pareto k values are too high - check which ones 
loo::pareto_k_influence_values(loo_2Dt_height)
loo::pareto_k_ids(loo_2Dt_height)
summary(fitted_2Dt_height$model)[[1]][loo::pareto_k_ids(loo_2Dt_height),]
summary(fitted_2Dt_noheight$model)[[1]][loo::pareto_k_ids(loo_2Dt_height),]

# Visually check convergence of the main parameters 
disp_params = c("a", "k", "b1_ht", "b", "zeta")
plot(fitted_2Dt_height$model, plotfun = "trace", pars = disp_params) 
plot(fitted_2Dt_noheight$model, plotfun = "trace", pars = disp_params) 
#plot(fitted_exppow$model, plotfun = "trace", pars = disp_params) 




## Combine them so they can be plotted together
#kern_summary_comb = bind_rows(fitted_2Dt$kernel, fitted_exppow$kernel)
fitted_2Dt_height$kernel = fitted_2Dt_height$kernel |>
          mutate(height_mod = "yes")
fitted_2Dt_noheight$kernel = fitted_2Dt_noheight$kernel |>
  mutate(height_mod = "no")

kern_summary_comb = bind_rows(fitted_2Dt_height$kernel, fitted_2Dt_noheight$kernel)

## Plot them together by dispersal model 
# ggplot(data = kern_summary_comb, aes(x = r, y = fit, color=disp_mod, fill=disp_mod)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha=0.3, color=NA) +
#   geom_line(linewidth=1) +
#   theme_bw(20) +
#   scale_color_viridis_d(begin=0.3,end=0.7, name="Kernel") +
#   scale_fill_viridis_d(begin=0.3,end=0.7, name="Kernel") +
#   labs(x="Distance (m)", y = "Kernel density") +
#   coord_cartesian(ylim = c(0, 0.0002),
#                   xlim = c(0, 200))
# 
# ggsave(file.path(data_dir, "fitted-dispersal-kernels", paste0(site_name, ".png")), width=8, height=5)

## Plot them together by height model or not
ggplot(data = kern_summary_comb, aes(x = r, y = fit, color=height_mod, fill=height_mod)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha=0.3, color=NA) +
  geom_line(linewidth=1) +
  theme_bw(20) +
  scale_color_viridis_d(begin=0.3,end=0.7, name="Kernel") +
  scale_fill_viridis_d(begin=0.3,end=0.7, name="Kernel") +
  labs(x="Distance (m)", y = "Kernel density") +
  coord_cartesian(ylim = c(0, 0.0002),
                  xlim = c(0, 200))

## OK this checks whether the dispersal kernel changes when you remove the height model. It doesn't! 


## Make a fitted-observed plot for a specific fitted model. This requires knowing which trees contributed to that plot (at least their distances and sizes).
dataset_name = paste0(site_name, "-", species, "-height-01")
disp_mod = "2Dt"
err_mod = "pois"
fecund_mod = "multiplier_exponential"

load_fit_and_plot(dataset_name = dataset_name, disp_mod = disp_mod, err_mod = err_mod, fecund_mod, plot_size_ha = plot_size_ha, ylim = c(NA, NA))
#ggsave(file.path(data_dir, "fitted-observed-seedlings", paste0(site_name, "_kernel-", disp_mod, "-", err_mod, "-", fecund_mod, ".png")), width=6, height=5)



disp_mod = "exppow"
err_mod = "pois"

load_fit_and_plot(dataset_name = dataset_name, disp_mod = disp_mod, err_mod = err_mod, plot_size_ha = plot_size_ha, ylim = c(NA, NA))
ggsave(file.path(data_dir, "fitted-observed-seedlings", paste0(site_name, "_kernel-", disp_mod, ".png")), width=6, height=5)


#### !!! NOTE: The functions underlying the code below still have to be ported to the new ragged array format. Derek can help with this if you would like this for the presentation.


# 
# #### Fitted vs obs for distance to nearest
# 
# ## Load the dataset specified
# prepped_data_dir = datadir(paste0("prepped-for-stan/", dataset_name))
# overstory_tree_size = read_lines(paste0(prepped_data_dir, "/overstory-tree-size.txt")) %>% as.numeric
# seedling_counts = read_lines(paste0(prepped_data_dir, "/seedling-counts.txt")) %>% as.numeric
# r = read.table(paste0(prepped_data_dir,"/dist-mat.txt")) %>% as.matrix # rows are plots, columns are trees
# colnames(r) = NULL
# 
# dist_to_nearest = apply(r, 1, min)
# d = data.frame(obs = seedling_counts, dist_to_nearest)
# #plot(seedling_counts ~ dist_to_nearest, data = d)
# 
# m = gam(obs ~ s(dist_to_nearest, k = 3), data = d, method = "REML", family = "poisson")
# summary(m)
# #plot(m)
# d$fit = fitted(m, type = "response")
# 
# plot_fitted_observed(d, 1, 1, c(NA, NA))
# ggsave(datadir(paste0("fitted-observed-seedlings/", site_name, "_kernel-", disp_mod, ".png")), width=6, height=5)
# 
# 
# ## Gaussian smooth
# 
# ## Estimate tree BA and then seed output
# 
# a = 1.4721
# b = 0.6848
# dbh = function(h) {
#   (h/a)^(1/b)
# }
# 
# overstory_tree_ba =  3.14*(dbh(overstory_tree_size)/2)^2
# 
# overstory_tree_fecundity = (0.0107*(  0.1226  ^-0.58)*((113000*( overstory_tree_ba ^0.855))^1.08))
# 
# # define gaussian kernel
# gaus_kern = function(x) {
#   dnorm(x, mean = 0, sd = 45)
# }
# 
# # apply gaussian kernel
# rel_output = gaus_kern(r)
# 
# # multiply by fecundity
# fecundity_matrix = matrix(rep(overstory_tree_fecundity, times = nrow(rel_output)), nrow = nrow(rel_output), byrow = TRUE)
# seed_reaching_plot = fecundity_matrix * rel_output
# gaus_seeds = rowSums(seed_reaching_plot)
# 
# d$gaus_seeds = gaus_seeds
# 
# #plot(seedling_counts ~ gaus_seeds, data = d)
# 
# m = gam(obs ~ 0 + s(gaus_seeds, k = 3), data = d, method = "REML", family = "poisson")
# summary(m)
# #plot(m)
# d$fit = fitted(m, type = "response")
# 
# #plot(fit ~ obs, data = d)
# 
# plot_fitted_observed(d, 1, 1, c(NA, NA))
# ggsave(datadir(paste0("fitted-observed-seedlings/", site_name, "_kernel-", disp_mod, ".png")), width=6, height=5)
# 
# 
# 
####### Make maps of predicted density

##### From the fitted kernel

# Need to adapt the prep-data script so that it works on rasters as well, treating each grid cell as a "plot"

### Make raster of predictions

# For every cell in this landscape, make a "plot", calc mat of distances from trees, predict

library(terra)
boundary = st_read(datadir(paste0("boundaries/", site_name, ".gpkg")))
grid = rast(resolution = 30, ext = ext(boundary) , crs = "EPSG:3310")
values(grid) = 1:ncell(grid)

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
plan(multisession)
plot_seedl_preds = future_map_dfr(tree_dists_by_plot, predict_seedl_plot, samples = samples_median, overstory_tree_size = overstory_tree_size)
row.names(plot_seedl_preds) = NULL

pts = bind_cols(pts,plot_seedl_preds)

values(grid) = pts$fit

grid_mask = mask(grid, boundary)
plot(grid)
writeRaster(grid_mask,datadir(paste0("regen-prediction-maps/", site_name, "_kernel.tif")), overwrite=TRUE)


##### For distance to nearest tree

# trees
# grid
# pts
# r

# for each point (grid cell), compute distance to nearest tree

min_dist = apply(r, 1, min)
pts = bind_cols(pts, min_dist = min_dist)
values(grid) = pts$min_dist
grid_mask = mask(grid, boundary)
plot(grid_mask)
writeRaster(grid_mask,datadir(paste0("regen-prediction-maps/", site_name, "_min-dist.tif")), overwrite=TRUE)



##### For gaussian kernel


# apply gaussian kernel
rel_output = gaus_kern(r)

# multiply by fecundity
fecundity_matrix = matrix(rep(overstory_tree_fecundity, times = nrow(rel_output)), nrow = nrow(rel_output), byrow = TRUE)
seed_reaching_plot = fecundity_matrix * rel_output
gaus_seeds = rowSums(seed_reaching_plot)
pts = bind_cols(pts, gaus_fixed = gaus_seeds)
values(grid) = pts$gaus_fixed
grid_mask = mask(grid, boundary)
plot(grid_mask)
writeRaster(grid_mask,datadir(paste0("regen-prediction-maps/", site_name, "_gaus-fixed.tif")), overwrite=TRUE)


#######################

#### Start comparison of different fecundity functions here ####

fitted_2Dt_multiplier = get_fitted_kernel(dataset_name = paste0(site_name, "-", species, "-height-01"),
                               disp_mod = "2Dt",
                               err_mod = "pois", 
                               fecund_mod = "multiplier")

fitted_2Dt_multiplier_exponent = get_fitted_kernel(dataset_name = paste0(site_name, "-", species, "-height-01"),
                                          disp_mod = "2Dt",
                                          err_mod = "pois", 
                                          fecund_mod = "multiplier_exponent")

## Make a fitted-observed plot for a specific fitted model. 
dataset_name = paste0(site_name, "-", species, "-height-01")
disp_mod = "2Dt"
err_mod = "pois"
fecund_mod = "multiplier"

load_fit_and_plot(dataset_name = dataset_name, disp_mod = disp_mod, err_mod = err_mod, fecund_mod = fecund_mod, plot_size_ha = plot_size_ha, ylim = c(NA, NA))
#ggsave(file.path(data_dir, "fitted-observed-seedlings", paste0(site_name, "_kernel-", disp_mod, ".png")), width=6, height=5)

fecund_mod = "multiplier_exponent"
load_fit_and_plot(dataset_name = dataset_name, disp_mod = disp_mod, err_mod = err_mod, fecund_mod = fecund_mod, plot_size_ha = plot_size_ha, ylim = c(NA, NA))

## Combine them so they can be plotted together
kern_summary_comb = bind_rows(fitted_2Dt_multiplier$kernel, fitted_2Dt_multiplier_exponent$kernel)

## Plot the dispersal kernels from the 2 fecundity models together
ggplot(data = kern_summary_comb, aes(x = r, y = fit, color=fecund_mod, fill=fecund_mod)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha=0.3, color=NA) +
  geom_line(linewidth=1) +
  theme_bw(20) +
  scale_color_viridis_d(begin=0.3,end=0.7, name="Kernel") +
  scale_fill_viridis_d(begin=0.3,end=0.7, name="Kernel") +
  labs(x="Distance (m)", y = "Kernel density") +
  coord_cartesian(ylim = c(0, 0.0002),
                  xlim = c(0, 200))

ggsave(file.path(data_dir, "fitted-dispersal-kernels", paste0(site_name, ".png")), width=8, height=5)

## plot the two fecundity models vs tree height 
hist(overstory_treesize_vector) # check tree height range 
  
fecund_fun_multiplier = function(treesize, b) {return(b*treesize)}
fecund_fun_multiplier_exponent = function(treesize, b, zeta) {return(b*(treesize^zeta))}
fecund_mod_plot_data = data.frame(treesize_m = seq(min(overstory_treesize_vector), max(overstory_treesize_vector), by = 1))
fecund_mod_plot_data = fecund_mod_plot_data |> 
      mutate(linear = fecund_fun_multiplier(treesize_m, b = 3.56), exponential = fecund_fun_multiplier_exponent(treesize_m, b = 1.006, zeta = 1.38)) |>
      pivot_longer(cols = linear:exponential, values_to = "nseeds", names_to = "model")

ggplot(data = fecund_mod_plot_data, aes(x = treesize_m, y = nseeds, color=model, fill=model)) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr), alpha=0.3, color=NA) +
  geom_line(linewidth=1) +
  theme_bw(20) +
  scale_color_viridis_d(begin=0.3,end=0.7, name="Fecundity model") +
  scale_fill_viridis_d(begin=0.3,end=0.7, name="Fecundity") +
  labs(x="Tree height (m)", y = "Fecundity") #+
#  coord_cartesian(ylim = c(0, 0.0002),
#                  xlim = c(0, 200))