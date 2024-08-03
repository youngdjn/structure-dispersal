# Get seedling density values from fitted dispersal kernels and visualize them

library(tidyverse)
library(here)
library(mgcv)
library(rstan)
library(terra)

data_dir = readLines(here("data_dir.txt"), n = 1)
# for Andrew's local work
data_dir = "/Users/latimer/Library/CloudStorage/Box-Box/str-disp_data"


# The main functions used by this script. For parameter definitions, see this file.
source(here("scripts/dispersal-modeling/05_examine-fitted-kernel_functions.R"))


### Compute summarized (median and credible interval) dispersal kernels for the fitted models ###
# These functions return a list of three data frames: 1) one called "kernel" which is the fitted
# dispersal kernel value at a range of distances, 2) another called "shadow" which is the seed
# shadow from a tree of an average size for the site (combines kernel and fecundity), and 3) once
# called "model" that has the Stan samples

site_name = "delta"
plot_size_ha = 0.0201 # 0.09 for crater, 0.0113 for Chips, 0.0201 for others

# This loads and summarizes the kernel info from the corresponding Stan model object for particular species, sites, and dispersal kernel types. 
species = "PINES"
fitted_2Dt_PINES = get_fitted_kernel(
  dataset_name = paste0(site_name, "-", species),
  disp_mod = "2Dt",
  err_mod = "pois", 
  fecund_mod = "multiplier_exponent_noheight"
)

species = "FIRS"
fitted_2Dt_FIRS = get_fitted_kernel(
  dataset_name = paste0(site_name, "-", species),
  disp_mod = "2Dt",
  err_mod = "pois", 
  fecund_mod = "multiplier_exponent_noheight"
)

# Plot the dispersal kernel for the fitted model
ggplot(data = fitted_2Dt_PINES$kernel, aes(x = r, y = fit, color = disp_mod, fill = disp_mod)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, color = NA) +
  geom_line(linewidth = 1) +
  theme_bw(20) +
  scale_color_viridis_d(begin = 0.3, end = 0.7, name = "Kernel") +
  scale_fill_viridis_d(begin = 0.3, end = 0.7, name = "Kernel") +
  labs(x = "Distance (m)", y = "Kernel density")

ggplot(data = fitted_2Dt_FIRS$kernel, aes(x = r, y = fit, color = disp_mod, fill = disp_mod)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, color = NA) +
  geom_line(linewidth = 1) +
  theme_bw(20) +
  scale_color_viridis_d(begin = 0.3, end = 0.7, name = "Kernel") +
  scale_fill_viridis_d(begin = 0.3, end = 0.7, name = "Kernel") +
  labs(x = "Distance (m)", y = "Kernel density")

# Plot the relationship between tree size and fecundity 
ggplot(data = fitted_2Dt_PINES$fecundity, aes(x = tree_size, y = fit, color = fecund_mod, fill = fecund_mod)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, color = NA) +
  geom_line(linewidth = 1) +
  theme_bw(20) +
  scale_color_viridis_d(begin = 0.3, end = 0.7, name = "Fecundity") +
  scale_fill_viridis_d(begin = 0.3, end = 0.7, name = "Fecundity") +
  labs(x = "Tree height (m)", y = "Fecundity") + 
  theme(legend.position = "none")

ggplot(data = fitted_2Dt_FIRS$fecundity, aes(x = tree_size, y = fit, color = fecund_mod, fill = fecund_mod)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, color = NA) +
  geom_line(linewidth = 1) +
  theme_bw(20) +
  scale_color_viridis_d(begin = 0.3, end = 0.7, name = "Fecundity") +
  scale_fill_viridis_d(begin = 0.3, end = 0.7, name = "Fecundity") +
  labs(x = "Tree height (m)", y = "Fecundity") + 
  theme(legend.position = "none")
  


## Compare the 2Dt and exppow models

species = "PINES"
fitted_2Dt = get_fitted_kernel(
  dataset_name = paste0(site_name, "-", species),
  disp_mod = "2Dt",
  err_mod = "exppow"
)

fitted_exppow = get_fitted_kernel(
  dataset_name = paste0(site_name, "-", species),
  disp_mod = "exppow",
  err_mod = "pois"
)
loo::loo_compare(loo(fitted_2Dt$model), loo(fitted_exppow$model))


## Combine them so they can be plotted together
kern_summary_comb = bind_rows(fitted_2Dt$kernel, fitted_exppow$kernel)

## Plot them together
ggplot(data = kern_summary_comb, aes(x = r, y = fit, color = disp_mod, fill = disp_mod)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, color = NA) +
  geom_line(linewidth = 1) +
  theme_bw(20) +
  scale_color_viridis_d(begin = 0.3, end = 0.7, name = "Kernel") +
  scale_fill_viridis_d(begin = 0.3, end = 0.7, name = "Kernel") +
  labs(x = "Distance (m)", y = "Kernel density")
# coord_cartesian(ylim = c(0, 0.000005),
#                 xlim = c(0, 300))

ggsave(file.path(data_dir, "figures/fitted-dispersal-kernels",
                 paste0(site_name, ".png")), width = 8, height = 5)


# Make a fitted-observed plot for a specific fitted model. This requires knowing which trees
# contributed to that plot (at least their distances and sizes).
site_name = "delta"
species = "PINES"
dataset_name = paste0(site_name, "-", species)
disp_mod = "2Dt"
err_mod = "pois_multiplier_exponent_noheight"
# Note to specify a particular form of the fecundity model, we can tack extra text onto the "err_mod" parameter -- for example, "pois_multiplier_exponent". To select a model without the height difference component, also append "_noheight".

load_fit_and_plot(dataset_name = dataset_name, disp_mod = disp_mod, err_mod = err_mod, plot_size_ha = plot_size_ha, ylim = c(NA, NA))

ggsave(file.path(data_dir, "figures/fitted-observed-seedlings",
                 paste0(site_name, "_", species, "_", disp_mod, "_", err_mod, ".png")), width = 6, height = 5)

site_name = "delta"
species = "FIRS"
dataset_name = paste0(site_name, "-", species)
load_fit_and_plot(dataset_name = dataset_name, disp_mod = disp_mod, err_mod = err_mod, plot_size_ha = plot_size_ha, ylim = c(NA, NA))

ggsave(file.path(data_dir, "figures/fitted-observed-seedlings",
                 paste0(site_name, "_", species, "_", disp_mod, "_", err_mod, ".png")), width = 6, height = 5)


# Optionally run this for a different kernel or error model
disp_mod = "exppow"
err_mod = "pois"

load_fit_and_plot(dataset_name = dataset_name, disp_mod = disp_mod, err_mod = err_mod,
                  plot_size_ha = plot_size_ha, ylim = c(NA, NA))
ggsave(file.path(data_dir, "figures/fitted-observed-seedlings",
                 paste0(site_name, "_kernel-", disp_mod, ".png")), width = 6, height = 5)


# --- Plot fitted vs obs for a *distance to nearest seedsource* model instead of a kernel model, to
# show how the kernel model is an improvement

## Load the dataset specified
prepped_data_dir = datadir(paste0("/prepped-for-stan/", dataset_name))
seedling_counts = read_lines(paste0(prepped_data_dir, "/seedling-counts.txt")) %>% as.numeric()
dist_vector = read.table(paste0(prepped_data_dir, "/dist-vector.txt")) |>
  as.matrix() # rows are plots, columns are trees
elevdiff_vector = read.table(paste0(prepped_data_dir, "/elevdiff-vector.txt")) |>
  as.matrix() # rows are plots, columns are trees
n_overstory_trees = read.table(paste0(prepped_data_dir, "/n-overstory-trees.txt")) |>
  as.matrix() # rows are plots, columns are trees
overstory_treesize_vector = read.table(paste0(prepped_data_dir,
                                              "/overstory-treesize-vector.txt")) |>
  as.matrix() # rows are plots, columns are trees
pos = read.table(paste0(prepped_data_dir, "/pos.txt")) |>
  as.matrix() # rows are plots, columns are trees

splits = rep(seq_along(n_overstory_trees), times = n_overstory_trees)
dists = split(dist_vector, splits)
elevdiffs = split(elevdiff_vector, splits)
overstory_treesizes = split(overstory_treesize_vector, splits)

dist_to_nearest = lapply(dists, min) |> unlist()
d = data.frame(obs = seedling_counts, dist_to_nearest)

# Visualize raw data
plot(seedling_counts ~ dist_to_nearest, data = d)

# Fit a GAM to predict seedling count based on distance to nearest
m = gam(obs ~ s(dist_to_nearest, k = 3), data = d, method = "REML", family = "poisson")
summary(m)
plot(m)
d$fit = fitted(m, type = "response")

plot_fitted_observed(d, 1, 1, c(NA, NA))
ggsave(datadir(paste0("figures/fitted-observed-seedlings/",site_name, "_dist-to-nearest.png")),
       width = 6, height = 5)

# -- Compare against a *Gaussian smooth* approach that estimtes seed output based on BA and then
# smooths it
# !!!!!! MUST CAREFULLY REVIEW THIS code before considering final

# Estimate tree BA and then seed output. This is based on some cursory allometry and needs to be
# refined.

a = 1.4721
b = 0.6848
dbh = function(h) {
  (h / a)^(1 / b)
}

ba = function(dbh) {
  3.14 * (dbh / 2)^2
}

overstory_tree_dbh = lapply(overstory_treesizes, dbh)
overstory_tree_ba = lapply(overstory_tree_dbh, ba)

# Estiamte fecundity based on BA using the Postcrpt math, assuming an average-sized seed across all
# CA conifer species, weighted by abundance (0.1226 g)
fecundity = function(ba) {
  (0.0107 * (0.1226^-0.58) * ((113000 * (ba^0.855))^1.08))
}

overstory_tree_fecundity = lapply(overstory_tree_ba, fecundity)

# define gaussian kernel, using the Postcrpt parameterization for the "all conifers" model
gaus_kern = function(x) {
  dnorm(x, mean = 0, sd = 45)
}

# apply gaussian kernel
rel_output = lapply(dists, gaus_kern)

# multiply relative output by fecundity, to get seeds reaching each plot from each tree
seeds_reaching_plot = list()
for (i in seq_len(length(rel_output))) {
  seeds_reaching_plot[[i]] = rel_output[[i]] * overstory_tree_fecundity[[i]]
}

# Sum across all trees contributing to each plot to get total seeds reaching each plot
gaus_seeds = lapply(seeds_reaching_plot, sum) |> unlist()

d$gaus_seeds = gaus_seeds

# Vis raw data (well actually computed gaussian smooth seed input vs observed seedling counts)
plot(seedling_counts ~ gaus_seeds, data = d)

# Fit a GAM to predict seedling density based on gaussian-smooth seed input
m = gam(obs ~ 0 + s(gaus_seeds, k = 3), data = d, method = "REML", family = "poisson")
summary(m)
plot(m)
d$fit = fitted(m, type = "response")

plot_fitted_observed(d, 1, 1, c(NA, NA))
ggsave(datadir(paste0("figures/fitted-observed-seedlings/",
                      site_name, "_gaus-smooth.png")), width = 6, height = 5)




####### Make raster maps of predicted density from the fitted kernel

# Optional TODO: Adapt the prep-data script so that it works on rasters as well. Currently
# converting each grid cell to a point ("plot") to work with existing function.

# For every raster cell in this landscape, make a "plot", calc mat of distances from trees, predict

# Get site's geographic analysis bounds
boundary = st_read(datadir(paste0("/boundaries/", site_name, ".gpkg")))

# Make a raster grid to hold predictions
grid = rast(resolution = 30, ext = ext(boundary), crs = "EPSG:3310")
values(grid) = 1:ncell(grid)

## make the cells into points
pts = as.points(grid) %>% st_as_sf()
coords = st_coordinates(pts)
pts$x = coords[, 1]
pts$y = coords[, 2]
pts_spatial = pts
st_geometry(pts) = NULL

## load the tree coords
trees = st_read(datadir(paste0("/ttops-live/", site_name, ".gpkg"))) |> st_transform(3310)
tree_coords = st_coordinates(trees)
trees$x = tree_coords[, 1]
trees$y = tree_coords[, 2]
tree_data = trees
st_geometry(tree_data) = NULL

# Calculate tree-point distance matrix
d2min <- 0.01

dist_sq <- outer(pts$x, tree_data$x, "-")^2 + outer(pts$y, tree_data$y, "-")^2
dist_sq[dist_sq < d2min] <- d2min
r <- sqrt(dist_sq)


# Get overstory tree size
overstory_tree_size = tree_data$Z


# Calculate the tree-point height distance matrix

# download DEM of area
area = st_buffer(trees, 400) |> st_union()
dem = elevatr::get_elev_raster(area, src = "gl1")
dem = rast(dem)

# Calculate the elev difference between each tree and each point (grid cell)
treeht = terra::extract(dem, trees)[, 2] + trees$Z
cellht = terra::extract(dem, pts_spatial)[, 2]

elevdiff <- -outer(cellht, treeht, "-")

## Load the fitted model and extract the parameter samples
site_name = "delta"
species = "PINES"
dataset_name = paste0(site_name, "-", species)
disp_mod = "2Dt"
err_mod = "pois"
fecund_mod = "multiplier_exponent_noheight"

samples = get_stan_model_samples(dataset_name = dataset_name, disp_mod = disp_mod, err_mod = err_mod, fecund_mod = fecund_mod)

# Summarize across the samples, dropping uncertainty (faster predictions)
samples_median = map(samples, median)

# To run across all "plots" (grid cells), need to make a list of tree_plot_dists with one list item
# per plot, containing the distances to each tree for that plot
tree_dists_by_plot = apply(r, 1, FUN = c, simplify = FALSE)
elev_diffs_by_plot = apply(elevdiff, 1, FUN = c, simplify = FALSE)

# Make predictions across all plots, but using just the point estimate of each parameter (no
# uncertainty)
plan(multicore)

## OPTIONALLY: set elev diffs to 0 to see effect of ignoring elev diffs
 elev_diffs_by_plot = 0

## OPTIONALLY: set overstory tree size to a small value to see effect of ignoring tree size
# overstory_tree_size = rep(quantile(overstory_tree_size, 0.25), length(overstory_tree_size))

# Use the fitted model to make the predictions for this new dataset of "plots" representing grid
# cells
plot_seedl_preds = future_map2_dfr(tree_dists_by_plot, elev_diffs_by_plot, predict_seedl_plot,
                                   samples = samples_median, tree_sizes = overstory_tree_size)
row.names(plot_seedl_preds) = NULL

pts = bind_cols(pts, plot_seedl_preds)

values(grid) = pts$fit

grid_mask = mask(grid, boundary)
plot(grid)
writeRaster(grid_mask,
            datadir(paste0("/figures/regen-prediction-maps/", site_name, "_kernel.tif")),
            overwrite = TRUE)


# --- For mapping the seed rain predictions of alternate methods (e.g. gaussian smooth, distance to
# nearest). Still need to clean this up and port it to the ragged array approach

# ##### For distance to nearest tree

# # trees
# # grid
# # pts
# # r

# # for each point (grid cell), compute distance to nearest tree

# min_dist = apply(r, 1, min)
# pts = bind_cols(pts, min_dist = min_dist)
# values(grid) = pts$min_dist
# grid_mask = mask(grid, boundary)
# plot(grid_mask)
# writeRaster(grid_mask,datadir(paste0("regen-prediction-maps/", site_name, "_min-dist.tif")), overwrite=TRUE)



# ##### For gaussian kernel


# # apply gaussian kernel
# rel_output = gaus_kern(r)

# # multiply by fecundity
# fecundity_matrix = matrix(rep(overstory_tree_fecundity, times = nrow(rel_output)), nrow = nrow(rel_output), byrow = TRUE)
# seed_reaching_plot = fecundity_matrix * rel_output
# gaus_seeds = rowSums(seed_reaching_plot)
# pts = bind_cols(pts, gaus_fixed = gaus_seeds)
# values(grid) = pts$gaus_fixed
# grid_mask = mask(grid, boundary)
# plot(grid_mask)
# writeRaster(grid_mask,datadir(paste0("regen-prediction-maps/", site_name, "_gaus-fixed.tif")), overwrite=TRUE)
