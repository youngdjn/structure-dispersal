# Use the functions in "02_fit-model-using-ml-functions.R" to fit dispersal models
# 

#### Setup ####

library(here)
library(tidyverse)

source("./scripts/dispersal-modeling/01_load-data-for-modeling.R")
source("./scripts/dispersal-modeling/02_fit-model-using-ml-functions.R")

# Set data directory -- detect whether on Jetstream vs Andrew's machine and set accordingly
if (grep("latimer", here()) == 1) data_dir = readLines(here("data_dir_andrew.txt"), n = 1) else data_dir = readLines(here("data_dir.txt"), n = 1)

# Choose site and species 
site_name = "delta"
focal_species = "PIPJ"
# NOTE FOR NOW THE PLOT DATA IS NOT BROKEN OUT BY SPECIES EXCEPT FOR DELTA! 

# Load data 
disp_data_dir = file.path(data_dir)
disp_data = get_dispdata(data_dir = disp_data_dir, 
                          site_name = site_name, # e.g. "delta"
                          focal_species = focal_species, # 4-letter code
                          overstory_tree_filepath = paste0("predicted-treecrowns-w-predicted-species/", site_name, ".geojson"),
                              # relative to `datadir`
                          seedling_plot_filepath = paste0("regen-plots-standardized/", site_name, ".gpkg"), 
                              # relative to `datadir`
                          target_crs = 3310, # target CRS (to project the raw data sources to)
                          seedling_plot_area = 201, # area of the plot in sq m
                          min_tree_height = 10, # ignore trees shorter than this (in meters)
                          density_raster_resolution = 10, # grid cell size for calculating local tree density in meters
                          tree_distance_cutoff = 300 # ignore trees farther than this from a plot (meters)
                          
)

# Alternatively: simulate a dataset 

# disp_data = simulate_dispdata(domain_size = 800, # length of one side of simulated square "domain"
#   center_buffer = 200, # distance from central area with trees to edge of "domain"
#   trees_per_hectare = 100, # density of trees to simulate
#   n_plots = 200, # number of seedling plots to simulate
#   a = 30,     # scale parameter for exppow dispersal kernel
#   k = 0.5,    # shape parameter for exppow dispersal kernel
#   b = 10,     # fecundity coefficient
#   seedling_plot_area = 201,  # size of each plot in m²
#   elev_range = 100, # elevation range (m) for simulated DEM
#   min_tree_height = 10, # min tree height (m) for simulation
#   max_tree_height = 30, # max tree height (m) for simulation
#   density_raster_resolution = 15, # grid cell size for calculating local tree density
#   tree_distance_cutoff = 300 # ignore trees farther than this from a plot
# )

# Set some diffent initial parameter values to check convergence
startpars1 = list(k = 0.5, a = 10, b = 10, theta = 1)
startpars2 = list(k = 0.9, a = 10, b = 1, theta = 2)

##### Compare among dispersal kernels and fecundity functions 

# Choose a species and site (only one at a time for now)
site_name = "delta"
focal_species = "ALL"

# Choose tree distance cutoff in m (beyond this assume zero seed dispersal)
tree_distance_cutoff = 300 

# Set data directory -- detect whether on Jetstream vs Andrew's machine and set accordingly
if (grep("latimer", here()) == 1) data_dir = readLines(here("data_dir_andrew.txt"), n = 1) else data_dir = readLines(here("data_dir.txt"), n = 1)

# Get the data for this site and species 
disp_data = get_dispdata(data_dir = data_dir, # base level for data files (e.g. "/ofo-share/str-disp_data")
              site_name = site_name, 
              focal_species = focal_species, 
              overstory_tree_filepath = paste0("predicted-treecrowns-w-predicted-species/", site_name, ".geojson"),
              seedling_plot_filepath = paste0("regen-plots-standardized/", site_name, ".gpkg"),
              target_crs = 3310, 
              seedling_plot_area = 201,
              min_tree_height = 10, # ignore trees shorter than this
              density_raster_resolution = 10, 
              tree_distance_cutoff = tree_distance_cutoff # ignore trees farther than this from a plot
) 



#### Parallelize the model fitting loop 

library(foreach)
library(doParallel)

# How many cores to use in cluster
n_cores <- detectCores()

# Register cluster
cluster <- makeCluster(n_cores - 2)
registerDoParallel(cluster)

model_fits <- fit_model_wrapper_fn_parallel(model_options_grid, disp_data)

# Don't forget to stop the cluster
stopCluster(cl = cluster)


# Get the AIC values 
model_AIC <- lapply(model_fits, f <- function(m) return(2*m$negloglik + 2*m$model_info$n_parameters))
model_options_grid$AIC <- unlist(model_AIC)

# compare parameter values 
model_options_grid$k = unlist(lapply(model_fits, f <- function(m) return(m$estimates$k)))
model_options_grid$a = unlist(lapply(model_fits, f <- function(m) return(m$estimates$a)))
model_options_grid$b = unlist(lapply(model_fits, f <- function(m) return(m$estimates$b)))

unlist(lapply(model_fits, f <- function(m) return(m$estimates$zeta)))

# Make plots 
ggplot(model_options_grid, aes(x = disp_kernel_vals, y = AIC, 
  color = lik_distrib_vals, shape = fecundity_fn_vals)) + 
  geom_point(size = 2) + theme_bw() + labs(x = "Dispersal Kernel") + 
  scale_color_discrete(name = "Likelihood type") + 
  scale_shape_discrete(name = "Fecundity function")

# Just plot the negative binomial fits which are always better 
model_options_grid |> 
  filter(lik_distrib_vals == "negbin") |> 
  ggplot(aes(x = disp_kernel_vals, y = AIC, color = fecundity_fn_vals)) + 
  geom_point() + theme_bw() + labs(x = "Dispersal Kernel") + 
  scale_color_discrete(name = "Likelihood type")


