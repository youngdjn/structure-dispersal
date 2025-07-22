# Use the functions in "02_fit-model-using-ml-functions.R" to fit dispersal models
# 

#### Setup ####

library(here)
library(tidyverse)

source("./scripts/dispersal-modeling/01_load-data-for-modeling.R")
source("./scripts/dispersal-modeling/02_fit-model-using-ml-functions.R")

# Set data directory -- detect whether on Jetstream vs Andrew's machine and set accordingly
if (grep("latimer", here()) == 1) data_dir = readLines(here("data_dir_andrew.txt"), n = 1) else data_dir = readLines(here("data_dir.txt"), n = 1)

# Load dataset from one fire 

# NOTE FOR NOW PROCEED WITH JUST DELTA -- NEED TO STRAIGHTEN OUT DATA FILE STRUCTURE!!!! 
#   ALL THESE FILE PATHS ARE PROVISIONAL JUST TO TEST FUNCTIONS 

disp_data_dir = file.path(data_dir)
disp_data = get_dispdata(data_dir = disp_data_dir, 
                          #site_name = "delta", # e.g. "delta"
                          focal_species = "ABCO", # 4-letter code
                          overstory_tree_filepath = "predicted-treecrowns-w-predicted-species/delta.geojson",
                              # relative to `datadir`
                          seedling_plot_filepath = "regen-plots-standardized/delta.gpkg", 
                              # relative to `datadir`
                          target_crs = 3310, # target CRS (to project the raw data sources to)
                          seedling_plot_area = 201, # area of the plot in sq m
                          min_tree_height = 10, # ignore trees shorter than this (in meters)
                          density_raster_resolution = 10, # grid cell size for calculating local tree density in meters
                          tree_distance_cutoff = 500 # ignore trees farther than this from a plot (meters)
                          
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


# settings: a named list containing the values for all the options 
#    available for model fitting. These include: 
#    - lik_distrib = the data distribution for the model (pois or negbin)
#    - disp_kernel = which dispersal kernel to use (exppow, 2Dt, lognomal, wald)
#    - fecundity_fn = which fecundity function to use (linear or exponential)
#    - optimizer = which optimizer to tell optim to use (BFGS, Nelder-Mead, etc)
#                   NOTE: Currently this only uses SANN to be able to constrain 
#                                some params, so this setting is ignored.
settings_to_use = list(lik_distrib = "negbin", 
                       disp_kernel = "exppow", 
                       fecundity_fn = "linear", 
                       optimizer = "BFGS")

# Check likelihood calculation
pars_vector <- unlist(startpars1)
calculate_negloglik(pars_vector = pars_vector, 
               disp_data = disp_data, settings = settings_to_use)

# Fit models 
m1 = fit_model_ml(pars = startpars1, fixed_pars = "a", parscale = c(0.5, 10, 10, 0.5), disp_data = disp_data, settings = settings_to_use)

m2 = fit_model_ml(pars = startpars2, fixed_pars = "a",  parscale = c(0.5, 10, 10, 0.5), disp_data = disp_data, settings = settings_to_use)

m1$estimates
m1$negloglik

m2$estimates
m2$negloglik

# Pretty good results if we set the parscale argument, otherwise not necessarily consistent 

# some converge to reasonable values, some don't 
# Going to negbin from poisson improves deviance a lot, but doesn't seem to help convergence. 


