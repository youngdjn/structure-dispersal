# Uses the functions in "02_fit-model-using-ml-functions.R" to fit dispersal models
#   using maximum likelihood. 
# The advantage of this over using stan and MCMC is speed for testing.

# NOTE: Need to decide whether to switch back to stan for final inference. 

#### Setup ####

library(here) 
library(tidyverse)
 
source("./scripts/dispersal-modeling/01_load-data-for-modeling_functions.R")
source("./scripts/dispersal-modeling/02_fit-model-using-ml-functions.R")

# Set data directory -- first detect whether on Jetstream vs Andrew's machine
data_dir = readLines(here("data_dir.txt"))
if (grep("latimer", here()) == 1) data_dir = readLines(here("data_dir_andrew.txt"), n = 1)


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

disp_data = simulate_dispdata(domain_size = 800, # length of one side of simulated square "domain"
  center_buffer = 200, # distance from central area with trees to edge of "domain"
  trees_per_hectare = 100, # density of trees to simulate
  n_plots = 200, # number of seedling plots to simulate
  a = 30,     # scale parameter for exppow dispersal kernel
  k = 0.5,    # shape parameter for exppow dispersal kernel
  b = 10,     # fecundity coefficient
  seedling_plot_area = 201,  # size of each plot in m²
  elev_range = 100, # elevation range (m) for simulated DEM
  min_tree_height = 10, # min tree height (m) for simulation
  max_tree_height = 30, # max tree height (m) for simulation
  density_raster_resolution = 15, # grid cell size for calculating local tree density
  tree_distance_cutoff = 300 # ignore trees farther than this from a plot
)

# Set some different initial parameter values to check convergence
startpars1 = list(k = 0.7, a = 40, b = 10, theta = 1)
startpars2 = list(k = 0.9, a = 10, b = 1, theta = 2)
parscale = c(1, 10, 10)

# Choose model control settings 
# 
# Documentation of settings needed to fit an ML model 
# settings: a named list containing the values for all the options 
#    available for model fitting. These include: 
#    - lik_distrib = the data distribution for the model (pois or negbin)
#    - disp_kernel = which dispersal kernel to use (exppow, 2Dt, lognomal, wald)
#    - fecundity_fn = which fecundity function to use (linear or exponential)
#    - optimizer = which optimizer to tell optim to use (BFGS, Nelder-Mead, etc)
#                   NOTE: Currently this only uses SANN to be able to constrain 
#                                some params, so this setting is ignored.

settings = list(lik_distrib = "negbin", disp_kernel = "exppow", fecundity_fn = "linear", optimizer = NULL)

m1 <- fit_model_ml(pars = startpars1, fixed_pars = NULL, parscale = parscale, disp_data = disp_data, settings = settings)
m2 <- fit_model_ml(pars = startpars2, fixed_pars = NULL, parscale = parscale, disp_data = disp_data, settings = settings)
m3 <- fit_model_ml(pars = startpars1, fixed_pars = "k", parscale = parscale, disp_data = disp_data, settings = settings)


##### Compare among dispersal kernels and fecundity functions 

# Choose a species and site (only one at a time for now)
site_name = "delta"
focal_species = "PIPJ"

# Choose tree distance cutoff in m (beyond this assume zero seed dispersal)
tree_distance_cutoff = 300 

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

# Make a data fram containing a grid of parameter values k, a, and b 
#   Also need to specify theta if using negative binomial
model_options_grid = expand.grid(k = seq(0.2, 7, by = 0.1), 
                                 a = seq(20, 120, by = 20), 
                                 b = seq(20, 60, by = 20), 
                                 theta = 0.6)


# Evaluate the likelihood at each set of values 
model_fits <- pmap(model_options_grid, calculate_negloglik, disp_data = disp_data, settings = settings)

# Get AIC values 
#model_AIC <- lapply(model_fits, f <- function(m) return(2*m$negloglik + 2*m$model_info$n_parameters))
#model_options_grid$AIC <- unlist(model_AIC)

# Compare parameter values 
#model_options_grid$k = unlist(lapply(model_fits, f <- function(m) return(m$estimates$k)))
#model_options_grid$a = unlist(lapply(model_fits, f <- function(m) return(m$estimates$a)))
#model_options_grid$b = unlist(lapply(model_fits, f <- function(m) return(m$estimates$b)))

#unlist(lapply(model_fits, f <- function(m) return(m$estimates$zeta)))

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


