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
startpars1 = list(b = 10, k = 0.5, a = 10)
startpars2 = list(b = 10, k = 0.5, a = 10, theta = 5)


# settings: a named list containing the values for all the options 
#    available for model fitting. These include: 
#    - lik_distrib = the data distribution for the model (pois or negbin)
#    - disp_kernel = which dispersal kernel to use (exppow, 2Dt, lognomal, wald)
#    - fecundity_fn = which fecundity function to use (linear or exponential)
#    - optimizer = which optimizer to tell optim to use (BFGS, Nelder-Mead, etc)
settings_to_use = list(lik_distrib = "pois", 
                       disp_kernel = "exppow", 
                       fecundity_fn = "linear", 
                       optimizer = "BFGS")

# Check likelihood calculation
calculate_negloglik(pars = c(startpars1$b, startpars1$k, startpars1$a), 
               disp_data = disp_data, settings = settings_to_use)
# It's sensitive to extreme values of k and a (gives NLL = Inf)

m1 = fit_model_optim(startpars1, 
                     n_overstory_trees = disp_data$n_overstory_trees, 
                     dist_vector = disp_data$dist_vector, 
                     overstory_tree_size = disp_data$overstory_tree_size, 
                     pos = disp_data$pos, 
                     seedling_counts = disp_data$seedling_counts, 
                     seedling_plot_area = disp_data$seedling_plot_area,
                     lik_distrib = "pois")

m2 = fit_model_optim(startpars2, 
                     n_overstory_trees = disp_data$n_overstory_trees, 
                     dist_vector = disp_data$dist_vector, 
                     overstory_tree_size = disp_data$overstory_tree_size, 
                     pos = disp_data$pos, 
                     seedling_counts = disp_data$seedling_counts, 
                     seedling_plot_area = disp_data$seedling_plot_area, 
                     lik_distrib = "negbin")

m1$estimates
m1$negloglik

m2$estimates
m2$negloglik

# some converge to reasonable values, some don't 
# Going to negbin from poisson improves deviance a lot, but doesn't seem to help convergence. 

m = m1 # choose model to plot

# plot fitted vs observed 
obspred_data <- data.frame(fitted = m$fitted.values, observed = disp_data$seedling_counts)
ggplot(obspred_data, aes(x = log(fitted), y = log(observed+0.5))) + geom_point()

# Check of residuals 
obspred_data$resids = obspred_data$observed-obspred_data$fitted
qqnorm(obspred_data$resids)
plot(resids~fitted, obspred_data)
hist(obspred_data$resids)


# plot dispersal kernel based on fitted parameters
kernel_plot_data <- data.frame(Distance = 1:800, Probability = disp_prob(k = m$estimates$k, a = m$estimates$a, dist_vector = 1:800))
ggplot(kernel_plot_data, aes(x = Distance, y = Probability)) + geom_line()

# visualize likelihood surface 
bvals = 90
kvals = seq(0.2, 0.7, by = 0.05)
avals = seq(5, 120, by = 5)
thetavals = 50
parameter_test_set <- expand.grid(bvals, kvals, avals, thetavals) 
names(parameter_test_set) = c("b", "k", "a", "theta")
head(parameter_test_set)
fn_to_apply_negloglik <- function(param_test_vals, n_overstory_trees, dist_vector, overstory_tree_size, pos, seedling_counts, seedling_plot_area) {
  pars = c(param_test_vals[1], param_test_vals[2], param_test_vals[3], param_test_vals[4])
  nll = calc_negloglik(pars = pars, n_overstory_trees, 
                       dist_vector, overstory_tree_size, pos, 
                       seedling_counts, seedling_plot_area)
  return(nll)
}

negloglikvals <- apply(parameter_test_set, 1, fn_to_apply_negloglik, 
                       n_overstory_trees = disp_data$n_overstory_trees, 
                       dist_vector = disp_data$dist_vector, 
                       overstory_tree_size = disp_data$overstory_tree_size, 
                       pos = disp_data$pos, 
                       seedling_counts = disp_data$seedling_counts, 
                       seedling_plot_area = disp_data$seedling_plot_area)
lik_surface_data <- cbind(parameter_test_set, negloglikvals)
head(lik_surface_data)
hist(negloglikvals)

# Where is the maximum? 
lik_surface_data[which.max(lik_surface_data$negloglikvals),]

# plot a 2D likelihood surface using negloglikvals data
ggplot(lik_surface_data, aes(x = a, y = k, z = negloglikvals)) + 
  geom_tile(aes(fill = log(negloglikvals))) + 
  #geom_contour() + 
  scale_fill_viridis_c() + 
  theme_minimal() + 
  labs(title = "Likelihood surface", x = "a", y = "k") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(title = "Negative log likelihood"))

ggplot(lik_surface_data, aes(x = b, y = k)) + 
  geom_tile(aes(fill = negloglikvals)) + 
  scale_fill_viridis_c() + 
  theme_minimal() + 
  labs(title = "Likelihood surface", x = "b", y = "k") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(title = "Negative log likelihood"))


# NOTE: Convergence is sensitive to starting values -- can converge to "reasonable" or extreme values for most data sets 
# NOTE: Using 500m distance seems slighly more stable (more informative)


## Next steps: 
# Visualize likelihood surface. DONE - shows a ridge as expected based on a / k correlation. 
# The model runs away to very unrealistic and extreme parameter combinations. TRY keeping b fixed. DONE - didn't help 
# Check that model can recover params from simulation. DONE -- yes it can! 

# Since simulations work to recover parameters, see if any of the data sets for the other 3 fires (other than Delta) can converge to reasonable parameter values. 

# Implement negbin likelihood - done 

# Systematically test which data sets x likelihood types converge consistently to something biologically plausible, or at least do so from reasonable starting values. 



