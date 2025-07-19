# Use the functions in "02_fit-model-using-ml-functions.R" to fit dispersal models
# 

#### Setup ####

library(here)
library(tidyverse)

# Set data directory -- detect whether on Jetstream vs Andrew's machine and set accordingly
if (grep("latimer", here()) == 1) data_dir = readLines(here("data_dir_andrew.txt"), n = 1) else data_dir = readLines(here("data_dir.txt"), n = 1)

# Load functions 
source("./scripts/dispersal-modeling/02_fit-model-using-ml-functions.R")


# Load dataset from one fire 
disp_data_dir = file.path(data_dir, "max500")
disp_data = get_disp_data(dataset_name = "delta-FIRS", data_dir = data_dir)

# Or instead load a simulated dataset 
#seedling_plot_area = 201
#n_seedling_plots = n_plots



#disp_data = lst(seedling_plot_area,
#                      n_overstory_trees,
#                     n_seedling_plots, 
#                    overstory_tree_size,
#                   seedling_counts,
#                      dist_vector,
#                      obs = length(dist_vector),
#                      pos)

# check that the model converges from dispersed initial values 
startpars1 = list(b = 10, k = 0.5, a = 10)
startpars2 = list(b = 10, k = 0.5, a = 10, theta = 5)

# Convergence is sensitive to starting values -- can converge to "reasonable" or extreme values for most data sets 
# Using 500m distance seems slighly more stable (more informative)


# Check likelihood calculation
calc_negloglik(pars = c(startpars1$b, startpars1$k, startpars1$a), 
               n_overstory_trees = disp_data$n_overstory_trees, 
               dist_vector = disp_data$dist_vector, 
               overstory_tree_size = disp_data$overstory_tree_size, 
               pos = disp_data$pos, 
               seedling_counts = disp_data$seedling_counts, 
               seedling_plot_area = disp_data$seedling_plot_area, lik_distrib = "pois")
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


## Next steps: 
# Visualize likelihood surface. DONE - shows a ridge as expected based on a / k correlation. 
# The model runs away to very unrealistic and extreme parameter combinations. TRY keeping b fixed. DONE - didn't help 
# Check that model can recover params from simulation. DONE -- yes it can! 

# Since simulations work to recover parameters, see if any of the data sets for the other 3 fires (other than Delta) can converge to reasonable parameter values. 

# Implement negbin likelihood - done 

# Systematically test which data sets x likelihood types converge consistently to something biologically plausible, or at least do so from reasonable starting values. 



