# Implement ML fitting of inverse dispersal model. 
# Following example from Frank Schurr. 

# Start with an example simple model (exppow, with a one-parameter fecundity function)

# Fecundity function (vectorized calculation for a single plot)
q_fun <- function(pars, overstory_tree_size, fecundity_fn = "linear") { 
  if (fecundity_fn == "linear") q = pars$b * overstory_tree_size # fecundity equation
  if (fecundity_fn == "exponential") q = pars$b * overstory_tree_size ^ pars$zeta # fecundity equation
  return(q)
}

# Dispersal kernel function (vectorized calculation for a single plot)
disp_prob <- function(pars, dist_vector, disp_kernel = "exppow") { # 
  # For readability, extract individual parameter values from pars list
  a = pars$a
  b = pars$b
  k = pars$k
  
  # choose kernel and apply to all trees associated with a given plot
  if (disp_kernel == "exppow") kernel_prob = exp(-(dist_vector/a)^k)*k / 
            (2 * pi * a^2 * gamma(2/k)) 
  if (disp_kernel == "2Dt") kernel_prob = ((k-1) / (pi * a^2)) * 
      ((1 + dist_vector^2) / a^2)^(-k) # NEED TO CHECK THIS (Katul vs Marchand)
  if (disp_kernel == "lognormal") {
    mu<-log(a)-k^2/2 # (e.g. Stoyan & Wagner 2001, Ecological Modelling 145: 35-47)
    kernel_prob = exp(-((log(dist_vector)-mu)^2)/ # CHECK
            (2*k^2))/(k*(2*pi)^(3/2)*dist_vector^2)
  }
  if (disp_kernel == "wald") {#(Katul et al. 2005, American Naturalist 166: 368-381)
      kernel_prob = k^0.5*(2*pi)^(-1.5) * dist_vector^(-2.5) * 
            exp(-(k*(dist_vector-a)^2)/(2*a^2*dist_vector)) 
  }

  return(kernel_prob)
}


# Calculate expected # of seedlings per plot 
calc_mu <- function(pars, n_overstory_trees, dist_vector, overstory_tree_size, pos, seedling_plot_area, fecundity_fn, disp_kernel) {
  n_seedling_plots = length(n_overstory_trees) 
  mu = vector(mode = "numeric", length = n_seedling_plots)
  
  for(i in 1:n_seedling_plots){
      segment_start = pos[i]
      segment_end = pos[i] + n_overstory_trees[i] - 1
      mu[i] = sum(disp_prob(pars, dist_vector[segment_start:segment_end], disp_kernel)
          * q_fun(pars, overstory_tree_size[segment_start:segment_end], fecundity_fn)
          * seedling_plot_area) # plot area
  }
  return(mu)
}

# Function to get the negative log likelihood for a set of parameter values
calc_negloglik <- function(pars, n_overstory_trees, dist_vector, overstory_tree_size, pos, seedling_counts, seedling_plot_area, lik_distrib, fecundity_fn, disp_kernel) {
  
  mu = calc_mu(pars=pars, n_overstory_trees=n_overstory_trees, dist_vector=dist_vector, overstory_tree_size=overstory_tree_size, pos=pos, seedling_plot_area=seedling_plot_area, fecundity_fn=fecundity_fn, disp_kernel=disp_kernel) 
  if (lik_distrib == "pois") {
    negloglik = -sum(dpois(x = seedling_counts, lambda = mu, log=TRUE))
  }
  if (lik_distrib == "negbin") {
    negloglik = -sum(dnbinom(x = seedling_counts, mu = mu, size = pars$theta, log=TRUE))
  }
  return(negloglik)
}


### Function to fit the model using optim
fit_model_ml <- function(pars, disp_data, settings)
{
  #ML fitting of an inverse model with source and path effects
  #ARGUMENTS:
  #
  # pars: a named list containing the starting values for parameters
  #           of the dispersal kernel (k, a), fecundity function (b, zeta), 
  #           and the negative binomial likelihood dispersion parameter (theta)
  # 
  # settings: a named list containing the values for all the options 
  #    available for model fitting. These include: 
  #    - lik_distrib = the data distribution for the model (pois or negbin)
  #    - disp_kernel = which dispersal kernel to use (exppow, 2Dt, lognomal, wald)
  #    - fecundity_fn = which fecundity function to use (linear or exponential)
  #    - optimizer = which optimizer to tell optim to use (BFGS, Nelder-Mead, etc)
  #
  # disp_data: a data object returned from get_disp_data() 
  #              or simulate_disp_data(). Must include: 
  #               - n_overstory_trees, dist_vector, overstory_tree_size, pos
  #                  (in sparse/ragged form: pos points to the starting point of each 
  #                  plot's data in the vectors dist_vector and overstory_tree_size, 
  #                  and n_overstory_trees gives number of trees per plot. 
  #                - seedling_plot_area = area of plots (m)
  #                - seedling count = number of trees in each plot
  #
  #VALUE: list with components:
  #
  #estimates: a named list of maximum likelihood parameter estimates,
  #           for each component parameters are in the same order as in startpars
  #negloglik: the negative log likelihood of the fitted inverse model
  #fitted.values: the fitted values for each seed trap
  #call:      the matched call
  #counts,convergence and message: the respective output of the optim function
  #           used to fit the inverse models
  
  # Save the model call
  cl <- match.call()
  
  # Convert list of parameters to the vector form needed by optim
  #   Note this set depends on which model is fitted and number of parameters in it
  
  # Define parameter structure based on model settings
  get_par_structure <- function(settings) {
    par_names <- c("k", "a")  # dispersal kernel parameters
    
    # Add fecundity parameters based on function type
    if (settings$fecundity_fn == "linear") {
      par_names <- c(par_names, "b")
    } else if (settings$fecundity_fn == "exponential") {
      par_names <- c(par_names, "b", "zeta")
    }
    
    # Add dispersion parameter if using negative binomial
    if (settings$lik_distrib == "negbin") {
      par_names <- c(par_names, "theta")
    }
    
    return(par_names)
  }
  
  # Get the parameter structure for this model
  par_structure <- get_par_structure(settings)
  
  # Convert named list to vector, ensuring correct order
  par_vector <- sapply(par_structure, function(name) pars[[name]])
  
  # Wrapper function that converts vector back to named list
  calc_negloglik_wrapper <- function(par_vec, disp_data, settings) {
    par_list <- setNames(as.list(par_vec), par_structure)
    calc_negloglik(pars = par_list, 
                   n_overstory_trees = disp_data$n_overstory_trees,
                   dist_vector = disp_data$dist_vector,
                   overstory_tree_size = disp_data$overstory_tree_size,
                   pos = disp_data$pos,
                   seedling_counts = disp_data$seedling_counts,
                   seedling_plot_area = disp_data$seedling_plot_area,
                   lik_distrib = settings$lik_distrib,
                   disp_kernel = settings$disp_kernel,
                   fecundity_fn = settings$fecundity_fn)
  }
  
  # Fit the model
  fit <- optim(par_vector, calc_negloglik_wrapper, 
               method = settings$optimizer,
               control = list(trace = TRUE, maxit = 10000), disp_data = disp_data,
               settings = settings)
  
  # Convert results back to named format
  fit$par_named <- setNames(as.list(fit$par), par_structure)

  # Old version without wrapper 
  # CHECK WHAT FORMAT "pars" HAS TO BE IN -- MAY NEED TO TRANSLATE GOING IN AND OUT TO LIST FORMAT? 
#  fit <- optim(pars, method = settings$optimizer, control = list(trace = TRUE, maxit = 10000), calc_negloglik, n_overstory_trees = disp_data$n_overstory_trees, dist_vector = disp_data$dist_vector, overstory_tree_size = disp_data$overstory_tree_size, pos = disp_data$pos, seedling_counts = disp_data$seedling_counts, seedling_plot_area = disp_data$seedling_plot_area, lik_distrib = settings$lik_distrib, disp_kernel = settings$disp_kernel, fecundity_fn = settings$fecundity_fn) 
  
  # Convert parameter estimates back to named format
  estimates <- setNames(as.list(fit$par), par_structure)
  
  # Calculate fitted values (you'll need to implement this part)
  par_list <- setNames(as.list(par_vec), par_structure)
  fitted_values <- calc_mu(pars = par_list, 
        n_overstory_trees = disp_data$n_overstory_trees, 
        dist_vector = disp_data$dist_vector, 
        overstory_tree_size = disp_data$overstory_tree_size, 
        pos = disp_data$pos, 
        seedling_plot_area = disp_data$seedling_plot_area, 
        fecundity_fun = settings$fecundity_fun, 
        disp_kernel = setting$disp_kernel)
  
  # Structure the return list
  result <- list(
    # User-friendly named results
    estimates = estimates,
    negloglik = fit$value,
    fitted.values = fitted_values,
    call = cl,
    
    # Optimization details
    convergence = fit$convergence,
    message = fit$message,
    counts = fit$counts,
    
    # Raw optim output (for advanced users/debugging)
    optim_output = fit,
    
    # Model metadata
    model_info = list(
      parameter_structure = par_structure,
      settings = settings,
      n_parameters = length(par_structure)
    )
  )
  
  # Optionally add a class for custom print/summary methods
  class(result) <- "fit_model_ml"

  return(results)
}

get_disp_data <- function(dataset_name, data_dir) # corresponding data files in datadir/prepped-for-stan/{dataset_name}
  { 
  
  # -- Load prepped dataset (corresponding data files in datadir/prepped-for-stan/{dataset_name})
  prepped_data_dir = file.path(data_dir, "prepped-for-stan", dataset_name)
  
  seedling_plot_area = read_file(file.path(prepped_data_dir, "plot-area.txt")) |> 
      as.numeric()
  dist_vector = read_lines(file.path(prepped_data_dir, "dist-vector.txt")) |>
      as.numeric() |>
      as.vector()
  overstory_treesize_vector = read_lines(file.path(prepped_data_dir,
      "overstory-treesize-vector.txt")) |>
      as.numeric() |>
      as.vector()
  seedling_counts = read_lines(file.path(prepped_data_dir, "seedling-counts.txt")) |>
      as.numeric() |>
      as.vector()
  n_overstory_trees = read_lines(file.path(prepped_data_dir, "n-overstory-trees.txt")) |>
      as.numeric() |>
      as.vector()
  pos = read_lines(file.path(prepped_data_dir, "pos.txt")) |>
      as.numeric() |>
      as.vector()
  
  ## Compile data and priors into list for model fitting 
  
  data_list <- lst(
      seedling_plot_area,
      n_overstory_trees,
      n_seedling_plots = length(seedling_counts),
      overstory_tree_size = overstory_treesize_vector,
      seedling_counts,
      dist_vector,
      obs = length(dist_vector),
      pos
  )

  # Check for missing data
  if (any(is.na(unlist(data_list)))) stop("Missing values in data.")
  
  return(data_list)
}

#### Test it out 

library(here)
library(tidyverse)

# Set data directory -- detect whether on Jetstream vs Andrew's machine and set accordingly
if (grep("latimer", here()) == 1) data_dir = readLines(here("data_dir_andrew.txt"), n = 1) else data_dir = readLines(here("data_dir.txt"), n = 1)


# Load dataset from one fire 
disp_data_dir = file.path(data_dir, "max500")
disp_data = get_disp_data(dataset_name = "delta-FIRS", data_dir = data_dir)

# Or instead load a simulated dataset 
#seedling_plot_area = 201
#n_seedling_plots = n_plots

# Specify settings for model 
settings = list(lik_distrib = "pois", disp_kernel = "exppow", fecundity_fn = "linear", optimizer= "BFGS")

# Specify parameters and starting values 
pars = list(a = 30, b = 5, k = 0.8, theta = NULL)


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



