# Implement ML fitting of inverse dispersal model using optim 
# Following example from Frank Schurr 

# Start with an example simple model (exppow, with a one-parameter fecundity function)

# Fecundity function (vectorized calculation for a single plot)
q_fun <- function(b, overstory_tree_size) { 
  q = b * overstory_tree_size # fecundity equation
  return(q)
}

# Dispersal kernel function (vectorized calculation for a single plot)
disp_prob <- function(k, a, dist_vector) { 
  kernel_prob = exp(-(dist_vector/a)^k)*k / (2 * pi * a^2 * gamma(2/k)) # apply exponential power kernel for all trees associated with a given plot
  return(kernel_prob)
}
  
# Calculate expected/fitted seedlings per plot 
calc_mu <- function(k, a, b, n_overstory_trees, dist_vector, overstory_tree_size, pos, seedling_plot_area) {
  n_seedling_plots = length(n_overstory_trees) 
  mu = vector(mode = "numeric", length = n_seedling_plots)
  for(i in 1:n_seedling_plots){
      segment_start = pos[i]
      segment_end = pos[i] + n_overstory_trees[i] - 1
      mu[i] = sum( disp_prob(k, a, dist_vector[segment_start:segment_end]) * 
                q_fun(b, overstory_tree_size[segment_start:segment_end]) * 
                seedling_plot_area) # plot area
     }
  return(mu)
}

# Function to get the negative log likelihood for a set of parameter values
calc_negloglik <- function(pars, n_overstory_trees, dist_vector, overstory_tree_size, pos, seedling_counts, seedling_plot_area) {
  b = pars[1]
  k = pars[2]
  a = pars[3]
  mu = calc_mu(k, a, b, n_overstory_trees, dist_vector, overstory_tree_size, pos, seedling_plot_area) 
  negloglik = -sum(dpois(x = seedling_counts, lambda = mu, log=TRUE))
  return(negloglik)
}


### Function to fit the model using optim
fit_model_optim <- function(startpars, n_overstory_trees = n_overstory_trees, dist_vector = dist_vector, overstory_tree_size = overstory_tree_size, pos = pos, seedling_counts = seedling_counts, seedling_plot_area = seedling_plot_area)
{
  #ML fitting of an inverse model with source and path effects
  #ARGUMENTS:
  #
  #startpars: a list with elements b, k, a giving starting values for parameters
  #           of the dispersal (k, a) and fecundity models (b)
  #
  # other variables are the overstory tree sizes, distances from each tree to each plot, in sparse/ragged form, pos which points to the starting point of each plot's data in the vectors, plus the number of trees in each plot.
  #
  #VALUE: list with components:
  #
  #estimates: a named list of maximum likelihood parameter estimates,
  #           for each component parameters are in the same order as in startpars
  #
  #negloglik: the negative log likelihood of the fitted inverse model
  #
  #fitted.values: the fitted values for each seed trap
  #
  #call:      the matched call
  #
  #counts,convergence and message: the respective output of the optim function
  #           used to fit the inverse models
  
  cl <- match.call()

  b.pars <- startpars$b
  k.pars <- startpars$u
  a.pars <- startpars$p
  pars.init <- c(b, k, a)
  
  fit <- optim(pars.init, method = "SANN", calc_negloglik, n_overstory_trees = n_overstory_trees, dist_vector = dist_vector, overstory_tree_size = overstory_tree_size, pos = pos, seedling_counts = seedling_counts, seedling_plot_area = seedling_plot_area) # Use simulated annealing - slow but thorough
  
  if (fit$convergence!=0) warning("Fit did not converge!")
  
  estimates<-list(b=fit$par[1],
                  k=fit$par[2],
                  a=fit$par[3])

  fv <- calc_mu(b = fit$par[1], k = fit$par[2], a = fit$par[3], n_overstory_trees = n_overstory_trees, dist_vector = dist_vector, overstory_tree_size = overstory_tree_size, pos = pos, seedling_plot_area = seedling_plot_area)
  
  res<-list(estimates=estimates,negloglik=fit$value,fitted.values=fv,call=cl,
            counts=fit$counts,convergence=fit$convergence,message=fit$message)
  
  return(res)
}

get_disp_data <- function(dataset_name) # corresponding data files in datadir/prepped-for-stan/{dataset_name}
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
  
  ## Compile data and priors into list for Stan
  
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

disp_data = get_disp_data(dataset_name = "delta-PINES")

calc_negloglik(pars = pars.init, 
    n_overstory_trees = disp_data$n_overstory_trees, 
    dist_vector = disp_data$dist_vector, 
    overstory_tree_size = disp_data$overstory_tree_size, 
    pos = disp_data$pos, 
    seedling_counts = disp_data$seedling_counts, 
    seedling_plot_area = disp_data$seedling_plot_area)

startpars = list(b = 1, k = 1, a = 20)

m = fit_model_optim(startpars, n_overstory_trees = n_overstory_trees, dist_vector = dist_vector, overstory_tree_size = overstory_tree_size, pos = pos, seedling_counts = seedling_counts, seedling_plot_area = seedling_plot_area)

m$estimates
m$negloglik

# Using simulated annealing works to fit the model!

# plot fitted vs observed 
obspred_data <- data.frame(fitted = m$fitted.values, observed = disp_data$seedling_counts)
ggplot(obspred_data, aes(x = log(fitted), y = log(observed+0.5))) + geom_point()

# plot dispersal kernel based on fitted parameters
kernel_plot_data <- data.frame(Distance = 1:500, Probability = disp_prob(k = m$estimates$k, a = m$estimates$a, dist_vector = 1:500))
ggplot(kernel_plot_data, aes(x = Distance, y = Probability)) + geom_line()

## Next steps: 
# Visualize likelihood surface 
# Check that model can recover params from simulation 
# Figure out how to bootstrap or estimate parameter uncertainties 
