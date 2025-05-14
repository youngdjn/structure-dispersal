# Implement ML fitting of inverse dispersal model using optim 
# Following example from Frank Schurr 

# Start with an example simple model (exppow or 2Dt with one-parameter fecundity function)

# Fecundity function (vectorized calculation for a single plot)
q_fun <- function(b, n_overstory_trees, overstory_tree_size) { 
  q = b * overstory_tree_size # fecundity equation
  return(q)
}

# Dispersal kernel function (vectorized calculation for a single plot)
disp_prob <- function(k, a, n_overstory_trees, dist_vector) { 
  kernel_prob = exp(-pow(dist_vector/a, k))*k / (2*pi()*square(a)*tgamma(2/k)) # apply exponential power kernel for all trees associated with a given plot
  return(kernel_prob)
}
  
# Calculate expected/fitted seedlings per plot 
calc_mu <- function(k, a, b, n_overstory_trees, dist_vector, overstory_tree_size, pos, seedling_plot_area) {
  n_seedling_plots = length(n_overstory_trees) 
  for(i in 1:n_seedling_plots){
      segment_start = pos[i]
      segment_end = pos[i] + n_overstory_trees[i]
      mu[i] = sum( disp_prob(k, a, n_overstory_trees[i], 
                dist_vector[segment_start:segment_end]) * 
                q_fun(b, n_overstory_trees[i], 
                  overstory_tree_size[segment_start:segment_end]) * 
                seedling_plot_area) # plot area
     }
  return(mu)
}

# Function to get the negative log likelihood for a set of parameter values
calc_negloglik <- function(k, a, b, n_overstory_trees, dist_vector, overstory_tree_size, pos, seedling_counts, seedling_plot_area) {
  mu = calc_mu(k, a, b, n_overstoy_trees, dist_vector, overstory_tree_size, pos) 
  negloglik = -sum(dpois(x = seedling_counts, lambda = mu, log=TRUE))
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
  #
  #
  #VALUE: an object of class "him" which is a list with components
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
  
  fit <- optim(pars.init, calc_negloglik, n_overstory_trees = n_overstory_trees, dist_vector = dist_vector, overstory_tree_size = overstory_tree_size, pos = pos, seedling_counts = seedling_counts, seedling_plot_area = seedling_plot_area)
  
  if (fit$convergence!=0) warning("Fit did not converge!")
  
  estimates<-list(b=fit$par[1],
                  k=fit$par[2],
                  a=fit$par[3])

  fv <- calc_mu(b = fit$par[1], k = fit$par[2], a = fit$par[3], n_overstory_trees = n_overstory_trees, dist_vector = dist_vector, overstory_tree_size = overstory_tree_size, pos = pos, seedling_counts = seedling_counts, seedling_plot_area = seedling_plot_area)
  
  res<-list(estimates=estimates,negloglik=fit$value,fitted.values=fv,call=cl,
            counts=fit$counts,convergence=fit$convergence,message=fit$message)
  
  return(res)
}

#### Test it out 