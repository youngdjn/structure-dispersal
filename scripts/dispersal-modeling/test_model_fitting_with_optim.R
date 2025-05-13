# Test whether ML dispersal model will converge using optim

# Start with an example simple model ()

# Fecundity function (vectorized calculation for a single plot)
q_fun <- function(b, n_overstory_trees, overstory_tree_size) { 
  q = b * overstory_tree_size # fecundity equation
  return(q)
}

# Dispersal kernel 2D function (vectorized calculation for a single plot
disp_prob <- function(k, a, n_overstory_trees, dist_vector) { 
  kernel_prob = exp(-pow(dist_vector/a, k))*k / (2*pi()*square(a)*tgamma(2/k)) # apply exponential power kernel for all trees associated with a given plot
  return(kernel_prob)
}
  
# Calculate expected/fitted seedlings per plot 
calc_mu <- function(k, a, n_overstoy_trees, overstory_tree_size, pos) {
     for(i in 1:n_seedling_plots){

          mu[i] = sum( disp_prob(k, a, n_overstory_trees[i], 
                    segment(dist_vector, pos[i], n_overstory_trees[i])) * 
                    q_fun(b, n_overstory_trees[i], 
                      overstory_tree_size[pos[i]:(pos[i] + n_overstory_trees[i]))  * 
                    seedling_plot_area) # plot area
     }
  return(mu)
}




  a = exp(alpha);
  k = kappa;
  b = exp(mu_beta); // fecundity multiplier parameter prior is on log(b)
