// Seed dispersal model with exponential power kernel

functions {
  
    // Calculate expected number of seeds per tree given tree size
    vector q_fun(real b, real zeta, int n_overstory_trees, vector overstory_tree_size) { 
        vector[n_overstory_trees] q;
        q = b * pow(overstory_tree_size, zeta);
        //q = b * overstory_tree_size; // equation that converts from tree size 
         return(q);
    }    
    
    // Calculate 2d dispersal kernel values 
    vector disp_prob(real k, real a, int n_overstory_trees, vector dist_vector) { 
      vector[n_overstory_trees] kernel_prob;
      kernel_prob = exp(-pow(dist_vector/a, k))*k / (2*pi()*square(a)*tgamma(2/k)); // apply exponential power kernel for all trees associated with a given plot
      return(kernel_prob);
    }
}

data {  
    real seedling_plot_area; // Area of seed traps (meters)
    int<lower=1> n_seedling_plots;    // Number of seedling plots
    int obs; // Number of tree-plot combinations in data
    int<lower=0> n_overstory_trees[n_seedling_plots]; // Overstory tree count for each plot 
    int pos[n_seedling_plots];    // Index of where each seedling plot starts in dist_vector
    vector[obs] dist_vector;    // Plot-to-tree distances in vector form
    vector[obs] overstory_tree_size; // Overstory tree size (meters) in vector form
    int<lower=0> seedling_counts[n_seedling_plots]; // Observed seedling counts

    // Hyperparameters for parameter priors
    real p_alpha[2];
    real p_kappa[2];
    real p_mu_beta[2];
}

parameters {
    real alpha; // alpha parameter (related to scale)
    real kappa; // shape parameter
    real mu_beta; // Mean log of b (fecundity multiplier)
    real<lower=0> zeta; // exponent for fecundity model
}

transformed parameters {
    real a; // Scale parameter
    real k; // Shape parameter
    vector[n_seedling_plots] log_lik;
    vector[n_seedling_plots] mu; // predicted number of seedlings per plot
    real b; // fecundity multiplier parameter
    
    a = exp(alpha);
    k = kappa;
    b = exp(mu_beta); // fecundity multiplier parameter prior is on log(b)

    // for each plot, get the vector of kernel values (seed contribution of each tree), summed across all trees (with sum function)
    for(i in 1:n_seedling_plots){

          mu[i] = sum( disp_prob(k, a, n_overstory_trees[i], segment(overstory_tree_size, pos[i], n_overstory_trees[i])) .* // Schurr's generalized exponential kernel
            q_fun(b, zeta, n_overstory_trees[i], segment(overstory_tree_size, pos[i], n_overstory_trees[i]) ) ) * // seeds per tree based on size
            seedling_plot_area; // plot area

       	  log_lik[i] = poisson_lpmf(seedling_counts[i] | mu[i]); // track for loo
    }
}

model {
    // Priors
    alpha ~ normal(p_alpha[1], p_alpha[2]);
	  kappa ~ gamma(p_kappa[1], p_kappa[2]);
    mu_beta ~ normal(p_mu_beta[1], p_mu_beta[2]);
    zeta ~ normal(1, 0.2); // give fecundity exponent a truncated normal prior centered at 1 
    
    // Likelihood
    seedling_counts ~ poisson(mu);
}
