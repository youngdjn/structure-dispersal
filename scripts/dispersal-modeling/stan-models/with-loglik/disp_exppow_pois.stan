// Seed dispersal model with exponential power kernel

functions {
  
    // Calculate expected number of seeds per tree given tree size
    vector q_fun(real b, int n_overstory_trees, vector overstory_tree_size) { 
        vector[n_overstory_trees] q;
        q = b * overstory_tree_size; // equation that converts from tree size to seeds; can be altered if info available on functional form -- note the form of this will also depend on how size is measured (height vs dbh vs basal area)
        // A standard alternative form: q = b * pow(overstory_tree_size, zeta), where zeta is often set at 2 or sometimes 8/3 when size is in terms of dbh, or can be fitted
        // Or adding an intercept: q = b * (pow(overstory_tree_size, zeta) - eta)
        return(q);
    }
}

data {  
 // Area of seed traps (meters)
    real seedling_plot_area; 
    
    // Number of seedling plots
    int<lower=1> n_seedling_plots;
    
    // Number of tree-plot combinations that are < 300 m
    int obs;

    // Number of overstory trees for each seedling plot 
    int<lower=0> n_overstory_trees[n_seedling_plots];

    // Index of where each seedling plot starts in dist_vector
    int pos[n_seedling_plots];

    // Pairwise distances (seedling plots to overstory trees), in vector form
    vector[obs] dist_vector;
    
    // Size of overstory trees
    vector[obs] overstory_tree_size;

    // Number of seedlings in plots
    int<lower=0> seedling_counts[n_seedling_plots];

    // Hyperparameters for parameter priors
    real p_alpha[2];
    real p_inv_k_real[2];
    real p_mu_beta[2];
}

parameters {
    real alpha; // alpha parameter (related to scale)
    real inv_k; // (Inv.) shape parameter
    real mu_beta; // Mean log of b
}

transformed parameters {
    real a; // Scale parameter
    real k; // Shape parameter
    a = exp(alpha - inv_k);
    k = inv(inv_k);

    vector[n_seedling_plots] log_lik;

    vector[n_seedling_plots] mu; // Mean number of seedlings per plot !!!CHECK: is it right to define mu here and not in model?

    real b; // fecundity multiplier parameter

    b = exp(mu_beta); // fecundity multiplier parameter has lognormal prior via normal prior on mu_beta

    // for each plot, get the vector of kernel values (seed contribution of each tree), summed across all trees (with sum function)
    for(i in 1:n_seedling_plots){
        
          //TODO: can make this easier to read by computing each term first?
          
          mu[i] = sum(k / (2*pi() * square(a) * tgamma(2/k)) * exp(- pow(segment(dist_vector, pos[i], n_overstory_trees[i]) / a, k))) .*
            q_fun(b, n_overstory_trees[i], segment(overstory_tree_size, pos[i], n_overstory_trees[i]) ) ) * // seeds per tree based on size
            seedling_plot_area; // area in which seeds land 

       	  log_lik[i] = poisson_lpmf(seedling_counts[i] | mu[i]);
            
 
          //TODO: where does the area (m) come into this expression besides seedling_plot_area?

    }

}


model {

    alpha ~ normal(p_alpha[1], p_alpha[2]);
	  inv_k_real ~ normal(p_inv_k_real[1], p_inv_k_real[2]);
    mu_beta ~ normal(p_mu_beta[1], p_mu_beta[2]);

    seedling_counts ~ poisson(mu);
}

/* generated quantities {
    vector[n_seedling_plots] log_lik;
    int pval;
    int tot_seedlings;
    int nnz;
    real rmode;
    real rmean;
    
    {
        //matrix[ntrap, nyear] mu;
        vector[n_seedling_plots] mu_v;
        int seedling_counts_sim[n_seedling_plots];
        vector[n_seedling_plots] ll_sim;
        mu_v = calc_mu(a, k, mu_beta, seedling_plot_area, n_overstory_trees, n_seedling_plots, r,
                overstory_tree_size);
                     
        //mu_v = to_vector(mu);
        nnz = 0;
        for (i in 1:n_seedling_plots) {
            log_lik[i] = poisson_lpmf(seedling_counts[i] | mu_v[i]);
            seedling_counts_sim[i] = poisson_rng(mu_v[i]);
            ll_sim[i] = poisson_lpmf(seedling_counts_sim[i] | mu_v[i]);
            if (seedling_counts_sim[i] > 0) nnz = nnz + 1;
        }
        pval = sum(ll_sim) < sum(log_lik);
        tot_seedlings = sum(seedling_counts_sim);
    }
    
    rmode = 0;
    rmean = a * tgamma(3 * inv_k) / tgamma(2 * inv_k);
} */
