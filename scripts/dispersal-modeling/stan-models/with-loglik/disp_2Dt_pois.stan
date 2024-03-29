// Seed dispersal model with 2Dt kernel

functions {
  
    // Calculate expected number of seeds per tree given tree size
    vector q_fun(real b, int n_overstory_trees, vector overstory_tree_size) { 
        vector[n_overstory_trees] q;
        q = b * overstory_tree_size; // equation that converts from tree size to seeds; can be altered if info available on functional form -- note the form of this will also depend on how size is measured (height vs dbh vs basal area)
        // A standard alternative form: q = b * pow(overstory_tree_size, zeta), where zeta is often set at 2 or sometimes 8/3 when size is in terms of dbh, or can be fitted
        // Or adding an intercept: q = b * (pow(overstory_tree_size, zeta) - eta)
        return(q);
    }
  
    //  Calculate expected number of seedlings per plot 
    vector calc_mu(real a, real k, real mu_beta,
              real seedling_plot_area, int n_overstory_trees, int n_seedling_plots, 
              matrix r, 
              vector overstory_tree_size) { 
              
        real b; // fecundity multiplier parameter
        matrix[n_seedling_plots, n_overstory_trees] disp_kern; // Dispersal kernel
        vector[n_seedling_plots] mu;

        for (j in 1:n_overstory_trees) {
            for (i in 1:n_seedling_plots) {
                disp_kern[i, j] = k / (pi() * a) * 
                                        pow(1 + square(r[i, j]) / a, -1-k);
            }
        }
        
        b = exp(mu_beta); // fecundity multiplier parameter has lognormal prior via normal prior on mu_beta
            
        mu = seedling_plot_area * disp_kern * q_fun(b, n_overstory_trees, overstory_tree_size); 

        return(mu);
    }
}

data {
    // Area of seed traps (meters)
    real<lower=0> seedling_plot_area; 
    
    // Number of trees and number of seedling plots
    int<lower=1> n_overstory_trees;
    int<lower=1> n_seedling_plots;

    // Distance matrix (seedling plots to overstory trees)
    matrix<lower=0>[n_seedling_plots, n_overstory_trees] r;

    // Size of overstory trees
    vector<lower=0>[n_overstory_trees] overstory_tree_size; 
    
    // Number of seedlings in plots
    int<lower=0> seedling_counts[n_seedling_plots];
    
    // Hyperparameters for parameter priors
    real p_alpha[2];
    real p_inv_k_real[2];
    real p_mu_beta[2];
}

parameters {
    real alpha; // (Log) of 2Dt scale parameter
    real inv_k_real; // Logit transform of inverse of k
    real mu_beta; // Mean log of b
}

transformed parameters {
    real a; // Scale parameter
    real k; // Shape parameter
    a = exp(alpha);
    k = inv(2 * inv_logit(inv_k_real));
}

model {
    vector[n_seedling_plots] mu; // Mean number of seedlings per plot
    
    alpha ~ normal(p_alpha[1], p_alpha[2]);
	  inv_k_real ~ normal(p_inv_k_real[1], p_inv_k_real[2]);
    mu_beta ~ normal(p_mu_beta[1], p_mu_beta[2]);
    
    mu = calc_mu(a, k, mu_beta, seedling_plot_area, n_overstory_trees, n_seedling_plots, r,
                overstory_tree_size);
                
    seedling_counts ~ poisson(mu);
}

generated quantities {
    vector[n_seedling_plots] log_lik;
    int pval;
    int tot_seedlings;
    int nnz;
    real rmode;
    real rmed;
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
    rmed = sqrt(a * (pow(2, 1/k) - 1));
    rmean = sqrt(pi() * a) / 2 * tgamma(k - 0.5) / tgamma(k);
}
