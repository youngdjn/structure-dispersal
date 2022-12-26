### Functions for getting seedling density values from fitted dispersal kernels and visualizing them

library(tidyverse)
library(rstan)
library(here)
library(sf)
library(furrr)


## Convenience functions ####
source(here("scripts/convenience_functions.R"))



## Function to get tree fecundity from mu_beta (the log of the fecundity scalar b) and tree size
## If mu_beta is a vector, it is a set of samples of the parameter from the model
## If overstory_tree_size is a vector, it is a set of sizes of multiple trees
q_fun = function(mu_beta, overstory_tree_size) {

  # get fecundity scalar b from mu_beta (its log)
  b = exp(mu_beta)
    
  # expand b and tree size (both vectors) into matrices that can be multiplied
  # one row for each tree, one column for each model sample
  b_matrix = matrix(b, byrow=TRUE, nrow=length(overstory_tree_size), ncol = length(mu_beta))
  overstory_tree_size_matrix = matrix(overstory_tree_size, byrow=FALSE, nrow=length(overstory_tree_size), ncol = length(mu_beta))
  
  ## here is where the actual fecundity calculation is performed
  q = b_matrix * overstory_tree_size_matrix
  
  # if this function was passed a single overstory tree size value (and thus q has 1 row), return a vector, not a matrix
  if(length(overstory_tree_size) == 1) {
    q = q[1,]
  }
  
  return(q)
}

## Functions to compute a dispersal kernel density value at a specific distance from one tree
calc_kern_2Dt = function(samples, r) {
  kern = samples$k / (pi*samples$a) * (1 + r^2/samples$a)^(-1-samples$k) # 2Dt
}

calc_kern_exppow = function(samples, r) {
  kern = samples$k / (2*pi * samples$a^2 * gamma(2/samples$k)) * exp(-(r / samples$a)^samples$k) # exppow
}

## Function to return the dispersal function based on its name
select_kernel_function = function(disp_mod) {
  
  if(disp_mod == "exppow") {
    kernel_function = calc_kern_exppow
  } else if(disp_mod == "2Dt") {
    kernel_function = calc_kern_2Dt
  } else {
    stop("Specified dispersal model type does not have a kernel function ('calc_kern_...') defined for it.")
  }
  
  return(kernel_function)
}


## Function to summarize a fitted kernel from a single tree for a given stan model,
##  as well as a fitted "seedling shadow" for a single tree of average size
get_fitted_kernel = function(dataset_name, disp_mod, err_mod) {
  
  ## Get tree size from the modeled dataset
  prepped_data_dir = datadir(paste0("prepped-for-stan/", dataset_name))
  overstory_tree_size = read_lines(paste0(prepped_data_dir, "/overstory-tree-size.txt")) %>% as.numeric
  
  ## Load the fitted model and get the parameter samples
  model_filename = paste0(datadir("stan-models/"), "stanmod_", dataset_name,"_",disp_mod, "_", err_mod,".rds")
  model_fit = readRDS(model_filename)
  samples = extract(model_fit)
  
  # Pick the kernel function based on the specified disp_mod
  kernel_function = select_kernel_function(disp_mod)
  
  ## Get the fitted kernel along a range of distances from the tree
  r = 0:500
  kern_out = sapply(r,kernel_function, samples = samples)
  kern_out = t(kern_out) # one row for each distance from the tree, one column for each model sample
  
  ## Summarize the kernel as the median and credible interval along a range of distances from the tree
  summarized_kernel = data.frame (r = r,
                             fit = apply(kern_out,1,mean),
                             lwr = apply(kern_out,1,quantile,probs=c(0.25)),
                             upr = apply(kern_out,1,quantile,probs=c(0.75)),
                             disp_mod = disp_mod)
  #Q: Should fit be summarized by mean or median?
  
  # Summarize a fitted "seedling shadow" from a single tree of average size (incorporates fitted fecundity estimation)
  mean_tree_size = mean(overstory_tree_size)
  fecundity_out = sapply(samples$mu_beta, q_fun, overstory_tree_size = mean_tree_size)
  seeds_out = t(t(kern_out) * fecundity_out) # one row for each distance from the tree, one column for each model sample
  summarized_seedlingshadow = data.frame (r = 0:500,
                              fit = apply(seeds_out,1,mean),
                              lwr = apply(seeds_out,1,quantile,probs=c(0.25)),
                              upr = apply(seeds_out,1,quantile,probs=c(0.75)),
                              disp_mod = disp_mod)
  
  return(list(kernel = summarized_kernel, shadow = summarized_seedlingshadow))
}


#### Predict seedling density at each plot ####

### Function to predict seedl dens for a plot by summing the contributions of all trees
predict_seedl_plot = function(samples, tree_plot_dists, overstory_tree_size) {

  # Pick the kernel function based on the specified disp_mod
  kernel_function = select_kernel_function(disp_mod)
  
  ## Get the fitted kernel samples for each tree distance
  kern_out = sapply(tree_plot_dists,kernel_function, samples = samples)
  
  ## If it's a matrix (i.e. there are multiple kernel samples), need to transpose so we have one row for each tree, one column for each model sample
  if(!is.null(dim(kern_out))) {
    kern_out = t(kern_out) 
  }
  # But if it's a vector, that means we supplied only a single "sample" (a point estimate of model fit), so don't transpose or it will turn the vector to a matrix
  
  ## Get the fitted fecundity for each tree
  fecundity_out = q_fun(samples$mu_beta, overstory_tree_size) # matrix with one row per tree, one column per model sample
  
  ## Summarize the seed rain reaching the plot summed across all trees' contributions
  seeds_out_bytree = kern_out * fecundity_out # one row for each tree, one column for each model sample
  seeds_out = colSums(seeds_out_bytree)
  summarized_seedlingshadow = data.frame(fit = mean(seeds_out),
                                          lwr = quantile(seeds_out,probs=0.25),
                                          upr = quantile(seeds_out,probs=0.75))
  
  return(summarized_seedlingshadow)
}
