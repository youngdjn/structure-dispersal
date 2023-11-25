# This script runs one instance of the hierarchical Bayesian model for a specified dispersal kernel function, count model family, and dispersal dataset (e.g. site-species combo)
# It relies on a dataset (of overstory trees and seedling counts) compiled by the script 01_prep-data-for-model.R

library(tidyverse)
library(rstan)
# library(bayesplot) # for 'rhat' function

library(here)


fit_stan_model = function(dataset_name, # which dataset to model (corresponding data files in datadir/prepped-for-stan/{dataset_name})
                          disp_mod, # 2Dt or exppow
                          err_mod,  # pois only currently
                          n_warmup, # stan warmup iter
                          n_iter,   # stan iter includes warmup
                          n_chains, # stan n chains
                          n_cores) {  # stan n cores

  ## Load priors and arrange into list. Load prior values from the code repo. ##
  
  disp_priors <- read_csv("scripts/dispersal-modeling/dispersal-kernel-modeling/priors/disp_priors.csv") %>%
    filter(model == disp_mod) %>%
    select(-model)
  repr_priors <- read_csv("scripts/dispersal-modeling/dispersal-kernel-modeling/priors/repr_priors.csv") %>%
    filter(stage == "seedling") %>%
    select(-stage)
  priors <- bind_rows(disp_priors, repr_priors)
  
  if (err_mod != "nb") 
    priors <- filter(priors, param != "p_ri_theta")
  
  priors_list <- setNames(map2(priors$prior_mean, priors$prior_sd, c), 
                          priors$param)
  

  ## Load prepped dataset (corresponding data files in datadir/prepped-for-stan_ragged/{dataset_name}) ##
  
  prepped_data_dir = file.path(data_dir, "prepped-for-stan_ragged", dataset_name)
  
  seedling_plot_area = read_file(file.path(prepped_data_dir, "plot-area.txt")) %>% as.numeric
  dist_vector = read_lines(file.path(prepped_data_dir, "dist-vector.txt")) %>% as.numeric |> as.vector()
  htdiff_vector = read_lines(file.path(prepped_data_dir, "htdiff-vector.txt"))%>% as.numeric |> as.vector()
  overstory_treesize_vector = read_lines(file.path(prepped_data_dir, "overstory-treesize-vector.txt")) %>% as.numeric |> as.vector()
  seedling_counts = read_lines(file.path(prepped_data_dir, "seedling-counts.txt")) %>% as.numeric |> as.vector()
  n_overstory_trees = read_lines(file.path(prepped_data_dir, "n-overstory-trees.txt")) |> as.numeric() |> as.vector()
  pos = read_lines(file.path(prepped_data_dir, "pos.txt")) |> as.numeric() |> as.vector()
  
  ## Compile data and priors into list for Stan
  
  data_list <- lst(seedling_plot_area,
                   n_overstory_trees, 
                   n_seedling_plots = length(seedling_counts),
                   overstory_tree_size = overstory_treesize_vector,
                   seedling_counts,
                   dist_vector,
                   htdiff_vector,
                   obs = length(dist_vector),
                   pos)    

  data_list = c(data_list, priors_list)
  
  # Check for missing data
  if (any(is.na(unlist(data_list)))) stop("Missing values in data.")
  
  ####### Run Stan model and save samples #######
  options(mc.cores = n_cores)
  model_file <- paste0("scripts/dispersal-modeling/dispersal-kernel-modeling/stan-models/stan-models-with-loglik-and-heightdiff/disp_", disp_mod, "_", err_mod, ".stan")
  
  res <- stan(model_file, data = data_list, chains = n_chains, 
              warmup = n_warmup, iter = n_iter, cores = n_cores)
  
  model_dir = file.path(data_dir, "stan-models/")
  if(!file.exists(model_dir)) dir.create(model_dir)
  model_filename = paste0(model_dir, "stanmod_", dataset_name,"_",disp_mod, "_", err_mod,".rds")
  
  saveRDS(res,model_filename)
  
  return(res)

}
