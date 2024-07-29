# Fit one instance of the hierarchical Bayesian model for a specified dispersal kernel function,
# count model family, and dispersal dataset (e.g. site-species combo). It relies on a dataset (of
# overstory trees and seedling counts) that has already been compiled by the script
# 01_prep-data-for-model.R

library(here)

data_dir = readLines(here("data_dir.txt"), n = 1)
# for Andrew's local work
data_dir = "/Users/latimer/Library/CloudStorage/Box-Box/str-disp_data"


# Main functions for the tasks in this script.
source(here("scripts/dispersal-modeling/02_fit-dispersal-kernel-stan_functions.R"))

# Fit a model for a specific site, disp function, error model, and stan parameters. See
# 02_fit-dispersal-kernel_functions.R for parameter definitions.
m = fit_stan_model(
  dataset_name = "delta-PINES", # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
  disp_mod = "2Dt", # 2Dt or exppow
  err_mod = "pois", # pois only currently
  n_warmup = 150, # stan warmup iter
  n_iter = 2500, # stan iter, includes warmup
  n_chains = 4, # stan n chains
  n_cores = 4 # stan n cores
)

m = fit_stan_model(
  dataset_name = "delta-FIRS", # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
  disp_mod = "2Dt", # 2Dt or exppow
  err_mod = "pois", # pois only currently
  n_warmup = 150, # stan warmup iter
  n_iter = 2500, # stan iter, includes warmup
  n_chains = 4, # stan n chains
  n_cores = 4 # stan n cores
)

m = fit_stan_model(
  dataset_name = "delta-PINES",
  disp_mod = "exppow",
  err_mod = "pois",
  n_warmup = 500,
  n_iter = 2500,
  n_chains = 4,
  n_cores = 4
)

m = fit_stan_model(
  dataset_name = "delta-FIRS",
  disp_mod = "2Dt",
  err_mod = "pois",
  n_warmup = 500,
  n_iter = 2500,
  n_chains = 4,
  n_cores = 4
)

m = fit_stan_model(
  dataset_name = "delta-FIRS",
  disp_mod = "exppow",
  err_mod = "pois",
  n_warmup = 500,
  n_iter = 2500,
  n_chains = 4,
  n_cores = 4
)

#### Fit models with specified dispersal model parameterizion, in addition to dispersal kernel, error model, and species and site #### 

# Model with multiplier and exponent 
m2 = fit_stan_model_fecund(dataset_name = "delta-PINES",   # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
                           disp_mod = "2Dt",                         # 2Dt or exppow
                           err_mod = "pois",                         # pois only currently
                           fecund_mod = "multiplier_exponent_noheight",                # multiplier, multiplier_exponent, multiplier_intercept, multiplier_exponent_intercept
                           n_warmup = 500,                           # stan warmup iter
                           n_iter = 2500,                             # stan iter, includes warmup
                           n_chains = 4,                             # stan n chains
                           n_cores = 4)    

loo(m1) # looic 4527
loo(m2) 
  