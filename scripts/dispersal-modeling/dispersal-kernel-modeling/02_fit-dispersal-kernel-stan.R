# Fit one instance of the hierarchical Bayesian model for a specified dispersal kernel function, count model family, and dispersal dataset (e.g. site-species combo)
# It relies on a dataset (of overstory trees and seedling counts) that has already been compiled by the script 01_prep-data-for-model.R

library(here)

#data_dir = readLines(here("data_dir.txt"), n=1)
# set data directory for Andrew's local work 
data_dir = "/Users/latimer/Library/CloudStorage/Box-Box/str-disp_data"


# Main functions for the tasks in this script.
source(here("scripts/dispersal-modeling/dispersal-kernel-modeling/02_fit-dispersal-kernel-stan_functions.R"))

#### Fit a model for a specific site, disp function, error model, and stan parameters. See 02_fit-dispersal-kernel_functions.R for parameter definitions.
m = fit_stan_model (dataset_name = "valley-allsp-height-01",   # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
                disp_mod = "2Dt",                         # 2Dt or exppow
                err_mod = "pois",                         # pois only currently
                n_warmup = 500,                           # stan warmup iter
                n_iter = 2500,                             # stan iter, includes warmup
                n_chains = 4,                             # stan n chains
                n_cores = 8)                              # stan n cores

m = fit_stan_model (dataset_name = "chips-allsp-height-01",   # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
                disp_mod = "2Dt",                         # 2Dt or exppow
                err_mod = "pois",                         # pois only currently
                n_warmup = 500,                           # stan warmup iter
                n_iter = 2500,                             # stan iter, includes warmup
                n_chains = 4,                             # stan n chains
                n_cores = 8)                              # stan n cores

m = fit_stan_model (dataset_name = "crater-pipj-height-01",   # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
                disp_mod = "2Dt",                         # 2Dt or exppow
                err_mod = "pois",                         # pois only currently
                n_warmup = 500,                           # stan warmup iter
                n_iter = 2500,                             # stan iter, includes warmup
                n_chains = 4,                             # stan n chains
                n_cores = 8)                              # stan n cores

m = fit_stan_model (dataset_name = "delta-allsp-height-01",   # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
                disp_mod = "2Dt",                         # 2Dt or exppow
                err_mod = "pois",                         # pois only currently
                n_warmup = 500,                           # stan warmup iter
                n_iter = 2500,                             # stan iter, includes warmup
                n_chains = 4,                             # stan n chains
                n_cores = 4)                              # stan n cores

#### Fit a model for a specific site, disp function, error model, and stan parameters. See 02_fit-dispersal-kernel_functions.R for parameter definitions.
m = fit_stan_model (dataset_name = "crater-pipj-height-01",   # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
                disp_mod = "exppow",                         # 2Dt or exppow
                err_mod = "pois",                         # pois only currently
                n_warmup = 500,                           # stan warmup iter
                n_iter = 2500,                             # stan iter, includes warmup
                n_chains = 6,                             # stan n chains
                n_cores = 6)                              # stan n cores

#### Fit a model for a specific site, disp function, error model, and stan parameters. See 02_fit-dispersal-kernel_functions.R for parameter definitions.
m = fit_stan_model (dataset_name = "valley-allsp-height-01",   # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
                disp_mod = "exppow",                         # 2Dt or exppow
                err_mod = "pois",                         # pois only currently
                n_warmup = 500,                           # stan warmup iter
                n_iter = 2500,                             # stan iter, includes warmup
                n_chains = 6,                             # stan n chains
                n_cores = 6)                              # stan n cores


m = fit_stan_model (dataset_name = "chips-allsp-height-01",   # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
                disp_mod = "exppow",                         # 2Dt or exppow
                err_mod = "pois",                         # pois only currently
                n_warmup = 500,                           # stan warmup iter
                n_iter = 2500,                             # stan iter, includes warmup
                n_chains = 6,                             # stan n chains
                n_cores = 6)                              # stan n cores

m = fit_stan_model (dataset_name = "delta-allsp-height-01",   # which dataset to model (corresponding data files must be in datadir/prepped-for-stan/{dataset_name}), produced by 01_prep-data-for-model.R
                disp_mod = "exppow",                         # 2Dt or exppow
                err_mod = "pois",                         # pois only currently
                n_warmup = 500,                           # stan warmup iter
                n_iter = 2500,                             # stan iter, includes warmup
                n_chains = 6,                             # stan n chains
                n_cores = 6)                              # stan n cores




# Q: Separate models by site? Or combine sites, but need a sparse dist mat so we don't compare plots and trees from different sites, and need to specify a different plot size by site
    # Lisa Rosenthal knows how to do the sparse part