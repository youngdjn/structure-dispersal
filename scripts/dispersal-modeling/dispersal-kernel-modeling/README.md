## Data management approach for this repo

See the top-level README for this repo for an overview of the data management approach for this repo.

## Code organization

In this folder, for each main task related to dispersal kernels (e.g., data prep, model fitting, model inspection), there are two code files: one that defines the necessary functions, and one that calls the functions (with the appropriate parameters) to accomplish the tasks. In each pair, the filename starts the same, but the file containng the functions ends in `_functions`. For example, `01_prep-data-for-model.R` and `01_prep-data-for-model_functions.R`. The `_functions` script does not need to be opened or run because it is sourced by the other script in the pair.

If you want to step through (or debug) the code in the functions, especially when using RStudio, I recommend inserting `browser()` at a in the function where you want to start stepping line by line. When you call the function from any script, the code will run normally until it hits the `browser()` line and then pause, allowing you to inspect the environment at that stage and advance one line at a time (or resume normal execution).

The code files begin with a number indicating the order they are run in the workflow. The numbers are not sequential so as to leave room for other scripts in between.

## Workflow for dispersal kernel modeling

1. [Only needed once per site-species combination] Prepare the field and drone data into the format for supplying to Stan. Performed by `01_prep-data-for-model.R` based on the parameters specified in the header of the file. Parameters include site, species, location of overstory tree map and regen plot data, regen plot size used at that site, and the function for obtaining the desired tree size metric from the drone-measured tree height. Saves the prepared data files in `{datadir}/prepped-for-stan/{dataset-name}`. This needs to be performed for each site-species-sizemetric combination.

1. Fit dispersal model using Stan. Performed by `02_fit-dispersal-kernel-stan.R` using the data files prepared in the prior step (referenced using the `{dataset-name}` from the prior step), along with the specified prior distributions, dispersal kernel functions, and count model families. Relies on Stan models specified in `stan-models/` and prior distributions defined in `priors/`. Saves fitted Stan models to {datadir}/stan-models/.

1. Examine fitted dispersal kernel and fecundity models. Performed by `05_examine-fitted-kernel.R`. This includes plotting the fitted dispersal kernel functions, simulating seed input at a (real or hypothetical) plot, comparing fitted vs observed seedling densities, etc.
