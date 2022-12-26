## Data management approach for this repo

See the top-level README for this repo for an overview of the data management approach for this repo.

## Workflow for dispersal kernel modeling

1. [Only needed once per site-species combination] Prepare the field and drone data into the format for supplying to Stan. Performed by `01_prep-data-for-model.R` based on the parameters specified in the header of the file. Parameters include site, species, location of overstory tree map and regen plot data, regen plot size used at that site, and the function for obtaining the desired tree size metric from the drone-measured tree height. Saves the prepared data files in `{datadir}/prepped-for-stan/{dataset-name}`. This needs to be performed for each site-species-sizemetric combination.

1. Fit dispersal model using Stan. Performed by `02_fit-dispersal-kernel-stan.R` using the data files prepared in the prior step (referenced using the `{dataset-name}` from the prior step), along with the prior distributions, dispersal kernel functions, and count model families specified at the start of the script. Relies in Stan models specified in `stan-models/` and prior distributions defined in `priors/`.
