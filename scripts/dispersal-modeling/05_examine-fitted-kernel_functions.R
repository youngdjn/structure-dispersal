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
q_fun = function(mu_beta, zeta = 1, overstory_tree_size) {
  # get fecundity scalar b from mu_beta (its log)
  b = exp(mu_beta)

  # expand b and tree size (both vectors) into matrices that can be multiplied
  # one row for each tree, one column for each model sample
  b_matrix = matrix(b, byrow = TRUE, nrow = length(overstory_tree_size), ncol = length(mu_beta))
  overstory_tree_size_matrix = matrix(overstory_tree_size,
                                      byrow = FALSE,
                                      nrow = length(overstory_tree_size),
                                      ncol = length(mu_beta))

  ## here is where the actual fecundity calculation is performed
  q = b_matrix * (overstory_tree_size_matrix^zeta)

  # if this function was passed a single overstory tree size value (and thus q has 1 row), return a
  # vector, not a matrix
  if (length(overstory_tree_size) == 1) {
    q = q[1, ]
  }

  return(q)
}

## Functions to compute a dispersal kernel density value at a specific distance from one tree
calc_kern_2Dt = function(samples, r) {
  kern = samples$k / (pi * samples$a) * (1 + r^2 / samples$a)^(-1 - samples$k)
  return(kern)
}

calc_kern_exppow = function(samples, r) {
  kern = samples$k / (2 * pi * samples$a^2 * gamma(2 / samples$k)) *
    exp(-(r / samples$a)^samples$k)
  return(kern)
}

## Function to return the dispersal function based on its name
select_kernel_function = function(disp_mod) {
  if (disp_mod == "exppow") {
    kernel_function = calc_kern_exppow
  } else if (disp_mod == "2Dt") {
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
  prepped_data_dir = file.path(data_dir, "prepped-for-stan", dataset_name)
  overstory_tree_size = read_lines(paste0(prepped_data_dir, "/overstory-treesize-vector.txt")) |>
    as.numeric()

  ## Load the fitted model and get the parameter samples
  model_filename = file.path(data_dir, "stan-models",
                             paste0("stanmod_", dataset_name, "_", disp_mod, "_", err_mod, ".rds"))
  model_fit = readRDS(model_filename)
  samples = rstan::extract(model_fit)

  ## Get LOOIC and kernel params
  a = loo(model_fit)
  looic = a$estimates["looic", 1]
  cat("\nLOOIC:", looic, "\n")

  # kernel params
  a = quantile(samples$a, probs = c(0.025, .5, 0.975)) |> round(4)
  a_format = paste0(a[2], " (", a[1], ", ", a[3], ")")
  cat("\nKernel param a:", a_format)
  k = quantile(samples$k, probs = c(0.025, .5, 0.975)) |> round(3)
  k_format = paste0(k[2], " (", k[1], ", ", k[3], ")")
  cat("\nKernel param k:", k_format)

  # Pick the kernel function based on the specified disp_mod
  kernel_function = select_kernel_function(disp_mod)

  ## Get the fitted kernel along a range of distances from the tree
  r = 0:500
  kern_out = sapply(r, kernel_function, samples = samples)
  kern_out = t(kern_out) # one row for each distance from the tree, one column for each model sample

  # Summarize the kernel as the median and 95% credible interval along a range of distances from the
  # tree
  summarized_kernel = data.frame(
    r = r,
    fit = apply(kern_out, 1, median),
    lwr = apply(kern_out, 1, quantile, probs = c(0.025)),
    upr = apply(kern_out, 1, quantile, probs = c(0.975)),
    disp_mod = disp_mod
  )
  # Q: Should fit be summarized by mean or median?

  # Summarize a fitted "seedling shadow" from a single tree of average size (incorporates fitted
  # fecundity estimation)
  mean_tree_size = mean(overstory_tree_size)
  fecundity_out = sapply(samples$mu_beta, q_fun, overstory_tree_size = mean_tree_size)
  # seed rain: one row for each distance from the tree, one column for each model sample:
  seeds_out = t(t(kern_out) * fecundity_out)
  # median fit and 95% CI
  summarized_seedlingshadow = data.frame(
    r = 0:500,
    fit = apply(seeds_out, 1, median),
    lwr = apply(seeds_out, 1, quantile, probs = c(0.025)),
    upr = apply(seeds_out, 1, quantile, probs = c(0.975)),
    disp_mod = disp_mod
  )

  return(list(kernel = summarized_kernel, shadow = summarized_seedlingshadow, model = model_fit))
}


### Function to predict seedl dens for a plot by summing the contributions of all trees
predict_seedl_plot = function(tree_dists, elevdiffs, tree_sizes, samples) {
  # Pick the kernel function based on the specified disp_mod
  kernel_function = select_kernel_function(disp_mod)

  ## Get the fitted kernel samples for each tree distance
  kern_out = sapply(tree_dists, kernel_function, samples = samples)

  # If it's a matrix (i.e. there are multiple kernel samples), need to transpose so we have one row
  # for each tree, one column for each model sample
  if (!is.null(dim(kern_out))) {
    kern_out = t(kern_out)
  }
  # But if it's a vector, that means we supplied only a single "sample" (a point estimate of model
  # fit), so don't transpose or it will turn the vector to a matrix

  ## Get the fitted fecundity for each tree
  # ! Andrew, note you'll need to add the zeta parameter samples to the q_fun call once you have
  # fitted stan models that include it.
  # Overstory tree size is a matrix with one row per tree, one column per model sample
  fecundity_out = q_fun(mu_beta = samples$mu_beta, zeta = 1,
                        overstory_tree_size = tree_sizes)

  ## Get the elevation difference scalar
  elev_diff_scalar = exp(samples$b1_ht * elevdiffs)

  ## Summarize the seed rain reaching the plot summed across all trees' contributions.
  # seeds_out_bytree is a matrix with one row for each tree, one column for each model sample
  seeds_out_bytree = kern_out * fecundity_out * elev_diff_scalar 
  seeds_out = colSums(seeds_out_bytree)
  summarized_seedlingshadow = data.frame(
    fit = mean(seeds_out),
    lwr = quantile(seeds_out, probs = 0.25),
    upr = quantile(seeds_out, probs = 0.75)
  )

  return(summarized_seedlingshadow)
}


plot_fitted_observed = function(fitted_observed_plot_seedl, plot_size_ha,
                                fitted_plot_area = 10000, ylim) {
  # Get seedling densities in seedl/ha
  fitted_observed_plot_seedl_transf = fitted_observed_plot_seedl %>%
    mutate(
      obs = obs / plot_size_ha, # from seedl per plot to seedl per ha
      fit = fit * fitted_plot_area
    ) # from seedl per sq m to seedl per ha

  ## Set zeroes to nonzero for plotting on log-scale axes
  # get the smallest nonzero number
  min = min(fitted_observed_plot_seedl_transf$obs[fitted_observed_plot_seedl_transf$obs > 0])
  # set to half
  zero_value = min / 2
  # substitute it for zero
  fitted_observed_plot_seedl_transf = fitted_observed_plot_seedl_transf |>
    mutate(obs_nonzero = ifelse(obs == 0, zero_value, obs))

  ## calc the MAE and R-sq
  mae = mean(abs((fitted_observed_plot_seedl_transf$obs) - (fitted_observed_plot_seedl_transf$fit)))
  cat("MAE:", mae, "\n")

  rsq = cor(log(fitted_observed_plot_seedl_transf$obs_nonzero),
            log(fitted_observed_plot_seedl_transf$fit))^2
  cat("Rsq:", rsq, "\n")

  breaks = c(1, 10, 100, 1000, 10000)
  breaks = breaks[breaks > zero_value]
  breaks = c(zero_value, breaks)
  labels = as.character(breaks)
  labels[1] = "[0]"

  minor_breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 30, 400,
                   500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000,
                   10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000)

  # Plot with log axes
  p = ggplot(data = fitted_observed_plot_seedl_transf, aes(x = obs_nonzero, y = fit)) +
    geom_abline(slope = 1, intercept = 0, color = "blue") +
    geom_point() +
    # lims(x=c(0,250), y=c(0,100)) +
    scale_x_continuous(trans = "log", breaks = breaks, labels = labels,
                       minor_breaks = minor_breaks) +
    scale_y_continuous(trans = "log", breaks = c(1, 10, 100, 1000, 10000),
                       minor_breaks = minor_breaks, limits = ylim) +
    labs(x = "Observed seedlings / ha", y = "Fitted seedlings / ha") +
    theme_bw(20)

  print(p)
}


load_fit_and_plot = function(dataset_name, disp_mod, err_mod, plot_size_ha, ylim, elevdiffs = 0) {
  # Run just the steps needed to make a fitted-observed plot (and fit metrics) for a specific fitted
  # model

  ## Load the dataset specified
  prepped_data_dir = file.path(data_dir, "prepped-for-stan", dataset_name)
  overstory_treesize_vector = read_lines(file.path(prepped_data_dir,
                                                   "overstory-treesize-vector.txt")) |> as.numeric()
  seedling_counts = read_lines(file.path(prepped_data_dir, "seedling-counts.txt")) |> as.numeric()
  dist_vector = read_lines(file.path(prepped_data_dir, "dist-vector.txt")) |> as.numeric()
  pos = read_lines(file.path(prepped_data_dir, "pos.txt")) |> as.numeric()
  n_overstory_trees = read_lines(file.path(prepped_data_dir,
                                           "n-overstory-trees.txt")) |> as.numeric()

  ## Load the fitted model and extract the parameter samples
  model_filename = file.path(data_dir, "stan-models",
                             paste0("stanmod_", dataset_name, "_", disp_mod, "_", err_mod, ".rds"))
  model_fit = readRDS(model_filename)
  samples = rstan::extract(model_fit)

  # Summarize across the samples, dropping uncertainty (faster predictions)
  samples_median = map(samples, median)
  # If want to retain uncertainty: samples_median= samples

  ## To run across all plots, need to get the tree-plot dists for each plot from the ragged array
  tree_dists_by_plot = list()
  for (i in seq_along(seedling_counts)) {
    tree_dists_by_plot[[i]] = dist_vector[pos[i]:(pos[i] + n_overstory_trees[i] - 1)]
  }

  ## Same for overstory tree size
  overstory_treesize_by_plot = list()
  for (i in seq_along(seedling_counts)) {
    overstory_treesize_by_plot[[i]] =
      overstory_treesize_vector[pos[i]:(pos[i] + n_overstory_trees[i] - 1)]
  }

  # Make predictions across all plots, but using just the point estimate of each parameter (no
  # uncertainty) plan(multisession(workers=8))
  plot_seedl_preds = future_map2(tree_dists_by_plot, overstory_treesize_by_plot,
                                 predict_seedl_plot, samples = samples_median, elevdiffs = 0) |>
    list_rbind()
  row.names(plot_seedl_preds) = NULL

  ## Combine with observed seedl
  fitted_observed_plot_seedl = bind_cols(obs = seedling_counts, plot_seedl_preds)

  plot_fitted_observed(fitted_observed_plot_seedl, plot_size_ha = plot_size_ha, ylim = ylim)
}
