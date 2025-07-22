# Implement ML fitting of inverse dispersal model. 
# Following example from Frank Schurr. 

# Start with an example simple model (exppow, with a one-parameter fecundity function)

# Function that applies dispersal and fecundity kernels to get the expected counts 
#     or fitted values for a given set of parameter values. 
calculate_expected_counts <- function(pars, disp_data, settings) {
  
  # Combine into expected counts per plot
  expected_counts <- numeric(disp_data$n_plots)
  
  for (i in seq_along(disp_data$seedling_counts)) {

    # Use indices to get the trees & their size & distances for plot i 
    start_idx <- disp_data$pos[i]
    end_idx <- disp_data$pos[i] + disp_data$n_overstory_trees[i] - 1
    plot_trees <- start_idx:end_idx
    heights_for_plot <- disp_data$tree_size_vector[plot_trees]
    distances_for_plot <- disp_data$dist_vector[plot_trees]

    # Calculate fecundity for each tree in plot i
    tree_fecundity <- calculate_fecundity(height = heights_for_plot, pars = pars, 
                                          fecundity_type = settings$fecundity_fn)
    
    # Calculate dispersal probabilities for each tree in plot i 
    dispersal_probs <- calculate_dispersal(distance = distances_for_plot, pars = pars,
                                           kernel_type = settings$disp_kernel)
    
    # Calculate expected seedling count
    expected_counts[i]  <- sum(tree_fecundity * dispersal_probs * seedling_plot_area)
  }
  
  return(expected_counts)
}

# Individual component functions
calculate_fecundity <- function(height, pars, fecundity_type) {
  switch(fecundity_type,
         "linear" = pars$b * height,
         "exponential" = pars$b * height ^ pars$zeta
  )
}

calculate_dispersal <- function(distance, pars, kernel_type) {
  distance <- pmax(distance, 1e-6) # avoid zero distance values
  k = pars$k # shape or dispersion parameter
  a = pars$a # scale or mean parameter 
  disp_probs = switch(kernel_type,
         "exppow" = exp(-(distance/a)^k)*k / 
                          (2 * pi * a^2 * gamma(2/k)), 
         "2Dt" = ((k-1) / (pi * a^2)) * ((1 + distance^2) / a^2)^(-k),
         "lognormal" = dlnorm(distance, meanlog = a, sdlog = k) / 
                          (2 * pi * distance), 
                          # 1D lognormal with correction to convert to 2D
         "wald" = k^0.5*(2*pi)^(-1.5) * distance^(-2.5) * 
                          exp(-(k*(distance-a)^2)/(2*a^2*distance)) 
                          # See Katul et al. 2005, American Naturalist 166: 368-381
  )
  return(disp_probs)
}

# Helper function to set up parameter structure 
get_par_structure <- function(settings) {
  par_names <- character(0)
  
  # Dispersal kernel parameters 
  # (assuming disperal parameters are the same across all kernels for now)
  par_names <- c(par_names, "k", "a")
  
  # Fecundity function parameters  
  if (settings$fecundity_fn == "linear") {
    par_names <- c(par_names, "b")
  } else if (settings$fecundity_fn == "exponential") {
    par_names <- c(par_names, "b", "zeta")
  }
  
  # Likelihood distribution parameters
  if (settings$lik_distrib == "negbin") {
    par_names <- c(par_names, "theta")
  }
  
  return(par_names)
}

calculate_negloglik <- function(pars_vector, disp_data, settings) {
  
  # First convert parameter vector from optim to a list to suppy to other functions
  
  # Get parameter structure for this model configuration
  par_structure <- get_par_structure(settings)
  
  # Convert vector to named list
  pars <- setNames(as.list(pars_vector), par_structure)
  
  
  # Assemble observed and expected counts for likelihood
  observed = disp_data$seedling_counts
  expected = calculate_expected_counts(pars, disp_data, settings)
  
  # Calculate NLL
  nll = switch(settings$lik_distrib,
         "pois" = -sum(dpois(observed, expected, log = TRUE)),
         "negbin" = -sum(dnbinom(observed, size = pars$theta, mu = expected, log = TRUE)))
  if (is.nan(nll)) nll = 1e100
  return(nll)
}

### Function to fit the model using optim
fit_model_ml <- function(pars, fixed_pars = NULL, parscale = NULL, disp_data, settings)
{
  #ML fitting of an inverse model with source and path effects
  #ARGUMENTS:
  #
  # pars: a named list containing the starting values for parameters
  #           of the dispersal kernel (k, a), fecundity function (b, zeta), 
  #           and the negative binomial likelihood dispersion parameter (theta)
  # 
  # fixed_pars: vector of parameter names to hold constant at their starting value 
  #
  # settings: a named list containing the values for all the options 
  #    available for model fitting. These include: 
  #    - lik_distrib = the data distribution for the model (pois or negbin)
  #    - disp_kernel = which dispersal kernel to use (exppow, 2Dt, lognomal, wald)
  #    - fecundity_fn = which fecundity function to use (linear or exponential)
  #    - optimizer = which optimizer to tell optim to use (BFGS, Nelder-Mead, etc)
  #                   NOTE: Currently this only uses SANN to be able to constrain 
  #                                some params, so this setting is ignored.
  #
  # disp_data: a data object returned from get_disp_data() 
  #              or simulate_disp_data(). Must include: 
  #               - n_overstory_trees, dist_vector, overstory_tree_size, pos
  #                  (in sparse/ragged form: pos points to the starting point of each 
  #                  plot's data in the vectors dist_vector and overstory_tree_size, 
  #                  and n_overstory_trees gives number of trees per plot. 
  #                - seedling_plot_area = area of plots (m)
  #                - seedling count = number of trees in each plot
  #                - n_plots = number of field plots with seedling counts. 
  #
  #VALUE: list with components:
  #
  #estimates: a named list of maximum likelihood parameter estimates,
  #           for each component parameters are in the same order as in startpars
  #negloglik: the negative log likelihood of the fitted inverse model
  #fitted.values: the fitted values for each seed trap
  #call:      the matched call
  #counts,convergence and message: the respective output of the optim function
  #           used to fit the inverse models
  
  require(optimx)
  
  # Save the model call
  cl <- match.call()
  
  # Convert list of parameters to the vector form needed by optim
  #   Note this set depends on which model is fitted and number of parameters in it
  
  # Check that parameter structure matches model settings
  par_names = names(pars)
  # Universal parameters
  try(if(is_null(pars$a)) stop("Parameter a missing."))
  try(if(is_null(pars$b)) stop("Parameter b missing."))
  try(if(is_null(pars$k)) stop("Parameter k missing."))
  
  # Additional parameters 
  if (settings$fecundity_fn == "exponential") try(if(is_null(pars$zeta)) stop("Parameter zeta missing."))
  if (settings$lik_distrib == "negbin") try(if(is_null(pars$theta)) stop("Parameter theta missing."))
  
  # Get parameter structure and convert named parameter list to vector
  par_structure <- get_par_structure(settings)
  pars_vector <- sapply(par_structure, function(name) pars[[name]])
  
  # start by setting parameter-specific bounds
  lower <- upper <- vector(mode = "numeric", length = length(pars_vector))
  idx <- match("k", par_structure)
  if (!is.na(idx)) { 
    lower[idx] <- 1e-10 # close to 0
    upper[idx] <- 3.0
  }
  idx <- match("a", par_structure)
  if (!is.na(idx)) { 
    lower[idx] <- 1e-10 # close to 0
    upper[idx] <- 100
  }
  idx <- match("b", par_structure)
  if (!is.na(idx)) { 
    lower[idx] <- 1e-10 # close to 0
    upper[idx] <- 1000
  }
  idx <- match("zeta", par_structure)
  if (!is.na(idx)) { 
    lower[idx] <- -Inf # close to 0
    upper[idx] <- Inf
  }
  idx <- match("theta", par_structure)
  if (!is.na(idx)) { 
    lower[idx] <- 1e-10 # close to 0
    upper[idx] <- Inf
  }
  
  # Set bounds for fixed parameters 
  if (!is.null(fixed_pars)) {
    idx <- match(fixed_pars, par_structure)
    lower[idx] <- upper[idx] <- pars_vector[idx]
  }

  # Optimize the parameters using sannbox with constraints
  fit <- sannbox(par = pars_vector,
                fn = calculate_negloglik,
                control = list(trace = 1, maxit = 5000, 
                               lower = lower, upper = upper, 
                               parscale =parscale),
                disp_data = disp_data,
                settings = settings)
  
  # Convert parameter estimates back to named format
  estimates <- as.list(fit$par)
  
  # Calculate fitted values
  fitted_values <- calculate_expected_counts(pars = estimates, 
        disp_data = disp_data,
        settings = settings)
  
  # Structure the return list
  result <- list(
    # User-friendly named results
    estimates = estimates,
    negloglik = fit$value,
    fitted.values = fitted_values,
    call = cl,
    
    # Optimization details
    convergence = fit$convergence,
    message = fit$message,
    counts = fit$counts,
    
    # Raw optim output 
    #optim_output = fit,
    
    # Model metadata
    model_info = list(
      parameter_structure = par_structure,
      settings = settings,
      n_parameters = length(par_structure)
    )
  )
  
  # Optionally add a class for custom print/summary methods
  class(result) <- "fit_model_ml"

  return(result)
}


# Older function that uses Derek's pre-calculated data values 
get_disp_data <- function(dataset_name, data_dir) # corresponding data files in datadir/prepped-for-stan/{dataset_name}
  { 
  
  # -- Load prepped dataset (corresponding data files in datadir/prepped-for-stan/{dataset_name})
  prepped_data_dir = file.path(data_dir, "prepped-for-stan", dataset_name)
  
  seedling_plot_area = read_file(file.path(prepped_data_dir, "plot-area.txt")) |> 
      as.numeric()
  dist_vector = read_lines(file.path(prepped_data_dir, "dist-vector.txt")) |>
      as.numeric() |>
      as.vector()
  overstory_treesize_vector = read_lines(file.path(prepped_data_dir,
      "overstory-treesize-vector.txt")) |>
      as.numeric() |>
      as.vector()
  seedling_counts = read_lines(file.path(prepped_data_dir, "seedling-counts.txt")) |>
      as.numeric() |>
      as.vector()
  n_overstory_trees = read_lines(file.path(prepped_data_dir, "n-overstory-trees.txt")) |>
      as.numeric() |>
      as.vector()
  pos = read_lines(file.path(prepped_data_dir, "pos.txt")) |>
      as.numeric() |>
      as.vector()
  
  ## Compile data and priors into list for model fitting 
  
  data_list <- lst(
      seedling_plot_area,
      n_overstory_trees,
      n_seedling_plots = length(seedling_counts),
      overstory_tree_size = overstory_treesize_vector,
      seedling_counts,
      dist_vector,
      obs = length(dist_vector),
      pos
  )

  # Check for missing data
  if (any(is.na(unlist(data_list)))) stop("Missing values in data.")
  
  return(data_list)
}


# Function to convert the model options grid settings to input for model fitting 
fit_model_wrapper_fn <- function(model_options_grid) {
  
  settings_to_use = list(lik_distrib = NULL, disp_kernel = NULL, 
                         fecundity_fn = NULL, optimizer = NULL) 
  n_models <- nrow(model_options_grid)
  model_list <- list(n_models) 
  for (i in 1:n_models) {
    print(paste("Running model", i, "of", n_models))
    # supply settings values 
    settings_to_use$lik_distrib = model_options_grid$lik_distrib_vals[i]
    settings_to_use$fecundity_fn = model_options_grid$fecundity_fn_vals[i]
    settings_to_use$disp_kernel = model_options_grid$disp_kernel_vals[i]
    
    # set initial parameters depending on model options 
    pars_inits <- list("k" = 1, "a" = 10, "b" = 10)
    parscale = c(1, 10, 10)
    if (settings_to_use$fecundity_fn == "exponential") {
      pars_inits$zeta = 1
      parscale = c(parscale, 1)
    }
    if (settings_to_use$lik_distrib == "negbin") {
      pars_inits$theta = 1
      parscale = c(parscale, 1)
    }
    model_list[[i]] <- fit_model_ml(pars = pars_inits, 
                                    fixed_pars = NULL, 
                                    parscale = parscale,
                                    disp_data = disp_data,
                                    settings = settings_to_use)
  }
  return(model_list) 
}


