# Implement ML fitting of inverse dispersal model. 
# Following example from Frank Schurr. 

# Start with an example simple model (exppow, with a one-parameter fecundity function)

# Helper function that orchestrates the calculation
calculate_expected_counts <- function(pars, disp_data, settings) {
  
  # Combine into expected counts per plot
  expected_counts <- numeric(disp_data$n_plots)
  
  for (i in seq_along(disp_data$n_plots)) {

    # Use indices to get the trees & their size & distances for plot i 
    start_idx <- disp_data$pos[i]
    end_idx <- disp_data$pos[i] + disp_data$n_overstory_trees[i] - 1
    plot_trees <- start_idx:end_idx
    heights_for_plot <- disp_data$overstory_tree_size[plot_trees]
    distances_for_plot <- disp_data$dist_vector[plot_trees]

    # Calculate fecundity for each tree in plot i
    tree_fecundity <- calculate_fecundity(height = heights_for_plot, pars = pars, 
                                          fecundity_type = settings$fecundity_fn)
    
    # Calculate dispersal probabilities for each tree in plot i 
    dispersal_probs <- calculate_dispersal(distance = distances_for_plot, pars = pars,
                                           kernel_type = settings$disp_kernel)
    
    # Calculate expected seedling count
    expected_counts[i]  <- tree_fecundity * dispersal_probs * seedling_plot_area
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
  k = pars$k
  a = pars$a
  switch(kernel_type,
         "exppow" = exp(-(distance/a)^k)*k / 
                          (2 * pi * a^2 * gamma(2/k)), 
         "2Dt" = ((k-1) / (pi * a^2)) * ((1 + distance^2) / a^2)^(-k),
                          # NEED TO CHECK THIS (Katul vs Marchand),
         "lognormal" = exp(-((log(distance)-mu)^2)/ 
                          (2*k^2))/(k*(2*pi)^(3/2)*distance^2), # CHECK
         "wald" = k^0.5*(2*pi)^(-1.5) * distance^(-2.5) * 
                          exp(-(k*(distance-a)^2)/(2*a^2*distance)) 
                          #(Katul et al. 2005, American Naturalist 166: 368-381)
  )
}

calculate_negloglik <- function(pars, disp_data, settings) {

  observed = disp_data$seedling_counts
  expected = calculate_expected_counts(pars, disp_data, settings)
    
  nll = switch(distrib_type,
         "pois" = sum(dpois(observed, expected, log = TRUE)),
         "negbin" = sum(dnbinom(observed, size = pars$theta, mu = expected, log = TRUE))
  )
  
}

### Function to fit the model using optim
fit_model_ml <- function(pars, disp_data, settings)
{
  #ML fitting of an inverse model with source and path effects
  #ARGUMENTS:
  #
  # pars: a named list containing the starting values for parameters
  #           of the dispersal kernel (k, a), fecundity function (b, zeta), 
  #           and the negative binomial likelihood dispersion parameter (theta)
  # 
  # settings: a named list containing the values for all the options 
  #    available for model fitting. These include: 
  #    - lik_distrib = the data distribution for the model (pois or negbin)
  #    - disp_kernel = which dispersal kernel to use (exppow, 2Dt, lognomal, wald)
  #    - fecundity_fn = which fecundity function to use (linear or exponential)
  #    - optimizer = which optimizer to tell optim to use (BFGS, Nelder-Mead, etc)
  #
  # disp_data: a data object returned from get_disp_data() 
  #              or simulate_disp_data(). Must include: 
  #               - n_overstory_trees, dist_vector, overstory_tree_size, pos
  #                  (in sparse/ragged form: pos points to the starting point of each 
  #                  plot's data in the vectors dist_vector and overstory_tree_size, 
  #                  and n_overstory_trees gives number of trees per plot. 
  #                - seedling_plot_area = area of plots (m)
  #                - seedling count = number of trees in each plot
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
  
  # Save the model call
  cl <- match.call()
  
  # Convert list of parameters to the vector form needed by optim
  #   Note this set depends on which model is fitted and number of parameters in it
  
  # Define parameter structure based on model settings
  get_par_structure <- function(settings) {
    par_names <- c("k", "a")  # dispersal kernel parameters
    
    # Add fecundity parameters based on function type
    if (settings$fecundity_fn == "linear") {
      par_names <- c(par_names, "b")
    } else if (settings$fecundity_fn == "exponential") {
      par_names <- c(par_names, "b", "zeta")
    }
    
    # Add dispersion parameter if using negative binomial
    if (settings$lik_distrib == "negbin") {
      par_names <- c(par_names, "theta")
    }
    
    return(par_names)
  }
  
  # Get the parameter structure for this model
  par_structure <- get_par_structure(settings)
  
  # Convert named list to vector, ensuring correct order
  par_vector <- sapply(par_structure, function(name) pars[[name]])
  
  # Wrapper function that converts vector back to named list
  calc_negloglik_wrapper <- function(par_vec, disp_data, settings) {
    par_list <- setNames(as.list(par_vec), par_structure)
    nll = calc_negloglik(pars = par_list, 
                   disp_data = disp_data,
                   settings = settings)
    return(nll)
  }
  
  # Fit the model
  fit <- optim(par_vector, calc_negloglik_wrapper, 
               method = settings$optimizer,
               control = list(trace = TRUE, maxit = 10000), disp_data = disp_data,
               settings = settings)
  
  # Convert results back to named format
  fit$par_named <- setNames(as.list(fit$par), par_structure)

  # Old version without wrapper 
  # CHECK WHAT FORMAT "pars" HAS TO BE IN -- MAY NEED TO TRANSLATE GOING IN AND OUT TO LIST FORMAT? 
#  fit <- optim(pars, method = settings$optimizer, control = list(trace = TRUE, maxit = 10000), calc_negloglik, n_overstory_trees = disp_data$n_overstory_trees, dist_vector = disp_data$dist_vector, overstory_tree_size = disp_data$overstory_tree_size, pos = disp_data$pos, seedling_counts = disp_data$seedling_counts, seedling_plot_area = disp_data$seedling_plot_area, lik_distrib = settings$lik_distrib, disp_kernel = settings$disp_kernel, fecundity_fn = settings$fecundity_fn) 
  
  # Convert parameter estimates back to named format
  estimates <- setNames(as.list(fit$par), par_structure)
  
  # Calculate fitted values (you'll need to implement this part)
  par_list <- setNames(as.list(par_vec), par_structure)
  fitted_values <- calculate_expected_counts(pars = par_list, 
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
    
    # Raw optim output (for advanced users/debugging)
    optim_output = fit,
    
    # Model metadata
    model_info = list(
      parameter_structure = par_structure,
      settings = settings,
      n_parameters = length(par_structure)
    )
  )
  
  # Optionally add a class for custom print/summary methods
  class(result) <- "fit_model_ml"

  return(results)
}

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

