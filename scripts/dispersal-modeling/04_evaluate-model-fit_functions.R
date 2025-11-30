# Script for developing functions to compute error metrics for fitted dispersal models. 
# They will all take as input a model object outputted from one fo the fit-dispersal-model functions. 
# 
# 

#### Notes on model fit meeting with David Russell: 
# From the meeting, some thoughts on error metrics
# Treat it as a classification problem and look at the confusion matrix for high vs. low density seedling plots
# Predicted mean vs observed mean (each averaged across all plot locations)
# Predicted variation vs. observed variation (across all plot locations)
# R^2
# Average error or average squared error (computed per predicted-observed pair and then averaged)
# Visual comparison of the density maps
# We also talked about comparing the density maps of two fit approaches to see if multiple modeling methods are ending up at the same conclusion, just different ways.

##### Prototype developed by claude with our specs 
##### NEED TO CHECK AND EDIT (10/22/25)



# Functions for evaluating dispersal model performance
# Includes both in-sample evaluation and cross-validation

library(dplyr)
library(ggplot2)
library(pROC)

#### Main Evaluation Functions ####

#' Evaluate model fit on a single dataset
#' 
#' Calculates continuous and categorical prediction metrics for a fitted model
#' 
#' @param model Fitted model object from fit_model_ml()
#' @param disp_data Dispersal data object (same structure as used for fitting)
#' @param threshold Threshold for classifying low vs high seedling density. 
#'                  If NULL (default), uses median split
#' @param make_plots Logical. Should diagnostic plots be generated?
#' @param plot_dir Directory to save plots (if make_plots = TRUE)
#' 
#' @return List containing:
#'   - continuous_metrics: data frame with MSE, RMSE, MAE, R2, bias
#'   - categorical_metrics: data frame with accuracy, sensitivity, specificity, AUC
#'   - confusion_matrix: confusion matrix
#'   - predictions: data frame with observed, predicted, and residuals
#'   - plots: list of ggplot objects (if make_plots = TRUE)
evaluate_model_fit <- function(model, 
                               disp_data = NULL,
                               threshold = NULL, 
                               make_plots = FALSE,
                               plot_dir = NULL) {
  
  # Extract observed and predicted values
  # If disp_data is provided, use it; otherwise use values from model object
  if (!is.null(disp_data)) {
    observed <- disp_data$seedling_counts
    # Recalculate predictions using model parameters
    predicted <- calculate_expected_counts(
      pars = model$estimates,
      disp_data = disp_data,
      settings = model$model_info$settings
    )
  } else {
    observed <- model$observed.values
    predicted <- model$fitted.values
  }
  
  n <- length(observed)
  
  # Calculate residuals
  residuals <- observed - predicted
  
  # --- Continuous Metrics ---
  mse <- mean(residuals^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(residuals))
  
  # R-squared (proportion of variance explained)
  ss_tot <- sum((observed - mean(observed))^2)
  ss_res <- sum(residuals^2)
  r2 <- 1 - (ss_res / ss_tot)
  
  # Bias (mean error)
  bias <- mean(residuals)
  
  # Assemble continuous metrics
  continuous_metrics <- data.frame(
    n_obs = n,
    MSE = mse,
    RMSE = rmse,
    MAE = mae,
    R2 = r2,
    bias = bias
  )
  
  # --- Categorical Metrics ---
  # Classify as low vs high density
  if (is.null(threshold)) {
    threshold <- median(observed)
  }
  
  obs_class <- classify_density(observed, threshold)
  pred_class <- classify_density(predicted, threshold)
  
  # Confusion matrix
  conf_mat <- table(Predicted = pred_class, Observed = obs_class)
  
  # Calculate metrics from confusion matrix
  if (all(c("low", "high") %in% rownames(conf_mat)) && 
      all(c("low", "high") %in% colnames(conf_mat))) {
    
    tn <- conf_mat["low", "low"]
    fp <- conf_mat["high", "low"]
    fn <- conf_mat["low", "high"]
    tp <- conf_mat["high", "high"]
    
    accuracy <- (tp + tn) / (tp + tn + fp + fn)
    sensitivity <- tp / (tp + fn)  # true positive rate
    specificity <- tn / (tn + fp)  # true negative rate
    precision <- tp / (tp + fp)
    F1 <- 2*tp / (2*tp + fp + fn) 
    
  } else {
    # Handle case where only one class is present
    accuracy <- sensitivity <- specificity <- precision <- NA
  }
  
  # Calculate AUC using predicted values as continuous predictor
  if (length(unique(obs_class)) > 1) {
    roc_obj <- roc(obs_class, predicted, quiet = TRUE)
    auc_value <- as.numeric(auc(roc_obj))
  } else {
    roc_obj <- NULL
    auc_value <- NA
  }
  
  # Assemble categorical metrics
  categorical_metrics <- data.frame(
    threshold = threshold,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    F1 = F1,
    AUC = auc_value
  )
  
  # --- Predictions data frame ---
  predictions_df <- data.frame(
    observed = observed,
    predicted = predicted,
    residual = residuals,
    obs_class = obs_class,
    pred_class = pred_class
  )
  
  # --- Generate plots if requested ---
  plot_list <- NULL
  if (make_plots) {
    plot_list <- generate_diagnostic_plots(predictions_df, roc_obj, threshold)
    
    # Save plots if directory specified
    if (!is.null(plot_dir)) {
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      
      ggsave(file.path(plot_dir, "obs_vs_pred.png"), 
             plot_list$obs_vs_pred, width = 6, height = 6)
      ggsave(file.path(plot_dir, "residuals_vs_fitted.png"), 
             plot_list$residuals_vs_fitted, width = 6, height = 6)
      ggsave(file.path(plot_dir, "residuals_qq.png"), 
             plot_list$residuals_qq, width = 6, height = 6)
      if (!is.null(plot_list$roc_curve)) {
        ggsave(file.path(plot_dir, "roc_curve.png"), 
               plot_list$roc_curve, width = 6, height = 6)
      }
    }
  }
  
  # --- Return results ---
  results <- list(
    continuous_metrics = continuous_metrics,
    categorical_metrics = categorical_metrics,
    confusion_matrix = conf_mat,
    predictions = predictions_df,
    threshold = threshold,
    plots = plot_list
  )
  
  class(results) <- "model_evaluation"
  return(results)
}


#' Cross-validation evaluation of model
#' 
#' Performs k-fold cross-validation and evaluates model performance
#' 
#' @param pars Named list of initial parameter values
#' @param settings Named list of model settings (lik_distrib, disp_kernel, fecundity_fn)
#' @param disp_data Dispersal data object
#' @param k_folds Number of folds for cross-validation (default = 5)
#' @param threshold Threshold for classifying low vs high density (default = NULL for median)
#' @param fixed_pars Vector of parameter names to hold constant
#' @param parscale Parameter scaling for optimization
#' @param make_plots Should diagnostic plots be made for each fold?
#' @param return_fold_details Should individual fold results be returned?
#' @param seed Random seed for reproducible fold assignment
#' 
#' @return List containing:
#'   - cv_metrics_summary: aggregated metrics across folds (mean ± SD)
#'   - fold_metrics: metrics for each individual fold (if return_fold_details = TRUE)
#'   - fold_models: fitted models for each fold (if return_fold_details = TRUE)
evaluate_model_cv <- function(pars,
                              settings,
                              disp_data,
                              k_folds = 5,
                              threshold = NULL,
                              fixed_pars = NULL,
                              parscale = NULL,
                              make_plots = FALSE,
                              return_fold_details = FALSE,
                              seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n_plots <- disp_data$n_plots
  
  # Assign plots to folds randomly
  fold_assignments <- sample(rep(1:k_folds, length.out = n_plots))
  
  # Initialize storage for results
  fold_continuous <- vector("list", k_folds)
  fold_categorical <- vector("list", k_folds)
  fold_models <- vector("list", k_folds)
  fold_predictions <- vector("list", k_folds)
  
  cat("\nPerforming", k_folds, "-fold cross-validation...\n")
  
  # Loop through folds
  for (fold in 1:k_folds) {
    cat("  Fitting fold", fold, "of", k_folds, "...\n")
    
    # Split data into training and test
    test_idx <- which(fold_assignments == fold)
    train_idx <- which(fold_assignments != fold)
    
    # Create training and test data objects
    train_data <- subset_disp_data(disp_data, train_idx)
    test_data <- subset_disp_data(disp_data, test_idx)
    
    # Fit model on training data
    fold_model <- tryCatch({
      fit_model_ml(
        pars = pars,
        fixed_pars = fixed_pars,
        parscale = parscale,
        disp_data = train_data,
        settings = settings
      )
    }, error = function(e) {
      warning(paste("Fold", fold, "failed to converge:", e$message))
      return(NULL)
    })
    
    if (is.null(fold_model)) {
      fold_continuous[[fold]] <- NA
      fold_categorical[[fold]] <- NA
      next
    }
    
    # Evaluate on held-out test data
    fold_eval <- evaluate_model_fit(
      model = fold_model,
      disp_data = test_data,
      threshold = threshold,
      make_plots = make_plots
    )
    
    # Store results
    fold_continuous[[fold]] <- fold_eval$continuous_metrics
    fold_categorical[[fold]] <- fold_eval$categorical_metrics
    fold_models[[fold]] <- fold_model
    fold_predictions[[fold]] <- fold_eval$predictions %>%
      mutate(fold = fold, plot_idx = test_idx)
  }
  
  # Combine results across folds
  continuous_combined <- bind_rows(fold_continuous, .id = "fold")
  categorical_combined <- bind_rows(fold_categorical, .id = "fold")
  
  # Calculate summary statistics (mean ± SD across folds)
  cv_continuous_summary <- continuous_combined %>%
    select(-fold, -n_obs) %>%
    summarise(across(everything(), list(
      mean = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE)
    ))) %>%
    tidyr::pivot_longer(everything(), 
                        names_to = c("metric", "stat"),
                        names_sep = "_",
                        values_to = "value") %>%
    tidyr::pivot_wider(names_from = stat, values_from = value)
  
  cv_categorical_summary <- categorical_combined %>%
    select(-fold, -threshold) %>%
    summarise(across(everything(), list(
      mean = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE)
    ))) %>%
    tidyr::pivot_longer(everything(), 
                        names_to = c("metric", "stat"),
                        names_sep = "_",
                        values_to = "value") %>%
    tidyr::pivot_wider(names_from = stat, values_from = value)
  
  # Prepare return object
  results <- list(
    cv_metrics_summary = list(
      continuous = cv_continuous_summary,
      categorical = cv_categorical_summary
    ),
    k_folds = k_folds,
    n_plots = n_plots
  )
  
  # Optionally include detailed fold-level results
  if (return_fold_details) {
    results$fold_metrics <- list(
      continuous = continuous_combined,
      categorical = categorical_combined
    )
    results$fold_models <- fold_models
    results$fold_predictions <- bind_rows(fold_predictions)
  }
  
  class(results) <- "model_cv_evaluation"
  return(results)
}


#### Helper Functions ####

#' Classify seedling density as low or high
#' 
#' @param counts Vector of seedling counts
#' @param threshold Threshold value (counts <= threshold are "low")
#' @return Factor with levels "low" and "high"
classify_density <- function(counts, threshold) {
  factor(ifelse(counts <= threshold, "low", "high"), 
         levels = c("low", "high"))
}


#' Subset dispersal data object to include only specified plots
#' 
#' @param disp_data Full dispersal data object
#' @param plot_indices Indices of plots to keep
#' @return Subsetted dispersal data object with same structure
subset_disp_data <- function(disp_data, plot_indices) {
  
  # Subset plot-level data
  subset_data <- list(
    overstory_trees = disp_data$overstory_trees,  # trees stay the same
    seedling_plots = disp_data$seedling_plots[plot_indices, ],
    seedling_plot_area = disp_data$seedling_plot_area,
    distance_matrix = disp_data$distance_matrix[plot_indices, , drop = FALSE],
    dem = disp_data$dem,
    tree_density = disp_data$tree_density,
    elev_diff_matrix = disp_data$elev_diff_matrix[plot_indices, , drop = FALSE],
    seedling_counts = disp_data$seedling_counts[plot_indices],
    n_plots = length(plot_indices)
  )
  
  # Rebuild ragged arrays for subset of plots
  r_cutoff <- subset_data$distance_matrix
  r_cutoff[r_cutoff == 0] <- NA  # assuming 0 means beyond cutoff
  
  # Number of non-NA trees per plot
  n_nonNA <- rowSums(!is.na(r_cutoff))
  
  # Create long vectors
  r_cutoff_vecfull <- as.vector(t(r_cutoff))
  r_cutoff_vec <- r_cutoff_vecfull[!is.na(r_cutoff_vecfull)]
  
  # Positions
  pos <- cumsum(c(1, n_nonNA[-length(n_nonNA)]))
  
  # Elevation differences
  elevdiff_cutoff_vecfull <- as.vector(t(subset_data$elev_diff_matrix))
  elevdiff_cutoff_vec <- elevdiff_cutoff_vecfull[!is.na(r_cutoff_vecfull)]
  
  # Tree density vector
  tree_density_vecfull <- rep(disp_data$overstory_trees$tree_density, 
                              length(plot_indices))
  tree_density_vec <- tree_density_vecfull[!is.na(r_cutoff_vecfull)]
  
  # Tree size vector
  tree_size_vecfull <- rep(disp_data$overstory_trees$size, 
                           length(plot_indices))
  tree_size_vec <- tree_size_vecfull[!is.na(r_cutoff_vecfull)]
  
  # Add ragged array components
  subset_data$pos <- pos
  subset_data$n_overstory_trees <- n_nonNA
  subset_data$dist_vector <- r_cutoff_vec
  subset_data$elev_diff_vector <- elevdiff_cutoff_vec
  subset_data$tree_density_vector <- tree_density_vec
  subset_data$tree_size_vector <- tree_size_vec
  
  return(subset_data)
}


#' Generate diagnostic plots for model evaluation
#' 
#' @param predictions_df Data frame with observed, predicted, residual columns
#' @param roc_obj ROC object from pROC package (or NULL)
#' @param threshold Density classification threshold
#' @return List of ggplot objects
generate_diagnostic_plots <- function(predictions_df, roc_obj, threshold) {
  
  # 1. Observed vs Predicted
  p1 <- ggplot(predictions_df, aes(x = observed, y = predicted)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(x = "Observed Seedling Count", 
         y = "Predicted Seedling Count",
         title = "Observed vs Predicted") +
    theme_bw() +
    theme(aspect.ratio = 1)
  
  # 2. Residuals vs Fitted
  p2 <- ggplot(predictions_df, aes(x = predicted, y = residual)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(se = TRUE, color = "blue") +
    labs(x = "Predicted Seedling Count", 
         y = "Residual",
         title = "Residuals vs Fitted Values") +
    theme_bw()
  
  # 3. Q-Q plot of residuals
  p3 <- ggplot(predictions_df, aes(sample = residual)) +
    stat_qq() +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(title = "Q-Q Plot of Residuals",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_bw()
  
  # 4. ROC curve (if available)
  p4 <- NULL
  if (!is.null(roc_obj)) {
    roc_df <- data.frame(
      sensitivity = roc_obj$sensitivities,
      specificity = roc_obj$specificities
    )
    
    auc_val <- round(as.numeric(auc(roc_obj)), 3)
    
    p4 <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
      geom_line(color = "blue", size = 1) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
      labs(title = paste0("ROC Curve (AUC = ", auc_val, ")"),
           x = "1 - Specificity (False Positive Rate)",
           y = "Sensitivity (True Positive Rate)") +
      theme_bw() +
      theme(aspect.ratio = 1)
  }
  
  return(list(
    obs_vs_pred = p1,
    residuals_vs_fitted = p2,
    residuals_qq = p3,
    roc_curve = p4
  ))
}


#### Print Methods ####

#' Print method for model_evaluation objects
print.model_evaluation <- function(x, ...) {
  cat("\n=== Model Evaluation Results ===\n")
  cat("\nContinuous Metrics:\n")
  print(x$continuous_metrics, row.names = FALSE)
  
  cat("\nCategorical Metrics (threshold =", x$threshold, "):\n")
  print(x$categorical_metrics, row.names = FALSE)
  
  cat("\nConfusion Matrix:\n")
  print(x$confusion_matrix)
  
  if (!is.null(x$plots)) {
    cat("\nDiagnostic plots available in $plots\n")
  }
  
  invisible(x)
}

#' Print method for model_cv_evaluation objects
print.model_cv_evaluation <- function(x, ...) {
  cat("\n=== Cross-Validation Results ===\n")
  cat(paste("\nK-folds:", x$k_folds))
  cat(paste("\nTotal plots:", x$n_plots, "\n"))
  
  cat("\nContinuous Metrics (Mean ± SD across folds):\n")
  print(x$cv_metrics_summary$continuous, row.names = FALSE)
  
  cat("\nCategorical Metrics (Mean ± SD across folds):\n")
  print(x$cv_metrics_summary$categorical, row.names = FALSE)
  
  invisible(x)
}

