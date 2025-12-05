# Visualize model results 

# Fit models 
#m2 = fit_model_ml(pars = startpars1, fixed_pars = NULL, parscale = c(1, 10, 10, 1), disp_data = disp_data, settings = settings_to_use)

# Choose model to plot
m = m1 

#m = model_fits[[11]] # for ABCO at Delta, negbin lognormal with exp fecundity

# Plot fitted vs observed 
obspred_data <- data.frame(fitted = m$fitted.values, observed = disp_data$seedling_counts)

ggplot(obspred_data, aes(x = fitted, y = observed)) + 
  geom_point() + theme_minimal() + 
  stat_smooth(method = "lm")

# Check of residuals 
obspred_data$resids = obspred_data$observed-obspred_data$fitted
qqnorm(obspred_data$resids)
plot(resids~fitted, obspred_data)
hist(obspred_data$resids)

# plot dispersal kernel based on fitted parameters
kernel_plot_data <- data.frame(Distance = 1:800, Probability = calculate_dispersal(distance = 1:800, pars = list(k = m$estimates$k, a = m$estimates$a), kernel_type = "exppow"))
ggplot(kernel_plot_data, aes(x = Distance, y = Probability)) + 
  geom_line(color = "cyan4", size = 2) + 
  theme_minimal() + 
  theme(axis.title = element_text(size = 18, face = "bold")) + 
  theme(axis.text.x = element_text(size = 12)) + 
  theme(axis.text.y = element_text(size = 12)) + 
  labs(y = "Relative seed density")
  

# plot fecundity function 
fecundity_plot_data <- data.frame(tree_height = seq(10, 30, by = 0.1)) |> 
  mutate(fecundity = (tree_height * m$estimates$b))#^m$estimates$zeta)
ggplot(fecundity_plot_data, aes(x = tree_height, y = fecundity)) + 
  geom_line() + theme_minimal()


# visualize likelihood surface 
kvals = seq(0.1, 0.6, by = 0.05)
avals = seq(5, 70, by = 2)
bvals = 20
thetavals = 50
parameter_test_set <- expand.grid(kvals, avals, bvals, thetavals) 
names(parameter_test_set) = c("k", "a", "b", "theta")
head(parameter_test_set)
fn_to_apply_negloglik <- function(param_test_vals, disp_data = disp_data, 
                                  settings = settings_to_use) {
  pars = c(param_test_vals[1], param_test_vals[2], 
           param_test_vals[3], param_test_vals[4])
  nll = calculate_negloglik(pars = pars, disp_data = disp_data, 
                            settings = settings)
  return(nll)
}

negloglikvals <- apply(parameter_test_set, 1, fn_to_apply_negloglik, 
                       disp_data = disp_data, settings = settings)
lik_surface_data <- cbind(parameter_test_set, negloglikvals)
head(lik_surface_data)
hist(negloglikvals)

# Where is the minimum? 
lik_surface_data[which.min(lik_surface_data$negloglikvals),]

# plot a 2D likelihood surface using negloglikvals data
ggplot(lik_surface_data, aes(x = a, y = k, z = negloglikvals)) + 
  geom_tile(aes(fill = log(negloglikvals))) + 
  #geom_contour() + 
  scale_fill_viridis_c() + 
  theme_minimal() + 
  labs(title = "Likelihood surface", x = "a", y = "k") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(title = "Negative log likelihood")) + 
  theme(axis.title = element_text(size = 16)) + 
  theme(axis.text.x = element_text(size = 12)) + 
  theme(axis.text.y = element_text(size = 12)) + 
  labs(y = "k (amount of long-distance dispersal)", 
       x = "a (scale of local dispersal)") + 
  ggtitle("Likelihood surface") + 
  theme(plot.title = element_text(size = 18))

ggplot(lik_surface_data, aes(x = b, y = k)) + 
  geom_tile(aes(fill = negloglikvals)) + 
  scale_fill_viridis_c() + 
  theme_minimal() + 
  labs(title = "Likelihood surface", x = "b", y = "k") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(title = "Negative log likelihood"))


# NOTE: Convergence is sensitive to starting values -- can converge to "reasonable" or extreme values for most data sets 
# NOTE: Using 500m distance seems slighly more stable (more informative)


## Next steps: 
# Visualize likelihood surface. DONE - shows a ridge as expected based on a / k correlation. 
# The model runs away to very unrealistic and extreme parameter combinations. TRY keeping b fixed. DONE - didn't help 
# Check that model can recover params from simulation. DONE -- yes it can! 

# Since simulations work to recover parameters, see if any of the data sets for the other 3 fires (other than Delta) can converge to reasonable parameter values. 

# Implement negbin likelihood - done 

# Systematically test which data sets x likelihood types converge consistently to something biologically plausible, or at least do so from reasonable starting values. 
# 
# Think about how to reduce number of parameters in models. Can we limit a to a set of biologically reasonable values and fit the other parameters around that? Or switch to single-parameter dispersal models? 


# 1) Implement model options grid and search values for k and b. 
# 2) Check this for simulated data with no noise and many plots. 
# 3) Ditto for simulated data with no noise 
# 4) Fix k to fit 1-parameter version of exponential kernel. 
# 5) Compare loo_cv of the 1- and 2- parameter models. 
# 6) Compare binary classification result of the more and less complex models

# NOTE there seems to be something wrong with the fixed parameters code. Fixing k always leads a to be at 100 (upper bound). May need to simply implement the exp kernel instead. 


# Plot overstory trees and plots 


library(ggspatial)

# Load the data 
### Load the overstory tree and seedling data for the specified site
overstory_trees = st_read(file.path(data_dir, overstory_tree_filepath)) |> 
  st_transform(target_crs)
seedling_plots = st_read(file.path(data_dir, seedling_plot_filepath)) |> 
  st_transform(target_crs)

# Convert overstory polys to points 
# (may want to make this optional in case tree locations are already points)
overstory_trees_centroids = st_centroid(overstory_trees)

# Filter overstory to exclude SNAG and trees smaller than 10m tall 
overstory_trees_centroids = overstory_trees_centroids |>
  filter(pred_class_ID != c("SNAG"), Z >= min_tree_height)


overstory_trees_PIPJ = overstory_trees_centroids |> 
  filter(pred_class_ID == "PIPJ") 

overstory_trees_minus_unknown = overstory_trees_centroids |> 
  filter(pred_class_ID != "unknown") 

# Choose the species to plot for the observed counts in plots 
seedling_plots  = seedling_plots |> 
  mutate(observed_count = count_PIPJ)


ggplot() + 
  geom_sf(data = overstory_trees_minus_unknown, color = "lightgray", size = 0.8) +
  geom_sf(data = overstory_trees_PIPJ, color = "darkgreen", size = 1) + 
  theme_minimal() + 
  geom_sf(data = seedling_plots, size = 2, aes(color = log10(observed_count))) + 
  scale_color_viridis_c(option = "A") + 
  annotation_scale(location = "tr", width_hint = 0.2, style = "ticks")



#### Predict seed/ling density across a grid of points

# Create prediction raster
predict_dispersal_raster <- function(model, disp_data, res = 10, 
                                     tree_distance_cutoff = 300) {
  
  require(terra)
  require(sf)
  
  # Create empty raster covering the site extent
  site_bbox <- st_bbox(disp_data$overstory_trees)
  # Expand slightly to show edges
  site_bbox[c("xmin", "ymin")] <- site_bbox[c("xmin", "ymin")] - 50
  site_bbox[c("xmax", "ymax")] <- site_bbox[c("xmax", "ymax")] + 50
  
  pred_rast <- rast(xmin = site_bbox["xmin"], xmax = site_bbox["xmax"],
                    ymin = site_bbox["ymin"], ymax = site_bbox["ymax"],
                    resolution = res,
                    crs = crs(disp_data$overstory_trees))
  
  # Get cell numbers and coordinates directly
  n_cells <- ncell(pred_rast)
  pred_points <- data.frame(
    cell = 1:n_cells,
    xyFromCell(pred_rast, 1:n_cells)
  )
  
  # Get tree locations and sizes
  tree_coords <- st_coordinates(disp_data$overstory_trees)
  tree_sizes <- disp_data$overstory_trees$size  # or height column
  
  # Calculate predicted seedling density for each raster cell
  predictions <- numeric(nrow(pred_points))
  
  for (i in 1:nrow(pred_points)) {
    # Calculate distances from this cell to all trees
    dx <- pred_points$x[i] - tree_coords[, "X"]
    dy <- pred_points$y[i] - tree_coords[, "Y"]
    distances <- sqrt(dx^2 + dy^2)
    
    # Only use trees within cutoff distance
    nearby <- distances <= tree_distance_cutoff & distances > 0
    
    if (sum(nearby) > 0) {
      # Calculate fecundity for nearby trees
      fecundity <- calculate_fecundity(
        height = tree_sizes[nearby],
        pars = model$estimates,
        fecundity_type = model$model_info$settings$fecundity_fn
      )
      
      # Calculate dispersal probabilities
      disp_probs <- calculate_dispersal(
        distance = distances[nearby],
        pars = model$estimates,
        kernel_type = model$model_info$settings$disp_kernel
      )
      
      # Sum contributions (scaled by plot area for seeds per m²)
      predictions[i] <- sum(fecundity * disp_probs * disp_data$seedling_plot_area)
    }
    
    if (i %% 1000 == 0) cat("Processing cell", i, "of", nrow(pred_points), "\n")
  }
  
  # Put predictions back into raster
  pred_rast[pred_points$cell] <- predictions
  
  return(pred_rast)
}

# Generate prediction raster
pred_raster <- predict_dispersal_raster(m1, disp_data, res = 10)

# Get tree spatial object with only the focal species 



# Plot continuous predictions
library(tidyterra)
library(ggplot2)

ggplot() +
  geom_spatraster(data = pred_raster) +
  scale_fill_viridis_c(name = "Predicted\nSeedlings", trans = "log1p") +
  geom_sf(data = disp_data$overstory_trees, size = 0.5, alpha = 0.5) +
  labs(title = "Predicted Seedling Density Across Site") + 
  theme_minimal() 



#geom_sf(data = disp_data$seedling_plots, aes(color = log10(observed_count)), size = 2) +
#  scale_color_viridis_c(option = "A") + 
#  theme_minimal() +

####


## Plot a discretized version 
# Define thresholds (e.g., using quantiles)
quants <- quantile(values(pred_raster), probs = c(0.33, 0.45), na.rm = TRUE)

pred_categorical <- pred_raster
values(pred_categorical) <- cut(values(pred_raster),
                                breaks = c(-Inf, 1, 5, Inf),
                                labels = c("Weak (<50)", "Moderate (<250)", "Strong (>250)"))

# Plot
ggplot() +
  geom_spatraster(data = pred_categorical) +
  scale_fill_manual(values = c("Weak (<50)" = "#fee5d9", 
                               "Moderate (<250)" = "#fcae91", 
                               "Strong (>250)" = "#fb6a4a"),
                    name = "Predicted\nRegeneration\nPotential\n (seedlings/ha)",
                    na.value = "white") +
  geom_sf(data = disp_data$overstory_trees, size = 0.5, alpha = 0.3) +
  geom_sf(data = disp_data$seedling_plots, aes(color = observed_count+0.5), size = 2) +
  scale_color_viridis_c(option = "A", name = "Seedling count", trans = "log10") + 
  theme_minimal() 
  #labs(title = "Predicted Regeneration Potential")



##### Try plotting spatial classification success 

# Show where the model got it right vs wrong
plot_spatial_classification <- function(model_results, disp_data) {
  
  # Create data frame with plot locations and classification results
  plot_df <- data.frame(
    x = disp_data$seedling_plots$x,
    y = disp_data$seedling_plots$y,
    observed = eval_results$predictions$obs_class,
    predicted = eval_results$predictions$pred_class,
    correct = eval_results$predictions$obs_class == 
              eval_results$predictions$pred_class
  )
  
  # Create detailed classification categories
  plot_df$classification <- with(plot_df, 
    case_when(
      correct & observed == "high" ~ "True Positive (high)",
      correct & observed == "low" ~ "True Negative (low)",
      !correct & predicted == "high" ~ "False Positive",
      !correct & predicted == "low" ~ "False Negative"
    ))
  
  # Plot
  ggplot(plot_df, aes(x = x, y = y, color = classification)) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(
      values = c("True Positive (high)" = "darkgreen",
                 "True Negative (low)" = "lightblue",
                 "False Positive" = "orange",
                 "False Negative" = "red"),
      name = "Classification"
    ) +
    theme_minimal() +
    labs(title = "Spatial Pattern of Classification Accuracy",
         subtitle = paste0("Overall Accuracy: ", 
                          round(eval_results$categorical_metrics$accuracy * 100, 1),
                          "%")) +
    coord_equal()
}

plot_spatial_classification(model_results, disp_data)




#### Confusion matrix as heatmap
library(ggplot2)
library(dplyr)

# Function to visualize confusion matrix
plot_confusion_matrix <- function(eval_results, normalize = FALSE) {
  
  library(ggplot2)
  library(dplyr)
  
  # Extract confusion matrix
  cm <- eval_results$confusion_matrix
  
  # Convert to data frame more explicitly
  # The confusion matrix from table() has Predicted as rows, Observed as columns
  cm_df <- data.frame(
    Predicted = rep(rownames(cm), times = ncol(cm)),
    Observed = rep(colnames(cm), each = nrow(cm)),
    Count = as.vector(cm)  # Convert matrix to vector (goes column-wise)
  )
  
  if (normalize) {
    # Normalize by column (observed totals) to show percentages
    cm_df <- cm_df %>%
      group_by(Observed) %>%
      mutate(Count = Count / sum(Count) * 100) %>%
      ungroup()
    fill_label <- "Percentage"
    text_format <- function(x) paste0(round(x, 1), "%")
  } else {
    fill_label <- "Count"
    text_format <- function(x) as.character(round(x, 0))
  }
  
  # Create heatmap
  p <- ggplot(cm_df, aes(x = Observed, y = Predicted, fill = Count)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = text_format(Count)), 
              size = 8, fontface = "bold") +
    scale_fill_gradient(low = "white", high = "steelblue",
                        name = fill_label) +
    theme_minimal(base_size = 14) +
    theme(panel.grid = element_blank(),
          axis.title = element_text(face = "bold")) +
    labs(title = "Confusion Matrix",
         subtitle = paste0("Threshold = ", 
                           round(eval_results$threshold*50, 1), 
                           " seedlings/ha"))
  
  return(p)
}
# Use it:
plot_confusion_matrix(model_results, normalize = FALSE)
plot_confusion_matrix(model_results, normalize = TRUE)  # as percentages
