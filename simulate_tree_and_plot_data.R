# Simulate data to test whether disp models can recover kernel parameter values. 

# More or less following Frank Schurr's approach 

library(ggplot2)
library(dplyr)
library(tidyr)
library(matrixStats)

# Define a square extent in which to simulate trees and seedling plots 

# Full domain
domain_size <- 800
center_buffer <- 200  # margin on each side to get central 400x400 area

# Tree area boundaries
tree_x_min <- center_buffer
tree_x_max <- domain_size - center_buffer
tree_y_min <- center_buffer
tree_y_max <- domain_size - center_buffer

library(spatstat.random)

# Define window for trees
tree_window <- owin(c(tree_x_min, tree_x_max), c(tree_y_min, tree_y_max))

# Use Thomas process for fractal-style clustering
# Adjust kappa, scale, and mu to tune clustering intensity
set.seed(123)
n_trees = 200
tree_pp <- rThomas(kappa = 0.02, scale = 20, mu = 10, win = tree_window)

# Take only n_trees points if more were generated
tree_samples <- sample(1:length(tree_pp$x), size = n_trees)
tree_points <- data.frame(
  x = tree_pp$x[tree_samples],
  y = tree_pp$y[tree_samples],
  height = runif(n_trees, 10, 30)  # random tree heights
)

# Generate plot locations
n_plots <- 100

set.seed(456)
seedling_plots <- data.frame(
  x = runif(n_plots, 0, domain_size),
  y = runif(n_plots, 0, domain_size)
)

# Plot tree_points and seedling_plots together 

ggplot() +
  geom_point(data = tree_points, aes(x = x, y = y), color = "green", size = 2) +
  geom_point(data = seedling_plots, aes(x = x, y = y), color = "blue", size = 3) +
  coord_fixed() +
  xlim(0, domain_size) +
  ylim(0, domain_size) +
  theme_minimal() +
  labs(title = "Tree and Seedling Plot Locations",
       x = "X Coordinate",
       y = "Y Coordinate")

### Test simulating dispersal 


# --- Parameters for the dispersal kernel ---

simulate_seed_rain <- function(tree_df, plot_df, 
                               a, k, b, 
                               plot_area_m2 = 1, 
                               seed = 1234) {
  #' Simulate seed rain using exponential power kernel
  #' 
  #' @param tree_df Data frame with columns x, y, height
  #' @param plot_df Data frame with columns x, y
  #' @param a Kernel scale parameter
  #' @param k Kernel shape parameter
  #' @param b Fecundity per m of height
  #' @param plot_area_m2 Area of each seedling plot (m²)
  #' @param seed Random seed for reproducibility
  #' 
  #' @return plot_df with expected and observed seed counts
  
  set.seed(seed)
  
  # Kernel function
  exp_power_kernel <- function(r, a, k) {
    coeff <- k / (2 * pi * a^2 * gamma(2 / k))
    return(coeff * exp(- (r / a)^k))
  }
  
  # Convert to matrices
  tree_mat <- as.matrix(tree_df[, c("x", "y")])
  plot_mat <- as.matrix(plot_df[, c("x", "y")])
  n_trees <- nrow(tree_mat)
  n_plots <- nrow(plot_mat)
  
  # Distance matrix (trees × plots)
  dist_matrix <- as.matrix(dist(rbind(tree_mat, plot_mat)))
  dist_matrix <- dist_matrix[1:n_trees, (n_trees + 1):(n_trees + n_plots)]
  
  # Apply kernel
  kernel_matrix <- exp_power_kernel(dist_matrix, a, k)
  
  # Fecundity vector
  mean_fecundity <- b * tree_df$height
  fecundity <- rnorm(n = length(mean_fecundity), mean = mean_fecundity, sd = mean(mean_fecundity)/10)
  
  # Expected seeds per plot (scaled by plot area)
  expected <- colSums(kernel_matrix * fecundity) * plot_area_m2
  
  # Simulate observed seeds using Poisson
  observed <- rpois(n_plots, expected)
  
  # Return augmented plot_df
  out_df <- plot_df
  out_df$expected_seeds <- expected
  out_df$observed_seeds <- observed
  return(out_df)
}

# Try it out 
# Assuming you already have:
# - tree_points: data frame of 1000 trees with x, y, height
# - seedling_plots: data frame of 200 plots with x, y

result_df <- simulate_seed_rain(
  tree_df = tree_points,
  plot_df = seedling_plots,
  a = 30,     # scale
  k = 0.5,    # shape
  b = 10,     # fecundity coefficient
  plot_area_m2 = 201  # size of each plot in m²
)

head(result_df)


# Plot tree_points and seedling_plots together, coloring plots by expected seeds 

plot_data = tibble(x = seedling_plots$x, y = seedling_plots$y, seedling_counts = result_df$expected_seeds)

ggplot() +
  #geom_point(data = tree_points, aes(x = x, y = y, color = "lightgreen")) +
  geom_point(data = plot_data, aes(x = x, y = y, color = seedling_counts)) + 
  scale_color_continuous(type = "viridis") + 
  coord_fixed() +
  xlim(0, domain_size) +
  ylim(0, domain_size) +
  theme_minimal() +
  labs(title = "Tree and Seedling Plot Locations",
       x = "X Coordinate",
       y = "Y Coordinate") 

#### Export simulated data for model fitting 

tree_coords <- as.matrix(tree_points[, c("x", "y")])
plot_coords <- as.matrix(seedling_plots[, c("x", "y")])

# Use crossdist from spatstat.geom (fast and memory-efficient)
library(spatstat.geom)

dist_matrix <- crossdist(tree_coords[,1], tree_coords[,2], 
                         plot_coords[,1], plot_coords[,2])  # trees × plots

# Create a list where each element is a vector of distances
# from all trees to the i-th plot
dist_ragged <- lapply(1:ncol(dist_matrix), function(j) dist_matrix[, j])

# Flatten the list into a single vector
dist_vector <- unlist(dist_ragged)

# Number of trees per plot
n_overstory_trees <- sapply(dist_ragged, length)

# Position index (start of each plot's tree distances in vector d)
pos <- cumsum(c(1, head(n_overstory_trees, -1)))

# Tree heights vector 
# Suppose: tree_indices_by_plot is a list of integer vectors (tree indices per plot)
tree_indices_by_plot <- lapply(dist_ragged, function(x) 1:length(x))  # update this if needed

# Use indices to get matching heights
overstory_tree_size_ragged <- lapply(tree_indices_by_plot, function(idx) tree_points$height[idx])
overstory_tree_size <- unlist(overstory_tree_size_ragged)

# Generate seedling counts using Poisson noise from expected values 
seedling_counts = result_df$observed_seeds


