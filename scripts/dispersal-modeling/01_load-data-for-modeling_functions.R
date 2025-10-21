# Functions that load data on tree locations and seedling plots, and package into standard data object for modeling. 
# -- they generate a DEM and elevation data 
# -- they generate a tree density raster
# -- they package data up into full matrices and into sparse long vector format. 

# Main functions
#   - get_dispdata() -- formats data from existing drone-derived tree maps
#   - simulate_dispdata() -- simulates data from tree and seedling plots

# Auxiliary functions 
#   - get_dem_data()
#   - get_tree_density()
#   - simulate_seed_rain()

### Loading existing tree and plot data 

# Function get_dispdata() outputs a large data object with tree and plot information, plus distances, tree density, and elevation. It contains the data in both full matrix formats and ragged array formats. Rather than writing to file, it keeps the result in memory for immediately passing to the model fitting function. This allows testing how the data prep parameters affect inferences. 

get_dispdata = function(data_dir, # base level for data files (e.g. "/ofo-share/str-disp_data")
                                site_name, # e.g. "delta"
                                focal_species, # 4-letter code
                                overstory_tree_filepath, # relative to `datadir`
                                seedling_plot_filepath, # relative to `datadir`
                                target_crs, # target CRS (to project the raw data sources to)
                                seedling_plot_area, # area of the plot in sq m
                                min_tree_height, # ignore trees shorter than this
                                density_raster_resolution = NULL, # grid cell size for calculating local tree density -- leave as NULL if not calculating density 
                                tree_distance_cutoff # ignore trees farther than this from a plot
) 
{
  
  require(sf)
  require(terra)
  
  ### Load the overstory tree and seedling data for the specified site
  overstory_trees = st_read(file.path(data_dir, overstory_tree_filepath)) |> 
    st_transform(target_crs)
  seedling_plots = st_read(file.path(data_dir, seedling_plot_filepath)) |> 
    st_transform(target_crs)
  
  # Convert overstory polys to points 
  # (may want to make this optional in case tree locations are already points)
  overstory_trees = st_centroid(overstory_trees)
  
  # Filter overstory to exclude SNAG and trees smaller than 10m tall 
  overstory_trees = overstory_trees |>
    filter(pred_class_ID != c("SNAG"), Z >= min_tree_height)
  
  # Get tree density raster for focal area -- using all live trees 
  cat("\n Calculating tree density raster")
  tree_density <- get_tree_density(overstory_trees, 
                                   seedling_plots, density_raster_resolution)

  # Remove unknown trees 
  overstory_trees <- overstory_trees |> 
    filter(pred_class_ID != "unknown")
  
  # Filter overstory to include only the focal species
  if (focal_species == "PINES") {
    overstory_trees = overstory_trees |>
      filter(pred_class_ID %in% c("PIPJ", "PILA"))
  } else if (focal_species == "FIRS") {
    overstory_trees = overstory_trees |>
      filter(pred_class_ID %in% c("ABCO", "PSME"))
  } else if (focal_species != "ALL") {
    overstory_trees = overstory_trees |>
      filter(pred_class_ID == focal_species)
  } 
  
  # Extract tree density data for the plot and tree locations 
  overstory_trees$tree_density = terra::extract(tree_density, overstory_trees, method = "bilinear")$count
  seedling_plots$tree_density = terra::extract(tree_density, seedling_plots, method = "bilinear")$count
  
  # Download elevation raster for focal area
  elev <- get_dem_data(overstory_trees, seedling_plots)
    
  # extract elevation data for the plot and tree locations 
  overstory_trees$elevation = terra::extract(elev, overstory_trees)
  seedling_plots$elevation = terra::extract(elev, seedling_plots)

 
  ### Prep overstory tree data: columns ID, x and y location, and size
  cat("Prepping overstory tree data")
  tree_coords = st_coordinates(overstory_trees, )
  overstory_trees$x = tree_coords[, 1]
  overstory_trees$y = tree_coords[, 2]
  
  # only keep trees > minimum tree height 
  overstory_trees = overstory_trees %>%
    filter(Z > min_tree_height) %>%
    mutate(size = Z) # "size" is just the height
  
  overstory_trees = overstory_trees %>%
    mutate(elevation_top = elevation + Z)
  
  overstory_tree_size <- overstory_trees$size
  
  # Prep seedling data with columns: plot id, x and y position, seedling count
  cat("Prepping seedling plot data")
  coords = st_coordinates(seedling_plots)
  seedling_plots$x = coords[, 1]
  seedling_plots$y = coords[, 2]
  
  # Specify the column name for the observed count based on the focal species
  count_col = paste0("count_", focal_species)
  
  seedling_plots = seedling_plots %>%
    dplyr::select(x, y, observed_count = any_of(count_col), elevation)

  # Assign a plot ID
  seedling_plots$seedling_plot_id <- seq_len(nrow(seedling_plots))
  
  # Get seedling count
  seedling_counts <- seedling_plots$observed_count
  
  # Round seedling count fractions up or down randomly (with p = fraction)
  round_frac <- function(x) {
    ifelse(runif(length(x)) < (x %% 1), ceiling(x), floor(x))
  }
  seedling_counts[seedling_counts %% 1 > 0] <- round_frac(seedling_counts[seedling_counts %% 1 > 0])
  
  ### Calculate distance matrix for distance between each overstory tree and each plot
  cat("\n Calculating distances")
  
  d2min <- 0.01
  
  dist_sq = outer(seedling_plots$x, overstory_trees$x, "-")^2 +
    outer(seedling_plots$y, overstory_trees$y, "-")^2
  dist_sq[dist_sq < d2min] <- d2min # Is this step necessary?
  
  r <- sqrt(dist_sq)
  
  # Any distances > tree_distance_cutoff  get set to NA
  r_cutoff = ifelse(r > tree_distance_cutoff, 0, r)
  r_cutoff = ifelse(r_cutoff == 0, NA, r)
  
  ## Add one dummy tree at tree_distance_cutoff m distance to each plot, so there are no plots with zero trees
  #r_cutoff = cbind(r_cutoff, rep(tree_distance_cutoff, nrow(r_cutoff)))
  
  # -- Prepare the objects needed to pass a "ragged matrix" of pairwise distances to stan 
  #number of non-NA values (overstory tree distances) per row (i.e. per seedling plot)
  cat("\n Prepping ragged data")
  n_nonNA = rowSums(!is.na(r_cutoff))
  r_cutoff_vecfull = as.vector(t(r_cutoff))
  r_cutoff_vec = r_cutoff_vecfull[!is.na(r_cutoff_vecfull)] # 1-D vector of all the non-NA values
  # index of the first non-NA value (tree distance) for each plot
  pos = cumsum(c(1, n_nonNA[-length(n_nonNA)]))
  
  ### Calc elevation difference (treetop to plot) matrix
  elev_diff = -outer(seedling_plots$elevation, overstory_trees$elevation_top, "-")
  
  ## Add one dummy tree at tree_distance_cutoff m distance with 0 height diff, so there are no plots with zero trees
  #elev_diff = cbind(elev_diff, rep(0, nrow(elev_diff)))
  
  # Prepare elevation differences to pass as a ragged array
  # (a long vector indexed by n_nonNA and pos)
  elevdiff_cutoff_vecfull = as.vector(t(elev_diff))
  elevdiff_cutoff_vec = elevdiff_cutoff_vecfull[!is.na(r_cutoff_vecfull)]
  
 # Create a long vector of tree densities corresponding to each overstory tree
  # (indexed by the "n_nonNA" and "pos" vectors)
  tree_density_vecfull = rep(overstory_trees$tree_density, nrow(seedling_plots))
  tree_density_vec = tree_density_vecfull[!is.na(r_cutoff_vecfull)]

  # Create a long vector of tree sizes corresponding to each overstory tree
  # (indexed by the "n_nonNA" and "pos" vectors)
  tree_size_vecfull = rep(overstory_trees$size, nrow(seedling_plots))
  tree_size_vec = tree_size_vecfull[!is.na(r_cutoff_vecfull)]
  
  # Add one dummy tree at tree_distance_cutoff m distance with 0 height diff and of average size, so there are no
  # plots with zero trees
  #overstory_tree_size = c(overstory_tree_size, mean(overstory_tree_size))
  #overstory_treesize_vec = overstory_tree_size[indexes_vec]
  
  ## Create the standard data object for modeling and return it
  disp_data <- list(  
    overstory_trees = overstory_trees, 
    seedling_plots = seedling_plots, 
    seedling_plot_area = seedling_plot_area, 
    distance_matrix = r,
    dem = elev, 
    tree_density = tree_density, 
    elev_diff_matrix = elev_diff, # full matrix of tree to plot elev diffs 
    # ragged array data for trees within cutoff distance
    pos = pos, # start positions for each plot in long vectors 
    n_overstory_trees = n_nonNA, # number of trees within cutoff distance of each plot
    dist_vector = r_cutoff_vec, 
    elev_diff_vector = elevdiff_cutoff_vec,
    tree_density_vector = tree_density_vec,
    tree_size_vector = tree_size_vec, 
    seedling_counts = seedling_counts,
    n_plots = length(seedling_counts))
  
  return(disp_data)
}


## Extract DEM data (elevs) at tree and plot points
get_dem_data <- function(overstory_trees, seedling_plots) {
  require(terra)
  require(sf)
  require(elevatr)
  
  # Create a combined polygon spanning all the trees and plots
  bound_trees = overstory_trees |> 
    st_combine() |> 
    st_buffer(100) |> 
    st_simplify()
  bound_plots = seedling_plots |> 
    st_combine() |> 
    st_buffer(100) |> 
    st_simplify()
  bound = st_union(bound_trees, bound_plots)

  # download the DEM 
  elev = get_elev_raster(bound |> st_as_sf(), z = 14, prj = 4326, src = "aws")

  return(elev)
}

## Create a raster layer of tree density values for focal area 
get_tree_density <- function(overstory_trees, seedling_plots, density_raster_resolution) {
  require(terra)
  require(sf)
  
  # Create a combined polygon spanning all the trees and plots
  bound_trees = overstory_trees |> st_buffer(100) |> st_union()
  bound_plots = seedling_plots |> st_buffer(100) |> st_union()
  bound = st_union(bound_trees, bound_plots)
  
  # create a template blank raster using the multipolygon bound for its extent
  # and a resolution of 10 m
  ext = st_bbox(bound)
  overstory_trees_pts = st_cast(overstory_trees, "POINT") # convert to points
  template_rast = rast(xmin = ext$xmin, xmax = ext$xmax, ymin = ext$ymin, ymax = ext$ymax, resolution = density_raster_resolution, crs = crs(overstory_trees_pts))
  setValues(template_rast, 0)
  
  # count the number of trees in each raster cell of template_rast
  tree_rast = rasterize(x = overstory_trees_pts, y = template_rast, fun = "count", background = 0)
  return(tree_rast)
}


### Simulating tree and plot data 

# Function simulate_dispdata() outputs the same kind of data object as get_dispdata(), with tree and plot information, plus distances, tree density, and elevation. It contains the data in both full matrix formats and ragged array formats. Rather than writing to file, it keeps the result in memory for immediately passing to the model fitting function. This allows testing models on repeatedly simulated data. 




simulate_dispdata = function(domain_size = 800, # length of one side of simulated square "domain" 
                        center_buffer = 200, # distance from central area with trees to edge of "domain" 
                        trees_per_hectare = 100, # density of trees to simulate
                        n_plots = 200, # number of seedling plots to simulate
                        a = 30,     # scale parameter for exppow dispersal kernel
                        k = 0.5,    # shape parameter for exppow dispersal kernel
                        b = 10,     # fecundity coefficient 
                        seedling_plot_area = 201,  # size of each plot in m²
                        elev_range = 100, # elevation range (m) for simulated DEM 
                        min_tree_height = 10, # min tree height (m) for simulation 
                        max_tree_height = 30, # max tree height (m) for simulation 
                        density_raster_resolution = 15, # grid cell size for calculating local tree density 
                        tree_distance_cutoff = 300 # ignore trees farther than this from a plot
                        
) {
  
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  require(matrixStats)
  require(spatstat.random)
  
  # Define central area for trees
  tree_x_min <- center_buffer
  tree_x_max <- domain_size - center_buffer
  tree_y_min <- center_buffer
  tree_y_max <- domain_size - center_buffer
  tree_window <- owin(c(tree_x_min, tree_x_max), c(tree_y_min, tree_y_max))
  
  # Use Thomas process to generate tree locations with fractal clustering 
  # Parameters adapted from https://www.fs.usda.gov/nrs/pubs/jrnl/2022/nrs_2022_lister_001.pdf
  tree_pp <- rThomas(kappa = 0.01, scale = 3, mu = 10, win = tree_window)
  # Thin to desired density of tree locations
  n_trees <- trees_per_hectare * (domain_size - 2*center_buffer)^2/10000
  tree_samples <- sample(1:length(tree_pp$x), size = n_trees)
  overstory_trees <- data.frame(
    x = tree_pp$x[tree_samples],
    y = tree_pp$y[tree_samples],
    height = runif(n_trees, min_tree_height, max_tree_height)  # random tree heights
  )
  
  # Generate plot locations
  seedling_plots <- data.frame(
    x = runif(n_plots, 0, domain_size),
    y = runif(n_plots, 0, domain_size)
  )
  
  # Turn the tree and plot data frames into points objects 
  overstory_trees = st_as_sf(overstory_trees, coords = c("x", "y"), remove = FALSE)
  seedling_plots = st_as_sf(seedling_plots, coords = c("x", "y"), remove = FALSE)
  
  # simulate terrain data using perlin noise 
  require(ambient)
  noise <- noise_perlin(c(domain_size, domain_size), pertubation = 'normal')
  noise_rast <- rast(normalize(noise, to = c(0, 1)))
  elev <- noise_rast * elev_range
  
  # extract elevation data for the plot and tree locations 
  overstory_trees$elevation = terra::extract(elev, overstory_trees)[,2]
  seedling_plots$elevation = terra::extract(elev, seedling_plots)[,2]
  
  # get tree density raster for focal area 
  tree_density <- get_tree_density(overstory_trees, seedling_plots, density_raster_resolution)
  
  # extract tree density data for the plot and tree locations 
  overstory_trees$tree_density = terra::extract(tree_density, overstory_trees)$count
  seedling_plots$tree_density = terra::extract(tree_density, seedling_plots)$count

  # add the value of the treetop elevation (terrain plus tree height)
  # and stick on a tree id column 
  overstory_trees = overstory_trees %>%
    mutate(id = 1:nrow(overstory_trees)) |>
    mutate(elevation_top = elevation + height)
  
  # Simulate the seedling data 
  # NOTE NEED TO UPDATE TO INCORPORATE ELEV_DIFF AND TREE_DENSITY 
  # Simulate dispersal from the trees to the plots 
  seed_rain <- simulate_seed_rain(
    tree_df = as.data.frame(overstory_trees),
    plot_df = as.data.frame(seedling_plots),
    a,     # scale
    k,    # shape
    b,     # fecundity coefficient
    seedling_plot_area  # size of each plot in m²
  )
  # Add random mortality for transition to seedlings 
  seedling_plots$seedling_counts = rbinom(n = n_plots,
                   size = seed_rain$observed_seeds, 
                   p = 0.15)
  
  
  ### Calculate distance matrix for distance between each overstory tree and each plot
  
  d2min <- 0.01
  
  dist_sq = outer(seedling_plots$x, overstory_trees$x, "-")^2 +
    outer(seedling_plots$y, overstory_trees$y, "-")^2
  dist_sq[dist_sq < d2min] <- d2min # Is this step necessary?
  
  r <- sqrt(dist_sq)
  
  # Any distances > tree_distance_cutoff  get set to NA
  r_cutoff = ifelse(r > tree_distance_cutoff, 0, r)
  r_cutoff = ifelse(r_cutoff == 0, NA, r)

  # -- Prepare the objects needed to pass a "ragged matrix" of pairwise distances to stan 
  #number of non-NA values (overstory tree distances) per row (i.e. per seedling plot)
  n_nonNA = rowSums(!is.na(r_cutoff))
  r_cutoff_vecfull = as.vector(t(r_cutoff))
  r_cutoff_vec = r_cutoff_vecfull[!is.na(r_cutoff_vecfull)] # 1-D vector of all the non-NA values
  # index of the first non-NA value (tree distance) for each plot
  pos = cumsum(c(1, n_nonNA[-length(n_nonNA)]))
  
  ### Calc elevation difference (treetop to plot) matrix
  elev_diff = -outer(seedling_plots$elevation, overstory_trees$elevation_top, "-")
  
  # Prepare elevation differences to pass as a ragged array
  # (a long vector indexed by n_nonNA and pos)
  elevdiff_cutoff_vecfull = as.vector(t(elev_diff))
  elevdiff_cutoff_vec = elevdiff_cutoff_vecfull[!is.na(r_cutoff_vecfull)]
  
  # Create a long vector of tree densities corresponding to each overstory tree
  # (indexed by the "n_nonNA" and "pos" vectors)
  tree_density_vecfull = rep(overstory_trees$tree_density, nrow(seedling_plots))
  tree_density_vec = tree_density_vecfull[!is.na(r_cutoff_vecfull)]
  
  # Create a long vector of tree sizes corresponding to each overstory tree
  # (indexed by the "n_nonNA" and "pos" vectors)
  tree_size_vecfull = rep(overstory_trees$height, nrow(seedling_plots))
  tree_size_vec = tree_size_vecfull[!is.na(r_cutoff_vecfull)]

  ## Create the standard data object for modeling and return it
  disp_data <- list(  
    overstory_trees = overstory_trees, 
    seedling_plots = seedling_plots, 
    seedling_plot_area = seedling_plot_area, 
    distance_matrix = r,
    dem = elev, 
    tree_density = tree_density, 
    elev_diff_matrix = elev_diff, # full matrix of tree to plot elev diffs 
    # ragged array data for trees within cutoff distance
    pos = pos, # start positions for each plot in long vectors 
    n_overstory_trees = n_nonNA, # number of trees within cutoff distance of each plot
    dist_vector = r_cutoff_vec, 
    elev_diff_vector = elevdiff_cutoff_vec,
    tree_density_vector = tree_density_vec,
    tree_size_vector = tree_size_vec, 
    seedling_counts = seedling_plots$seedling_counts, 
    n_plots = nrow(seedling_plots))
  
  return(disp_data)
}


## Simulate the dispersal process 
simulate_seed_rain <- function(tree_df, plot_df, 
                               a, k, b, 
                               seedling_plot_area = 1
                               ) {
  #' Simulate seed rain using exponential power kernel
  #'  - May want to add other kernels later 
  #' 
  #' @param tree_df Data frame with columns x, y, height
  #' @param plot_df Data frame with columns x, y
  #' @param a Kernel scale parameter
  #' @param k Kernel shape parameter
  #' @param b Fecundity per m of height
  #' @param seedling_plot_area Area of each seedling plot (m²)
  #' 
  #' @return data_frame with expected and observed seed counts for each plot
 
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
  expected <- colSums(kernel_matrix * fecundity) * seedling_plot_area
  
  # Simulate observed seeds using Poisson
  observed <- rpois(n_plots, expected)
  
  # Return augmented plot_df
  out_df <- plot_df
  out_df$expected_seeds <- expected
  out_df$observed_seeds <- observed
  return(out_df)
}


