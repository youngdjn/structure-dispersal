library(tidyverse)
library(here)
library(sf)
library(elevatr)

prep_data = function(dataset_name,               # site-species-version (version is for keeping track of other things)
                     overstory_tree_filepath,    # relative to `datadir`
                     seedling_plot_filepath,     # relative to `datadir`
                     seedling_plot_crs,
                     target_crs,                 # target CRS (to project the raw data sources to)
                     seedling_plot_area         # area of the plot in sq m
                     #TODO: specify which species
                    ) {
  
  ### Load the overstory tree and seedling data for the specified site
  overstory_trees = st_read(file.path(data_dir, overstory_tree_filepath)) %>% st_transform(target_crs)
  seedling_plots = st_read(file.path(data_dir, seedling_plot_filepath)) |> st_transform(target_crs)
  
  # # Extract DEM data (elevs) at tree and plot points
  overstory_trees = get_elev_point(overstory_trees)
  seedling_plots = get_elev_point(seedling_plots)

  overstory_trees$elevation = runif(nrow(overstory_trees), 0, 100)
  seedling_plots$elevation = runif(nrow(seedling_plots), 0, 100)
  
  ### Prep overstory tree data: columns ID, x and y location, and size
  tree_coords = st_coordinates(overstory_trees, )
  overstory_trees$x = tree_coords[,1]
  overstory_trees$y = tree_coords[,2]
  
  # Only keep trees > 10 m tall
  overstory_trees = overstory_trees %>%
    filter(Z > 10) %>%
    mutate(size = Z) # "size" is just the height
  
  overstory_trees = overstory_trees %>%
    select(id = treeID, x, y, size, elevation, Z) |>
    mutate(elevation_top = elevation + Z)
  
  st_geometry(overstory_trees) = NULL
  
  overstory_tree_size <- overstory_trees$size
  
  
  ### Prep seedling data with columns: plot id, x and y position, and years with the value being the number of seedlings in the year
  
  coords = st_coordinates(seedling_plots)
  seedling_plots$x = coords[,1]
  seedling_plots$y = coords[,2]
  
  ## For Crater Fire only, need to thin plots to exclude low severity and convert seedling density to count
  if(grepl("crater",dataset_name)) {
    seedling_plots = seedling_plots %>%
      filter(BurnClass != "Low")
  }
  
  seedling_plots = seedling_plots %>%
    select(x,y,observed_count,elevation)
  
  st_geometry(seedling_plots) = NULL
  
  # Assign a plot ID
  seedling_plots$seedling_plot_id <- 1:nrow(seedling_plots)
  
  # Get seedling count
  seedling_counts <- seedling_plots$observed_count

  
  # Round seedling count fractions up or down randomly (with p = fraction)
  round_frac <- function(x) {
    ifelse(runif(length(x)) < (x %% 1), ceiling(x), floor(x))
  }
  seedling_counts[seedling_counts %% 1 > 0] <- round_frac(seedling_counts[seedling_counts %% 1 > 0])
  
  
  
  ### Calculate distance matrix for distance between each overstory tree and each plot
  
  d2min <- 0.01
  
  dist_sq <- outer(seedling_plots$x, overstory_trees$x, "-")^2 + outer(seedling_plots$y, overstory_trees$y, "-")^2
  dist_sq[dist_sq < d2min] <- d2min # Is this step necessary?
  
  
  r <- sqrt(dist_sq)
  
  # Any distances > 300  get set to NA
  r_cutoff = ifelse(r > 300, 0, r)
  r_cutoff = ifelse(r_cutoff == 0, NA, r)
  
  ## Add one dummy tree at 300 m distance to each plot, so there are no plots with zero trees
  r_cutoff = cbind(r_cutoff, rep(300, nrow(r_cutoff)))  
  
  ## Prepare the objects needed to pass a "ragged array" of pairwise distances to stan
  n_nonNA = rowSums(!is.na(r_cutoff)) # number of non-NA values (overstory tree distances) per row (i.e. per seedling plot)
  r_cutoff_vecfull = as.vector(t(r_cutoff))
  r_cutoff_vec = r_cutoff_vecfull[!is.na(r_cutoff_vecfull)] # 1-D vector of all the non-NA values
  pos = cumsum(c(1, n_nonNA[-length(n_nonNA)])) # index of the first non-NA value (tree distance) for each plot
  
  ### Calc elevation diffrence (treetop to plot) matrix
  elev_diff = -outer(seedling_plots$elevation, overstory_trees$elevation_top, "-")
  
  ## Add one dummy tree at 300 m distance with 0 height diff, so there are no plots with zero trees
  elev_diff = cbind(elev_diff, rep(0, nrow(elev_diff)))  
  
  ## Prepare it as well to pass as a ragged array, but dropping the same trees as were dropped from the dist vector
  elevdiff_cutoff_vecfull = as.vector(t(elev_diff))
  elevdiff_cutoff_vec = elevdiff_cutoff_vecfull[!is.na(r_cutoff_vecfull)]

  ### Get the overstory tree sizes in the same format (one vector of sizes, indexed using the `n_nonNA` and `pos` vectors)
  ### Note: this will replicate tree sizes when they occur in multiple plots; previously with the square dist mat approach they were not replicated because all trees (all the same trees) were used for every plot. This new approach requires 15x the number of values to store the tree sizes. Not sure of its effect on stan memory and speed.
  # Get list of vector indexes to the tree IDs. Each list element corresponds to a plot, and it contains a vector that lists the column indexes for the trees that match that plot (i.e. are within 300 m)
  indexes = apply(r_cutoff, 1, function(x) which(!is.na(x)))
  indexes_vec = unlist(indexes)
  
  ## Add one dummy tree at 300 m distance with 0 height diff and of average size, so there are no plots with zero trees
  overstory_tree_size = c(overstory_tree_size, mean(overstory_tree_size))
  
  overstory_treesize_vec = overstory_tree_size[indexes_vec]
  
  ### Write to file: distance matrix, overstory tree size, observed seedling count, and plot area
  prepped_data_dir = file.path(data_dir, "prepped-for-stan", dataset_name)
  dir.create(prepped_data_dir, recursive=TRUE)
  
  write_file(as.character(seedling_plot_area),file.path(prepped_data_dir, "plot-area.txt"))
  write_lines(r_cutoff_vec, file.path(prepped_data_dir, "dist-vector.txt"))
  write_lines(elevdiff_cutoff_vec, file.path(prepped_data_dir, "htdiff-vector.txt"))
  write_lines(overstory_treesize_vec, file.path(prepped_data_dir, "overstory-treesize-vector.txt"))
  write_lines(seedling_counts, file.path(prepped_data_dir, "seedling-counts.txt"))
  write_lines(n_nonNA, file.path(prepped_data_dir, "n-overstory-trees.txt"))
  write_lines(pos, file.path(prepped_data_dir, "pos.txt"))
  
}
