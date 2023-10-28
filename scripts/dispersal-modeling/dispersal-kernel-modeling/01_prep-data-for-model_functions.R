library(tidyverse)
library(here)
library(sf)
library(elevatr)


## Functions for computing the desired size metric from the drone-derived tree height
source(here("scripts/dispersal-modeling/dispersal-kernel-modeling/tree-size-functions.R"))


prep_data = function(dataset_name,               # site-species-sizemetric-version (version is for keeping track of other things)
                     overstory_tree_filepath,    # relative to `datadir`
                     seedling_plot_filepath,     # relative to `datadir`
                     seedling_plot_crs,
                     target_crs,                 # target CRS (to project the raw data sources to)
                     seedling_plot_area,         # area of the plot in sq m
                     #TODO: specify which species
                     size_function_name) {      # the name of the function for obtaining the desired tree size metric from drone-derived tree height (metric defined in tree-size-functions.R)
  
  ### Load the site data as specified
  overstory_trees = st_read(datadir(overstory_tree_filepath)) %>% st_transform(target_crs)
  seedling_plots = st_read(datadir(seedling_plot_filepath)) |> st_transform(target_crs)
  
  size_function = get(size_function_name) # function for computing size from height defined in tree-size-functions.R
  
  # Extract DEM data (elevs) at tree and plot points
  overstory_trees = get_elev_point(overstory_trees)
  seedling_plots = get_elev_point(seedling_plots)

  
  ### Prep overstory tree data: columns ID, x and y location, and size
  tree_coords = st_coordinates(overstory_trees, )
  overstory_trees$x = tree_coords[,1]
  overstory_trees$y = tree_coords[,2]
  
  overstory_trees = overstory_trees %>%
    filter(Z > 10) %>%
    mutate(size = size_function(Z))
  
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
  
  
  # Round fractions up or down randomly (with p = fraction)
  round_frac <- function(x) {
    ifelse(runif(length(x)) < (x %% 1), ceiling(x), floor(x))
  }
  seedling_counts[seedling_counts %% 1 > 0] <- round_frac(seedling_counts[seedling_counts %% 1 > 0])
  
  
  
  ### Calculate distance matrix for distance between each overstory tree and each plot
  
  d2min <- 0.01
  
  dist_sq <- outer(seedling_plots$x, overstory_trees$x, "-")^2 + outer(seedling_plots$y, overstory_trees$y, "-")^2
  dist_sq[dist_sq < d2min] <- d2min # Is this step necessary?
  
  r <- sqrt(dist_sq)

  ht_diff = -outer(seedling_plots$elevation, overstory_trees$elevation_top, "-")
  
  
  ### Write to file: distance matrix, overstory tree size, observed seedling count, and plot area
  prepped_data_dir = datadir(paste0("prepped-for-stan/", dataset_name))
  dir.create(prepped_data_dir, recursive=TRUE)
  
  write_file(as.character(seedling_plot_area),paste0(prepped_data_dir,"/plot-area.txt"))
  write.table(r,paste0(prepped_data_dir,"/dist-mat.txt"), row.names=FALSE, col.names=FALSE)
  write.table(ht_diff,paste0(prepped_data_dir,"/ht-diff-mat.txt"), row.names=FALSE, col.names=FALSE)
  write_lines(overstory_tree_size, paste0(prepped_data_dir, "/overstory-tree-size.txt"))
  write_lines(seedling_counts, paste0(prepped_data_dir, "/seedling-counts.txt"))
  
}
