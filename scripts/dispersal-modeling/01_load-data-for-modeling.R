# Get data for model and package into standard data object for modeling 

# Function get_dispdata() outputs a large data object with tree and plot information, plus distances, tree density, and elevation. It contains the data in both full matrix formats and ragged array formats. Rather than writing to file, it keeps the result in memory for immediately passing to the model fitting function. This allows both testing of repeatedly simulated data, and testing how the data prep parameters affect inferences. 

get_dispdata = function(data_dir, # base level for data files (e.g. "/ofo-share/str-disp_data")
                                site_name, # e.g. "delta"
                                focal_species, # 4-letter code
                                overstory_tree_filepath, # relative to `datadir`
                                seedling_plot_filepath, # relative to `datadir`
                                target_crs, # target CRS (to project the raw data sources to)
                                seedling_plot_area, # area of the plot in sq m
                                min_tree_height, # ignore trees shorter than this
                                density_raster_resolution, # grid cell size for calculating local tree density 
                                tree_distance_cutoff # ignore trees farther than this from a plot
                                
) {
  
  ### Load the overstory tree and seedling data for the specified site
  overstory_trees = st_read(file.path(data_dir, overstory_tree_filepath)) |>
    st_transform(target_crs)
  seedling_plots = st_read(file.path(data_dir, seedling_plot_filepath)) |>
    st_transform(target_crs)
  
  # Convert overstory polys to points 
  # (may want to make this optional in case tree locations are already points)
  overstory_trees = st_centroid(overstory_trees)
  
  # Filter overstory to exclude SNAG and include only the focal species
  overstory_trees = overstory_trees |>
    filter(!(pred_class_ID %in% c("SNAG", "unknown")))
  
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
  
  # download elevation raster for focal area
  elev <- get_dem_data(overstory_trees, seedling_plots)
  
  # extract elevation data for the plot and tree locations 
  overstory_trees$elevation = terra::extract(elev, overstory_trees)
  seedling_plots$elevation = terra::extract(elev, seedling_plots)
  
  # get tree density raster for focal area 
  tree_density <- get_tree_density(overstory_trees, seedling_plots, density_raster_resolution)
  
  # extract tree density data for the plot and tree locations 
  overstory_trees$tree_density = terra::extract(tree_density, overstory_trees)$count
  seedling_plots$tree_density = terra::extract(tree_density, seedling_plots)$count
 
  ### Prep overstory tree data: columns ID, x and y location, and size
  tree_coords = st_coordinates(overstory_trees, )
  overstory_trees$x = tree_coords[, 1]
  overstory_trees$y = tree_coords[, 2]
  
  # only keep trees > minimum tree height 
  overstory_trees = overstory_trees %>%
    filter(Z > min_tree_height) %>%
    mutate(size = Z) # "size" is just the height
  
  overstory_trees = overstory_trees %>%
    select(id = treeID, x, y, size, elevation, Z) |>
    mutate(elevation_top = elevation + Z)
  
  overstory_tree_size <- overstory_trees$size
  
  # Prep seedling data with columns: plot id, x and y position, seedling count
  coords = st_coordinates(seedling_plots)
  seedling_plots$x = coords[, 1]
  seedling_plots$y = coords[, 2]
  
  # Specify the column name for the observed count based on the focal species
  count_col = paste0("count_", focal_species)
  
  seedling_plots = seedling_plots %>%
    dplyr::select(x, y, observed_count = one_of(count_col), elevation)

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
    seedling_counts = seedling_counts)
  
  return(disp_data)
}


## Extract DEM data (elevs) at tree and plot points
get_dem_data <- function(overstory_trees, seedling_plots) {
  require(terra)
  require(sf)
  require(elevatr)
  
  # Create a combined polygon spanning all the trees and plots
  bound_trees = overstory_trees |> st_buffer(100) |> st_union()
  bound_plots = seedling_plots |> st_buffer(100) |> st_union()
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

