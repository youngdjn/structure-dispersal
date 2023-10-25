## Assign field species to drone trees that match the field trees

library(tidyverse)
library(sf)
library(units)

# set up connection to python for linear sum assignment function
library(reticulate)
reticulate::use_condaenv("base", required = TRUE)
lapsolver = import("lapsolver")

MAX_MATCHING_DIST = set_units(10, "m")


# Load field trees
trees_field = st_read("/ofo-share/str-disp_drone-data-partial/cross-site/field-reference-trees/stems_v3_aligned.gpkg") |>
  st_transform(3310)

# Load field perims
perims_field = st_read("/ofo-share/str-disp_drone-data-partial/cross-site/field-reference-trees/plot_bounds_v3_manuallyCorrected.gpkg") |>
  st_transform(3310)

# Load drone trees
trees_drone = st_read("/ofo-share/str-disp_drone-data-partial/cross-site/ttops/chips.gpkg") |>
  st_transform(3310)


## Function to check if a tree is taller than any other tree
any_taller = function(i, tree_map) {
  focal_height = tree_map[i,]$Height
  dist = st_distance(tree_map[i,], tree_map) %>% as.vector
  heightdiff = tree_map$Height - focal_height
  dist_thresh = heightdiff * 0.1 + 1
  focal_is_under = ((focal_height < tree_map$Height) & (dist < dist_thresh))
  return(any(focal_is_under))
}


## Get the field tree dataset into the expected format (column names, etc)
trees_field$Height = trees_field$ht_top

## Get the drone tree dataset into the expected format (column names, etc)
trees_drone = trees_drone |>
mutate(height = Z,
       Height = Z) 




## Plan for matching. Loss function is:
#     Use linear sum assignment to match trees
#     For all the matches, sum the 3D distance between pairings
#     Compute the mean distance
# Optimize the x, y shift of field trees




# Start with Chips
trees_field_foc = trees_field |>
    filter(stem_map_name == "Chips_1")

perim_field_foc = perims_field |>
    filter(stem_map_name == "Chips_1")

perim_buff = st_buffer(perim_field_foc, MAX_MATCHING_DIST)

# Get drone trees within the buffered field plot
trees_drone_foc = trees_drone |>
  st_intersection(perim_buff)

# Determine whether under a neighbor
#trees_field$under_neighbor = map_lgl(1:nrow(trees_field), any_taller, tree_map = trees_field)


# ## For testing, shift the field trees

# # data frame of amount to shift
# shift_df <- data.frame(x=-30, y=-30) %>% 
#   st_as_sf(coords = c("x", "y"))

# # add the two geometries together (just the geometry columns) to shift the field trees
# trees_field_foc$geom <- trees_field_foc$geom + shift_df$geometry
# st_crs(trees_field_foc) = st_crs(trees_field)





# Get distance matrix
dist_mat = st_distance(trees_field_foc, trees_drone_foc) %>% as.matrix

# Disallow matches for distances over the threshold
dist_mat[dist_mat > (MAX_MATCHING_DIST)] = NA
dist_mat = dist_mat^2

# Run linear sum assignment
matches = lapsolver$solve_dense(dist_mat)

# Select the matched trees and get distances between pairs
trees_field_foc_match = trees_field_foc[matches[[1]] + 1, ]
trees_drone_foc_match = trees_drone_foc[matches[[2]] + 1, ]

dists = st_distance(trees_field_foc_match, trees_drone_foc_match, by_element = TRUE) %>% as.vector



# make lines pairing matches for vis
lines = st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, st_geometry(trees_field_foc_match), st_geometry(trees_drone_foc_match), SIMPLIFY=FALSE))
st_crs(lines) = st_crs(trees_field_foc_match)

st_write(lines, "/ofo-share/scratch-derek/pairing-lines_squared-dist.gpkg", delete_dsn = TRUE)

# Filter both tree sets to comparable heights (field tree height can be < 10)...need to think if a mod to matching code is necessary given that field trees go down to 10...what if drone tree is 10 and field was measured as 9.9 and excluded?


st_write(trees_field_foc, "/ofo-share/scratch-derek/trees_field_v2.gpkg", delete_dsn = TRUE)
st_write(trees_drone, "/ofo-share/scratch-derek/trees_drone_v2.gpkg", delete_dsn = TRUE)

# Make matches by calling command line arg

# Assign species to drone trees based on the matched field tree
# Export as crown polygon