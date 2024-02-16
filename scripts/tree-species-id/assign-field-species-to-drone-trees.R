## Assign field species to drone trees that match the field trees

library(tidyverse)
library(sf)
library(units)
library(tictoc)

source("/ofo-share/utils/tree-map-comparison/lib/match-trees.R")


# Load field trees
trees_field = st_read("/ofo-share/str-disp_drone-data-partial/cross-site/field-reference-trees/stems_v4.gpkg") |>
  st_transform(3310)

# Load field perims
perims_field = st_read("/ofo-share/str-disp_drone-data-partial/cross-site/field-reference-trees/plot_bounds_v4.gpkg") |>
  st_transform(3310)


## Get the field tree dataset into the expected format (column names, etc)
trees_field$Height = trees_field$ht_top

# If there's "snag" in the species, make the spacies "SNAG" and make percent green 0
trees_field = trees_field |>
  mutate(species = toupper(species)) |>
  mutate(species = ifelse(str_detect(species, regex("snag", ignore_case = TRUE)), "SNAG", species),
        pct_current_green = ifelse(str_detect(species, regex("snag", ignore_case = TRUE)), 0, pct_current_green)) |>
  # assume that if percent current green is NA, it's 0
  mutate(pct_current_green = ifelse(is.na(pct_current_green) | pct_current_green == "NA", 0, pct_current_green))

# Make a data frame of all the stem maps we want, so we can loop through it
stemmaps = data.frame(stem_map_name = c("Chips_1", "Chips_1_ABCO", "Chips_2", "Delta_1", "Delta_2", "Delta_3", "Valley_1", "Lassic_1", "Lassic_2"),
                      fire_name =     c("chips",   "chips",        "chips",   "delta",   "delta",    "delta", "valley",    "lassic",    "lassic"))

crowns_drone_w_field_data = data.frame()

for(i in 1:nrow(stemmaps)) {

  stem_map_name_foc = stemmaps[i, ]$stem_map_name
  fire_name_foc = stemmaps[i, ]$fire_name

  # Load field trees
  trees_field_foc = trees_field |>
      filter(stem_map_name == stem_map_name_foc) |>
      mutate(observed_tree_id = tree_id)

  # Load field perim
  perim_field_foc = perims_field |>
      filter(stem_map_name == stem_map_name_foc)

  # Load drone trees (points and crowns) and crop to focal area around field reference trees
  trees_drone = st_read(paste0("/ofo-share/str-disp_drone-data-partial/cross-site/ttops/", fire_name_foc, ".gpkg")) |>
    st_transform(3310)

  ## Get the drone tree dataset into the expected format (column names, etc)
  trees_drone = trees_drone |>
    select(predicted_tree_id = treeID,
           height = Z) 

  # Load drone crowns
  crowns_drone = st_read(paste0("/ofo-share/str-disp_drone-data-partial/cross-site/crowns/", fire_name_foc, ".gpkg")) |>
    st_transform(3310) |>
    select(predicted_tree_id = treeID)

  # Designate area beyond the field stem map perimeter to allow field trees to match to drone trees
  perim_buff = st_buffer(perim_field_foc, 10)

  ## Get drone trees and crowns within the buffered field plot
  trees_drone_foc = trees_drone |>
    st_intersection(perim_buff)

  crowns_drone_foc_idxs = crowns_drone |>
    st_intersects(perim_buff, sparse = FALSE)

  crowns_drone_foc = crowns_drone[crowns_drone_foc_idxs, ]

  # Run matching and filter to only matched trees
  matches = match_trees_singlestratum(trees_field_foc,
                                      trees_drone_foc,
                                      search_height_proportion = 0.5,
                                      search_distance_fun_slope = 0.1,
                                      search_distance_fun_intercept = 1)

  matches = matches |>
    filter(!is.na(final_predicted_tree_match_id))

  ## Take the crown polygons and look up the species of the matched field tree
  # First get only the columns we need from the field tree data
  trees_field_foc_simp = matches |>
    st_drop_geometry() |>
    select(observed_tree_id,
           species_observed = species,
           height_observed = ht_top,
           percent_green_observed = pct_current_green,
           stem_map_name,
           predicted_tree_id = final_predicted_tree_match_id) |>
    mutate(live_observed = as.numeric(percent_green_observed) > 0,
           percent_green_observed = as.numeric(percent_green_observed),
           fire = fire_name_foc)

  #  Join the field tree data to the drone crown polygons, also pull in the photogrammetry tree height (from the treetop points)
  crowns_drone_foc_w_field_data = crowns_drone_foc |>
    inner_join(trees_field_foc_simp, by = "predicted_tree_id") |>
    left_join(trees_drone_foc |> st_drop_geometry(), by = join_by(predicted_tree_id, stem_map_name)) |>
    rename(height_chm = height)

  # Bind onto running data frame
  crowns_drone_w_field_data = rbind(crowns_drone_w_field_data, crowns_drone_foc_w_field_data)

}


st_write(crowns_drone_w_field_data, paste0("/ofo-share/str-disp_drone-data-partial/cross-site/crowns-w-field-labels/crowns_drone_w_field_data.gpkg"), delete_dsn = TRUE)





# ## Vis: Make lines between matched trees, as well as drone and field reference trees
# # Make two aligned data frames of the matching trees (field and drone; only those that match)
# trees_field_foc_match = trees_field_foc[match(matches$observed_tree_id, trees_field_foc$observed_tree_id), ]
# trees_drone_foc_match = trees_drone_foc[match(matches$final_predicted_tree_match_id, trees_drone_foc$predicted_tree_id),]
# # Make lines pairing matches for vis
# lines = st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, st_geometry(trees_field_foc_match), st_geometry(trees_drone_foc_match), SIMPLIFY=FALSE))
# lines = st_sf(lines)
# lines$observed_tree_id = trees_field_foc_match$observed_tree_id
# lines$predicted_tree_id = trees_drone_foc_match$predicted_tree_id

# st_crs(lines) = st_crs(trees_field_foc_match)

# st_write(lines, "/ofo-share/scratch-derek/pairing-lines_custom.gpkg", delete_dsn = TRUE)

# # Also write out drone and field ref trees for vis
# st_write(trees_field_foc, "/ofo-share/scratch-derek/trees_field_v2.gpkg", delete_dsn = TRUE)
# st_write(trees_drone, "/ofo-share/scratch-derek/trees_drone_v2.gpkg", delete_dsn = TRUE)