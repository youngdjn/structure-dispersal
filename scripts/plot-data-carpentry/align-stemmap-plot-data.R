## This script addresses a case where a large stem map was created as a compilation of multiple smaller contiguous stem maps with different center points that might be shifted differently relative to reality
# Take the original tree point locations and shifted tree point locations,
# determine the shifts for each stemmap-center combo (only for trees actually shifted), and
# apply the shifts to all trees in that combo


library(tidyverse)
library(sf)

unshifted_orig = st_read("/ofo-share/str-disp_data-partial/surveys/main/processed/stems_v2.gpkg") |>
    st_transform(3310)
shifted_orig = st_read("/ofo-share/str-disp_data-partial/surveys/main/processed/stems_v2_manualyShiftedSubset.gpkg") |>
    st_transform(3310)

unshifted = unshifted_orig
shifted = shifted_orig

# Test if a tree has been shifted: does it have the exact same coordinates in both datasets?

# get their coords
coords = st_coordinates(unshifted)
unshifted = unshifted |>
    mutate(x_unshifted = coords[, 1], y_unshifted = coords[, 2]) |>
    select(fire, stem_map_id, center_id, tree_id, x_unshifted, y_unshifted) |>
    st_drop_geometry()
coords = st_coordinates(shifted)
shifted = shifted |>
    mutate(x_shifted = coords[, 1], y_shifted = coords[, 2]) |>
    select(fire, stem_map_id, center_id, tree_id, x_shifted, y_shifted) |>
    st_drop_geometry()

# join them
trees = full_join(unshifted, shifted)

# test if the coords are the same
trees = trees |>
    mutate(shifted = !(x_unshifted == x_shifted & y_unshifted == y_shifted))

# For each stemmap-center combo, determine the shift, considering only trees that were actually shifted

# get the mean shift for each stemmap-center combo
shifts = trees |>
    filter(shifted) |>
    group_by(fire, stem_map_id, center_id) |>
    summarize(x_shift = mean(x_shifted - x_unshifted), y_shift = mean(y_shifted - y_unshifted)) |>
    ungroup()

## Apply the shifts to all trees in that combo

# join the shifts to the trees
trees = trees |>
    left_join(shifts) |>
    mutate(x_shift = ifelse(is.na(x_shift), 0, x_shift), y_shift = ifelse(is.na(y_shift), 0, y_shift))

# apply the shifts
trees = trees |>
    mutate(x_autoshifted = x_unshifted + x_shift, y_autoshifted = y_unshifted + y_shift)

# pull in all original attributes
trees = trees |>
    select(fire, stem_map_id, center_id, tree_id, x_autoshifted, y_autoshifted) |>
    left_join(unshifted_orig |> st_drop_geometry())

# convert to spatial (sf)
trees = trees |>
    st_as_sf(coords = c("x_autoshifted", "y_autoshifted"), crs = 3310) |>
    st_transform(4326) |>
    select(fire, stem_map_id, center_id, tree_id, date, species, ht_lowest_ndl, ht_top, pct_prefire_green, pct_current_green, distance, azimuth, notes)

# get lat/lon and convert back to tabular
coords = st_coordinates(trees)

trees = trees |>
    mutate(lon = coords[, 1], lat = coords[, 2])

# delete delta fire trees manually measured from center c16a in stem map 1
trees = trees[(!((trees$fire == "Delta" & trees$stem_map_id == 1 & ((trees$center_id == "c16a" & !is.na(trees$center_id)))))), ]

## Assign a stem map name that cobines fire and stem_map_id (but also removes the number suffix of the Delta_3 and Lassic_3 fires)
trees[trees$fire == "Delta_3", "fire"] = "Delta"
trees[trees$fire == "Lassic_3", "stem_map_id"] = 2 # Lassic_3 is actually a subset of Lassic 2 but with a different base loc so it needed to be shifted separately, but then merged with Lassic 2
trees[trees$fire == "Lassic_3", "fire"] = "Lassic"

trees$stem_map_name = paste(trees$fire, trees$stem_map_id, sep = "_")
trees = trees |>
    select(-fire, -stem_map_id) |>
    select(stem_map_name, everything())

# write to file
write_csv(trees |> st_drop_geometry(), "/ofo-share/str-disp_data-partial/surveys/main/processed/stems_v2_aligned.csv")
#st_write(trees, "/ofo-share/str-disp_data-partial/surveys/main/processed/stems_v2_aligned.gpkg", delete_dsn = TRUE)
