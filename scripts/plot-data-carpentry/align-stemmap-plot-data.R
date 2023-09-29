## This script addresses a case where a large stem map was created as a compilation of multiple smaller contiguous stem maps with different center points that might be shifted differently relative to reality
# Take the original tree point locations and shifted tree point locations,
# determine the shifts for each stemmap-center combo (only for trees actually shifted), and
# apply the shifts to all trees in that combo


library(tidyverse)
library(sf)

unshifted = st_read("/ofo-share/str-disp_data-partial/surveys/main/processed/stems_v2.gpkg") |>
    st_transform(3310)
shifted = st_read("/ofo-share/str-disp_data-partial/surveys/main/processed/stems_v2_manualyShiftedSubset.gpkg") |>
    st_transform(3310)


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