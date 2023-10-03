# Take mapped trees, and for each plot_id and stemmap_id combo, compute a convex hull.
# Then manually adjust in QGIS to straight lines, and to exclude incomplete subplots based on subplot coners provided to crew

library(tidyverse)
library(sf)

trees = read_csv("/ofo-share/str-disp_data-partial/surveys/main/processed/stems_v2_aligned.csv") |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

# get each stem map name
stem_maps = unique(trees$stem_map_name)

# for each stem map, get the plot bounds
plot_bounds = list() # list of sf objects
plot_bboxes = list()
for(stem_map in stem_maps){
    # get the trees for this stem map
    trees_stem_map = trees |>
        filter(stem_map_name == stem_map)
    # get the plot bounds
    plot_bounds[[stem_map]] = trees_stem_map |>
        group_by(stem_map_name) |>
        summarize() |>
        st_convex_hull()
    # get the plot bbox
    plot_bboxes[[stem_map]] = trees_stem_map |>
        st_transform(3857) |>
        group_by(stem_map_name) |>
        summarize() |>
        st_bbox() |>
        st_as_sfc() |>
        st_as_sf() |>
        mutate(stem_map_name = stem_map)
}   

# Combine and buffer out by 1 m so the trees are clearly inside the plot bounds
plot_bounds = bind_rows(plot_bounds) |> st_transform(3310) |> st_buffer(1.5) |> st_simplify(dTolerance = 0.3) |> st_transform(4326)
plot_bboxes = bind_rows(plot_bboxes) |> st_transform(3310) |> st_buffer(1.5) |> st_simplify(dTolerance = 0.3) |> st_transform(4326)


# write to file
st_write(plot_bounds, "/ofo-share/str-disp_data-partial/surveys/main/processed/plot_bounds.gpkg", delete_dsn = TRUE)
st_write(plot_bboxes, "/ofo-share/str-disp_data-partial/surveys/main/processed/plot_bboxes.gpkg", delete_dsn = TRUE)
