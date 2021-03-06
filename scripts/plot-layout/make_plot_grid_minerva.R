library(sf)
library(tidyverse)
library(here)

data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

### Convenience function
round.choose <- function(x, roundTo) {
  x + (roundTo - x %% roundTo)
}

## Open the perimeters within which to place plots
plot_perimeters <- st_read(data("regen/plot_layout/minerva/minerva_candidate_areas.gpkg"),stringsAsFactors=FALSE) %>% st_transform(3310)
plot_perimeters$subarea = plot_perimeters$Name#1:nrow(plot_perimeters)
# ## Boggs only: buffer in to avoid boundary plots
# plot_perimeters = st_buffer(plot_perimeters, -12)
# plot_perimeters$subarea = c(1,1,1,1,1,1,2,2,3,3)

## Make grid of points within them # for Delta used 50 m
perims_focal = plot_perimeters %>% filter(Description == "")
plots_denser = st_make_grid(perims_focal,cellsize=35,what="centers") %>% st_as_sf() %>% st_intersection(perims_focal)

perims_focal = plot_perimeters %>% filter(Description == "b")
plots_sparser = st_make_grid(perims_focal,cellsize=45,what="centers") %>% st_as_sf() %>% st_intersection(perims_focal)

plots_w_subunit = rbind(plots_denser,plots_sparser)

st_write(plots_w_subunit, data("regen/plot_layout/minerva/minerva_points_01_prethin.gpkg"), delete_dsn = TRUE)


### Open the manually thinned plots
plots = st_read(data("regen/plot_layout/minerva/minerva_points_01_prethin.gpkg"))
geom = st_coordinates(plots)
plots = plots[!is.nan(geom[,1]),]

plots$subarea = plots$subarea

## Round latitude to the nearest 20 so we can add increasing numbers within rows, then along rows
plots$lat_round = st_coordinates(plots)[,2] %>% round.choose(25)
plots$long = st_coordinates(plots)[,1]

## sort by rounded lat, then long
plots = plots %>%
  arrange(-lat_round,long)

## give plots unique, ascending numbers *within their clusters*, format nicely
plots = plots %>%
  group_by(subarea) %>%
  mutate(field_id = row_number()) %>%
  mutate(field_id = str_pad(field_id,3,pad="0")) %>% # pad with leading zeros
  mutate(field_id = str_c(subarea,field_id))
  
## write
plots$name = plots$field_id
plots$Name = plots$field_id
plots$id = plots$field_id
st_write(plots %>% st_transform(4326), data("regen/plot_layout/forfield/minerva/minerva_plots_01.kml"), delete_dsn = TRUE)

## make and write a 300 m buffer for GPS flight
plots_buffer = plots %>% st_union %>% st_buffer(300)
st_write(plots_buffer %>% st_transform(4326),data("regen/plot_layout/forfield/boggs_project_buffer.kml"))



# 
# 
# ## make the circular stem map plot boundaries
# stemmap_center = st_read(data("regen/plot_layout/delta_stemmap_center2.gpkg")) %>% st_transform(3310)
# stemmap_perim = st_buffer(stemmap_center,60)
# stemmap_perim$id = 1:nrow(stemmap_perim)
# 
# ## save the plot bounds, stem map centers, plots, project area as KML for export to device
# st_write(stemmap_center %>% st_transform(4326), data("regen/plot_layout/forfield/delta_stemmap_center.geojson"), delete_dsn = TRUE)
# st_write(stemmap_center %>% st_transform(4326), data("regen/plot_layout/forfield/delta_stemmap_center.kml"), delete_dsn = TRUE)
# st_write(stemmap_perim %>% st_transform(4326), data("regen/plot_layout/forfield/delta_stemmap_perim.kml"), delete_dsn = TRUE)
# 
# plots = st_read(data("regen/plot_layout/delta_points_01_forfield.gpkg"))
# plots = plots %>%
#   mutate(id = field_id,
#          name = field_id)
# st_write(plots %>% st_transform(4326), data("regen/plot_layout/forfield/delta_plots_01.geojson"), delete_dsn = TRUE)
# st_write(plots %>% st_transform(4326), data("regen/plot_layout/forfield/delta_plots_01.kml"), delete_dsn = TRUE)



### export plot list and coords to open or print
plots_geo = st_transform(plots,4326)
plots_geo$lat = st_coordinates(plots_geo)[,2]
plots_geo$lon = st_coordinates(plots_geo)[,1]
plots_geo = plots_geo %>%
  select(id = field_id,lat,lon) %>%
  mutate(lat = round(lat,6),
         lon = round(lon,6)) %>%
  arrange(id)
st_geometry(plots_geo) = NULL
write_csv(plots_geo,data("regen/plot_layout/forfield/minerva/minerva_plots_list.csv"))
