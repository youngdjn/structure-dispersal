## Calculate tree density around hypothetical Chips seed traps


library(tidyverse)
library(sf)
library(raster)
library(here)


# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

## Load focal region
focal_area = st_read(data("scouting/boundaries/chips_focal.geojson")) %>% st_transform(3310)
highsev_area = st_read(data("scouting/boundaries/chips_focal_highsev.geojson")) %>% st_transform(3310)

## Load tree data
trees = st_read(data("scouting/detected_trees/ChipsA_treetops_live.geojson")) %>% st_transform(3310)

## Put a grid of hypoetical seed traps within the highsev area
  traps = st_make_grid(highsev_area,cellsize=20,what="centers") %>% st_as_sf
traps$trap_id = 1:nrow(traps)


## For each trap, get the density of trees within 20m and 35 m

trap_buffers_20m = traps %>% st_buffer(20)
trap_buffers_35m = traps %>% st_buffer(35)
  
trap_buffers_20m$inplot = st_within(trap_buffers_20m,focal_area, sparse=FALSE)
trap_buffers_35m$inplot = st_within(trap_buffers_35m,focal_area, sparse=FALSE)


## Use only trees over 10 m
trees = trees %>%
  filter(height > 10)

trap_buffers_20m$treecount = st_within(trees,trap_buffers_20m , sparse=FALSE) %>% colSums()
trap_buffers_35m$treecount = st_within(trees,trap_buffers_35m , sparse=FALSE) %>% colSums()

trap_buffers_20m = trap_buffers_20m %>%
  mutate_at(vars(starts_with("treecount")), ~./0.1256) %>%
  mutate_at(vars(starts_with("treecount")), ~ifelse(inplot, ., NA)) %>%
  rename_at(vars(starts_with("treecount")), ~str_replace(.,"treecount","density") %>% paste0("_20m"))

trap_buffers_35m = trap_buffers_35m %>%
  mutate_at(vars(starts_with("treecount")), ~./0.3847) %>%
  mutate_at(vars(starts_with("treecount")), ~ifelse(inplot, ., NA)) %>%
  rename_at(vars(starts_with("treecount")), ~str_replace(.,"treecount","density") %>% paste0("_35m"))



st_geometry(trap_buffers_20m) = NULL
st_geometry(trap_buffers_35m) = NULL

traps = left_join(traps,trap_buffers_20m %>% select(trap_id,starts_with("density")))
traps = left_join(traps,trap_buffers_35m %>% select(trap_id,starts_with("density")))

st_write(traps %>% st_transform(4326), data("scouting/density_estimates/chips_seed_trap_tree_densities.geojson"))

hist(traps$density_35m,breaks=20)


