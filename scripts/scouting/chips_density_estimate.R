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
traps = st_make_grid(highsev_area,cellsize=20,what="centers")


## For each trap, get the density of trees within









