## Function to compare a drone map with a ground map and, ultimately, compare all drone maps in the directory with the ground map

library(tidyverse)
library(sf)
library(terra)
library(here)


# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

#### Inputs ####

# Project area boundary
focal_area = st_read(data("drone/boundaries/delta-boundary-from-photos.gpkg")) #%>% st_transform(32610)

# Ortho file
ortho_file = data("/storage/disp-uav/products/delta/delta_meta033_20210415T0728_ortho_dsm.tif")
ortho = rast(ortho_file)
ortho_agg = aggregate(ortho,fact=3)

writeRaster(ortho_agg,data("drone/processed-products/delta_meta033_20210415T0728_ortho_dsm_agg.tif"))
