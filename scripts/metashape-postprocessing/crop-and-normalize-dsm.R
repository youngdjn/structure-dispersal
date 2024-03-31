## Takes a DSM, converts to CHM, rescales to 0.12 m, and saves

library(sf)
library(here)
library(purrr)
library(tidyverse)
library(terra)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions and main functions ####

site = "lassic"


# load DTM
dtm = rast(file.path(data_dir, "cross-site", "dtms", "photogrammetry", paste0(site, ".tif")))

## get DSM layer from metashape output
dsm_file = file.path(data_dir, "cross-site", "dsms", paste0(site, ".tif"))


# file to write
filename = file.path(data_dir, "cross-site", "chms", paste0(site, ".tif"))

# get site boundary
boundary = st_read(file.path(data_dir, "cross-site", "boundaries", datadir(paste0(site, ".gpkg"))))

# Crop to study area boundary
dsm = rast(dsm_file)
dsm = crop(dsm, boundary |> st_transform(crs(dsm)))
dsm = mask(dsm, boundary |> st_transform(crs(dsm)))

dtm = crop(dtm, boundary |> st_transform(crs(dtm)))
dtm = mask(dtm, boundary |> st_transform(crs(dtm)))


## upscale to 0.12 m
dsm_upscale = project(dsm, y = "EPSG:3310", res = 0.12, method = "bilinear")


## interpolate the the DTM to the res, extent, etc of the DSM
dtm_interp = project(dtm, dsm_upscale, method = "bilinear")


#### Calculate canopy height model ####
#### and save to tif

## calculate canopy height model
chm = dsm_upscale - dtm_interp


## create dir if doesn't exist, then write
writeRaster(chm, filename, overwrite = TRUE)

gc()