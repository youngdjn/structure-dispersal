## Function to compare a drone map with a ground map and, ultimately, compare all drone maps in the directory with the ground map

library(tidyverse)
library(sf)
library(raster)
library(here)


# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

#### Inputs ####

# Project area boundary
focal_area = st_read(data("drone/boundaries/delta-boundary-from-photos.gpkg")) #%>% st_transform(32610)

# DTM
dtm = raster(data("drone/metashape-products/delta_meta033_20210415T0728_dtm.tif"))

# DSM file
dsm_file = data("drone/metashape-products/delta_meta033_20210415T0728_dsm.tif")
dsm = raster(dsm_file)

# crop and mask DSM to project roi
dsm = crop(dsm, focal_area %>% st_transform(crs(dsm)))
dsm = mask(dsm,focal_area %>% st_transform(crs(dsm)))

dtm = crop(dtm, focal_area %>% st_transform(crs(dtm)))
dtm = mask(dtm,focal_area %>% st_transform(crs(dtm)))

# interpolate the the dtm to the res, extent, etc of the DSM
dtm_interp = resample(dtm %>% projectRaster(crs=crs(dsm)),dsm)


#### Calculate canopy height model ####
#### and save to tif

# calculate canopy height model
chm = dsm - dtm_interp

# downscale to 0.12 m
chm = projectRaster(chm,res=0.12, crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", method="bilinear")


# create dir if doesn't exist, then write
writeRaster(chm,data("drone/processed-products/delta_meta033_20210415T0728_dtm.tif"), overwrite=TRUE) # naming it metashape because it's just based on metashape dsm (and usgs dtm) -- to distinguish from one generated from point cloud

gc()
