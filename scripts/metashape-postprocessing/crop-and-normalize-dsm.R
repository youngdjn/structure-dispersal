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

source(here("scripts/convenience_functions.R"))



#### DTM

dtm = rast(datadir("drone/metashape-products/crater/dem_usgs.tif"))

## get DSM layer from metashape output
dsm_file = datadir("drone/metashape-products/crater/Crater120m_20210122T1219_dsm.tif")

file_minus_extension = str_sub(dsm_file,1,-5)
fileparts = str_split(file_minus_extension,fixed("/"))[[1]]
filename_only = fileparts[length(fileparts)]
filename_no_dsm = str_replace(filename_only,"_dsm","")

# file to write
filename = datadir(paste0("drone/processed-products/crater/",filename_no_dsm,"_chm.tif"))

# # skip if file aleady exists
# if(file.exists(filename)) {
#   cat("Already exists:",filename,". Skipping.\n")
#   return(FALSE)
# }

dsm = rast(dsm_file)


# upscale to 0.12 m
dsm_upscale = project(dsm, y = "EPSG:3310", res=0.12, method="bilinear")


# interpolate the the DTM to the res, extent, etc of the DSM
dtm_interp = resample(dtm,dsm_upscale, method="bilinear")


#### Calculate canopy height model ####
#### and save to tif

# calculate canopy height model
chm = dsm_upscale - dtm_interp


# create dir if doesn't exist, then write
writeRaster(chm,filename) # naming it metashape because it's just based on metashape dsm (and usgs dtm) -- to distinguish from one generated from point cloud

gc()

