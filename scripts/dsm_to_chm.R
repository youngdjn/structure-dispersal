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
  focal_area = st_read(data("scouting/boundaries/chips_focal.geojson")) #%>% st_transform(32610)
  
  # DTM
  dtm = raster(data("scouting/dtms/ChipsA_20200523T0842_dtm.tif"))
  
  # DSM file
  dsm_file = data("scouting/metashape_outputs/ChipsA_20200523T0842_dsm.tif")
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

# create dir if doesn't exist, then write
writeRaster(chm,data("scouting/chms/ChipsA_chm.tif"), overwrite=TRUE) # naming it metashape because it's just based on metashape dsm (and usgs dtm) -- to distinguish from one generated from point cloud

gc()
