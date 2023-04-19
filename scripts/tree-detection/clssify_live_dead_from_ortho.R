## Detect and remove dead trees based on lack of green and small crown footprint.

library(tidyverse)
library(sf)
library(terra)
library(here)


# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

sites  = c("crater", "delta", "chips", "valley")


for(site in sites)  {
  
  ttops_file = paste0("cross-site/ttops/", site, ".gpkg")
  crowns_file = paste0("cross-site/crowns/", site, ".gpkg")
  ortho_file = paste0("cross-site/orthos-crop-agg/", site, ".tif")
  # chm_file = paste0("cross-site/chms/", site, ".tif")
  boundary_file = paste0("cross-site/boundaries/", site, ".gpkg")
  ttops_out_file = paste0("cross-site/ttops-live/", site, ".gpkg")
  crowns_out_file = paste0("cross-site/crowns-live/", site, ".gpkg")
  
  
  
  # load treetops
  ttops = st_read(file.path(data_dir, ttops_file))
  # keep only if over 10 m
  ttops = ttops[ttops$Z > 10,]
  
  # load crowns
  crowns = st_read(file.path(data_dir, crowns_file))
  # keep only if over 10 m
  crowns = crowns[crowns$Z > 10,]
  
  
  if(site != "crater") { 
  
    # load ortho
    ortho = rast(file.path(data_dir, ortho_file))
    
    # compute RGR 
    rgr = (ortho[[2]] - ortho[[1]]) / (ortho[[1]] + ortho[[2]])
    
    # writeRaster(rgr, file.path(data_dir, "temp/rgr.tif"), overwrite = TRUE)
    
    # get brightness, to exclude those pixels
    brightness = ortho[[1]] + ortho[[2]] + ortho[[3]]
    shadowfree = brightness > 150
    noshadow_mask = shadowfree
    noshadow_mask[values(noshadow_mask) == FALSE] = NA
    
    rgr = rgr * noshadow_mask
    
    # writeRaster(rgr, file.path(data_dir, "temp/rgr_masked.tif"), overwrite = TRUE)
    # writeRaster(brightness, file.path(data_dir, "temp/brightness.tif"), overwrite = TRUE)
    
    rgr_extract = terra::extract(rgr,crowns |> st_transform(crs(rgr)), fun=mean, na.rm = TRUE)[,2]
    
    crowns$rgr = rgr_extract
    
    # st_write(crowns, file.path(data_dir, "temp/crowns_rgr.gpkg"), delete_dsn = TRUE)
    
    
    # Get crown area
    crown_area = st_area(crowns)
    large_enough = crown_area > units::set_units(3, "m^2")
    
    # Greenness threshold
    green_enough = rgr_extract > 0.02
    
    crowns$live = large_enough & green_enough
    
    
    ## Bind this classifier onto the ttops
    crowns_nonsp = crowns
    st_geometry(crowns_nonsp) = NULL
    crowns_nonsp = crowns_nonsp |>
      select(treeID, live)
    
    ttops = left_join(ttops, crowns_nonsp, by = "treeID") |>
      filter(!is.na(live)) |>
      filter(live)
    
    crowns = crowns |>
      filter(!is.na(live)) |>
      filter(live)
  
  }
  
  st_write(ttops,file.path(data_dir, ttops_out_file), delete_dsn = TRUE)
  st_write(crowns,file.path(data_dir, crowns_out_file), delete_dsn = TRUE)

}