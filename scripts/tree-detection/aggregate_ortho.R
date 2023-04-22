## Function to prepare reduced-res orthomosaics for purposes of determining live/dead trees

library(tidyverse)
library(sf)
library(terra)
library(here)
library(furrr)

write("TMP = /ofo-share/tmp/", file=file.path('~/.Renviron'))


# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

sites = c("delta", "chips", "valley", "crater")

agg_ortho = function(site) {

  ortho_file = paste0("cross-site/orthos/", site, ".tif")
  chm_file = paste0("cross-site/chms/", site, ".tif")
  boundary_file = paste0("cross-site/boundaries/", site, ".gpkg")
  agg_ortho_out_file = paste0("cross-site/orthos-crop-agg/", site, ".tif")
  
  # load CHM
  chm = rast(file.path(data_dir, chm_file))
  
  # load boundary
  boundary = st_read(file.path(data_dir, boundary_file))
  
  # load ortho, crop, aggregate, mask
  ortho = rast(file.path(data_dir, ortho_file))
  
  ortho = crop(ortho, boundary |> st_transform(crs(ortho)))
  
  agg_fact = mean(res(chm))/ mean(res(ortho)) * 4
  ortho_agg = aggregate(ortho, ceiling(agg_fact), na.rm = TRUE, cores = 4)
  
  ortho_agg = mask(ortho_agg, boundary |> st_transform(crs(ortho)))
  
  writeRaster(ortho_agg, file.path(data_dir, agg_ortho_out_file), overwrite = TRUE)
}

plan(multisession(workers = 4))
future_walk(sites, agg_ortho)
