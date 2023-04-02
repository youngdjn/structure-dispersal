## Takes a CHM and makes a map of treetops

library(sf)
library(terra)
library(here)
library(tidyverse)
library(lidR)

data_dir = readLines(here("data_dir.txt"), n=1)

## Convenience functions ####
#source(here("scripts/convenience_functions.R"))



#### START ####


## Layers to process
# plots_file = "surveys/crater/intermediate/crater_foc.geojson"
# plot_buffer = 400
focal_area = "boundaries/delta-boundary-reduced.gpkg"
chm_file = "chms/DeltaB-120m_20230310T1701_dsm.tif"
treetop_out_file = "ttops/DeltaB-120m_20230310T1701_ttops.gpkg"

focal_area = "boundaries/emerald-boundary-from-photos.gpkg"
chm_file = "chms/emerald-120m_20230401T2215_chm.tif"
treetop_out_file = "ttops/emerald-120m_20230401T2215_ttops.gpkg"


# # get focal area
# # in this case by loading focal plots and buffering by 300 m
# 
# plots = st_read(datadir(plots_file))
# plots = plots %>%
#   filter(BurnClass != "Low")
# 
# plots_buff = st_buffer(plots,plot_buffer) %>% st_union
# 
# # extra buffer for vwf, to be cropped out later
# plots_buff_extra = st_buffer(plots_buff,20)
plots_buff = st_read(file.path(data_dir, focal_area))
plots_buff_extra = st_buffer(plots_buff_extra, 5)

# find the chm file
chm = rast(file.path(data_dir, chm_file))


# crop and mask it
chm_crop = crop(chm,plots_buff_extra %>% st_transform(crs(chm)) %>% vect)
chm_mask = mask(chm_crop,plots_buff_extra %>% st_transform(crs(chm)) %>% vect)
chm = chm_mask

cat("Fix extraneous vals")
# if it's taller than 50, set to 50
# chm[chm>55] = 55.1 ## need to set this larger for future projects probably
chm[chm < 0] = -0.1


chm_res = res(chm) %>% mean
# pixels_smooth_1 = round(((0.5/chm_res)-1)/2)*2 + 1 # round to nearest odd integer
# pixels_smooth_2 = round(((1/chm_res)-1)/2)*2 + 1
# pixels_smooth_3 = round(((1.5/chm_res)-1)/2)*2 + 1
# pixels_smooth_4 = round(((2/chm_res)-1)/2)*2 + 1

# apply 3x3 smooth
weights = matrix(1,nrow=3,ncol=3)
chm_smooth = focal(chm, weights, fun=mean)


cat("Detecting trees\n")

lin <- function(x){
  win = x*0.11 + 0
  win[win < 0.5] = 0.5
  win[win > 100] = 100
  return(win)
  } # window filter function to use in next step

treetops <- locate_trees(chm_smooth, lmf(ws = lin, shape = "circular", hmin = 5))

treetops = as(treetops,"sf")

# crop to the inner buffer
treetops = st_intersection(treetops,plots_buff %>% st_transform(st_crs(treetops)))

## Save treetops

st_write(treetops,file.path(data_dir, treetop_out_file), delete_dsn=TRUE, quiet=TRUE)


# ## Save buffer
# st_write(plots_buff, datadir("temp/plots_buff2.gpkg"))
