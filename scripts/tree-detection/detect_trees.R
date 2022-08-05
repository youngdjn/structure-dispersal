## Takes a CHM and makes a map of treetops

library(sf)
library(terra)
library(ForestTools)
library(here)
library(tidyverse)

data_dir = readLines(here("data_dir.txt"), n=1)

## Convenience functions ####
source(here("scripts/convenience_functions.R"))



#### START ####


## Layers to process
plots_file = "surveys/crater/intermediate/crater_foc.geojson"
plot_buffer = 400
chm_file = "drone/processed-products/crater/Crater120m_20210122T1219_chm.tif"
treetop_out_file = "drone/processed-products/crater/Crater120m_20210122T1219_ttops_vwf196.gpkg"



# get focal area
# in this case by loading focal plots and buffering by 300 m

plots = st_read(datadir(plots_file))
plots = plots %>%
  filter(BurnClass != "Low")

plots_buff = st_buffer(plots,plot_buffer) %>% st_union

# extra buffer for vwf, to be cropped out later
plots_buff_extra = st_buffer(plots_buff,20)

# find the chm file
chm = rast(datadir(chm_file))


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


weights = matrix(1,nrow=9,ncol=9)
chm_smooth = focal(chm, weights, fun=mean)


cat("Detecting trees\n")

lin <- function(x){x*0.04 + 0} # window filter function to use in next step

treetops <- vwf(CHM = raster::raster(chm_smooth), winFun = lin, minHeight = 5)

treetops = as(treetops,"sf")

# crop to the inner buffer
treetops = st_intersection(treetops,plots_buff %>% st_transform(st_crs(treetops)))

## Save treetops

st_write(treetops,datadir(treetop_out_file), delete_dsn=TRUE, quiet=TRUE)
