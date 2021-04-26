## Takes a CHM and makes a map of treetops

library(sf)
library(raster)
library(ForestTools)
library(here)
library(purrr)
library(furrr)
library(tidyverse)

data_dir = readLines(here("data_dir.txt"), n=1)

## Convenience functions ####
source(here("scripts/convenience_functions.R"))



#### START ####

### Parameters
smooth = 1
### Define params
a = -0.5
b = 0.07
c = 0

### Parameters
smooth = 1
### Define params
a = 0
b = 0.06
c = 0


# find the chm file
chm_file = data("drone/processed-products/delta_meta033_20210415T0728_chm.tif")
chm = raster(chm_file)

cat("Fix extraneous vals")
# if it's taller than 50, set to 50
chm[chm>55] = 55.1 ## need to set this larger for future projects probably
chm[chm < 0] = -0.1


chm_res = res(chm) %>% mean
pixels_smooth_1 = round(((0.5/chm_res)-1)/2)*2 + 1 # round to nearest odd integer
pixels_smooth_2 = round(((1/chm_res)-1)/2)*2 + 1
pixels_smooth_3 = round(((1.5/chm_res)-1)/2)*2 + 1
pixels_smooth_4 = round(((2/chm_res)-1)/2)*2 + 1

chms = list()

cat("Smoothing\n")

if(0 %in% smooth) {
  chms[["smooth0"]] = chm
}

if(1 %in% smooth) {
  weights = matrix(1,nrow=pixels_smooth_1,ncol=pixels_smooth_1)
  chms[["smooth1"]] = focal(chm, weights, fun=mean)
}

if(2 %in% smooth) {
  weights = matrix(1,nrow=pixels_smooth_2,ncol=pixels_smooth_2)
  chms[["smooth2"]] = focal(chm, weights, fun=mean)
}

if(3 %in% smooth) {
  weights = matrix(1,nrow=pixels_smooth_3,ncol=pixels_smooth_3)
  chms[["smooth3"]] = focal(chm, weights, fun=mean)
}

if(4 %in% smooth) {
  weights = matrix(1,nrow=pixels_smooth_4,ncol=pixels_smooth_4)
  chms[["smooth4"]] = focal(chm, weights, fun=mean)
}


if(smooth == 0) {
  chm = chms[["smooth0"]]
} else if(smooth == 1) {
  chm = chms[["smooth1"]]
} else if(smooth == 2) {
  chm = chms[["smooth2"]]
} else if(smooth == 3) {
  chm = chms[["smooth3"]]
} else if(smooth == 4) {
  chm = chms[["smooth4"]]
} else if(smooth == 5) {
  chm = chms[["smooth5"]]
} else if(smooth == 6) {
  chm = chms[["smooth6"]]
} else if(smooth == 7) {
  chm = chms[["smooth7"]]
} else if(smooth == 8) {
  chm = chms[["smooth8"]]
} else {
  stop("Requested smoothed chm",smooth,"not provided.")
}

cat("Detecting trees\n")

lin <- function(x){x^2*c + x*b + a} # window filter function to use in next step

treetops <- try(
  vwf(CHM = chm, winFun = lin, minHeight = 5, maxWinDiameter = 199)
  ,silent=TRUE)

  if(class(treetops) == "try-error") {
    cat("***** Skipping because of VWF error ******\n" )
    return(FALSE)
  }

treetops = as(treetops,"sf") %>% st_transform(4326)

## Save treetops

# create dir if doesn't exist, then write

st_write(treetops,paste0(data("drone/processed-products/detected-trees/"),"delta_meta033_20210415T0728_ttops-vwf142.gpkg"), delete_dsn=TRUE, quiet=TRUE)

