## Function to compare a drone map with a ground map and, ultimately, compare all drone maps in the directory with the ground map

library(tidyverse)
library(sf)
library(terra)
library(here)


# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


# load treetops
ttops = st_read(datadir("drone/processed-products/detected-trees/delta_meta033_20210415T0728_ttops-vwf142.gpkg"))
# keep only if over 5 m
ttops = ttops[ttops$height > 5,]

# load aggregated ortho
ortho = rast(datadir("drone/processed-products/delta_meta033_20210415T0728_ortho_dsm_agg.tif"))

chm = rast(datadir("drone/processed-products/delta_meta033_20210415T0728_chm.tif"))

# compute focal means around each tree
rgr = ortho$del_1 / ortho$del_2



# exclude anomalously high values that may have come from division by near zero
rgr[rgr > 4] = NA


rgr_focal = terra::focal(rgr,w=25, fun="mean")




# find skinny trees (probably snags)
ground = chm < 3
# get how wide each tree is (number of non-ground pixels within 25 m window)
pct_ground = focal(ground,w=25,fun="mean")


## extract values at detected trees
tree_rgr = extract(rgr_focal,ttops %>% st_transform(crs(rgr_focal)) %>% vect)
tree_pctground = extract(pct_ground,ttops %>% st_transform(crs(pct_ground)) %>% vect)

ttops$rgr_coarse = tree_rgr[,2]
ttops$pct_ground = tree_pctground[,2]

ttops$green_coarse = ttops$rgr_coarse < 0.9
ttops$solid_tree = ttops$pct_ground < 0.7

ttops$confident_tree = ttops$solid_tree & ttops$green_coarse

st_write(ttops,datadir("drone/processed-products/filtered-trees/ttops_filtered.gpkg"))
st_write(ttops %>% filter(confident_tree == TRUE),datadir("drone/processed-products/filtered-trees/ttops_filtered_live.gpkg"))
