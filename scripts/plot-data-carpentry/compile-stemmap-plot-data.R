library(tidyverse)
library(sf)
library(here)
library(readxl)
library(lubridate)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

#### Load data ####
### Datasheets and kobo
ds_basestation = read_excel(data("surveys/main/unprocessed/datasheets/dispersal-data-entry.xlsx"),sheet="base_station")
ds_species = read_excel(data("surveys/main/unprocessed/datasheets/dispersal-data-entry.xlsx"),sheet="seedl_cone", col_types="text")
ds_stem_plot = read_excel(data("surveys/main/unprocessed/datasheets/dispersal-data-entry.xlsx"),sheet="stem_map_plot")
ds_stem_tree = read_excel(data("surveys/main/unprocessed/datasheets/dispersal-data-entry.xlsx"),sheet="stem_map_tree")

base_shifts = read.csv(data("surveys/main/unprocessed/base_shifts/base_shifts.csv"))

### Emlid data
chips1 = st_read(data("surveys/main/unprocessed/emlid/Chips-stemmap.geojson")) %>% mutate(fire = "Chips", stem_map_id = "1", base_loc = 1)
chips1_abco = st_read(data("surveys/main/unprocessed/emlid/Chips-abco-stemmap.geojson")) %>% mutate(fire = "Chips", stem_map_id = "1_ABCO", base_loc = 1)
delta1 = st_read(data("surveys/main/unprocessed/emlid/Delta-trees.geojson")) %>% mutate(fire = "Delta", stem_map_id = "1", base_loc = 1)
delta2 = st_read(data("surveys/main/unprocessed/emlid/Delta_trees_2.geojson")) %>% mutate(fire = "Delta", stem_map_id = "2", base_loc = 2)
lassic1 = st_read(data("surveys/main/unprocessed/emlid/Lasic-stem-1.geojson")) %>% mutate(fire = "Lassic", stem_map_id = "1", base_loc = 1)
lassic2 = st_read(data("surveys/main/unprocessed/emlid/Lassic-stem-2.geojson")) %>% mutate(fire = "Lassic", stem_map_id = "2", base_loc = 1)
valley1 = st_read(data("surveys/main/unprocessed/emlid/Valley-trees2.geojson")) %>% mutate(fire = "Valley", stem_map_id = "1", base_loc = 2)

emlid = rbind(chips1,chips1_abco,delta1,delta2,lassic1,lassic2,valley1)


## fix some emlid typos
emlid[emlid$collection.start == "2020-07-16 00:08:47","name"] = 151 # first instance of 152 should be 151, not 152
emlid[emlid$collection.start == "2020-07-02 21:36:24","name"] = 59 # second instance of 58 should be 59
emlid[emlid$collection.start == "2020-07-02 22:00:37","name"] = 65 # first instance of 66 should be 65
emlid[emlid$collection.start == "2020-08-20 23:50:48","name"] = 34 # renamed a lassic 43 (the one that came bt 33 and 35) to 34


# remove an extraneous point
emlid = emlid %>%
  filter(name != "Point 90") %>%
  filter(collection.start != "2020-07-02 21:23:17") %>% # remove the first delta 50
  filter(collection.start != "2020-07-02 23:57:59") # remove the first delta 99
  

## get the datasheet stem map name in the right character format
ds_stem_tree = ds_stem_tree %>%
  mutate(stem_map_id = stem_map_id %>% as.character %>% str_replace(fixed(".0"),""))



# Prep emlid data, with date in right format
emlid = emlid %>%
  select(date = collection.end, point_name = name, fire, stem_map_id, base_loc) %>%
  mutate(date = date - hours(1) - hours(7)) %>% # subtract an hour because st_read seems to add an hour
  mutate(date = force_tz(date,"UTC")) %>%
  mutate(date = str_sub(date,1,10))

# Clean emlid data
emlid = emlid %>%
  mutate(point_name = str_replace(point_name,fixed("Point "),""))



# Chips trees > 353 and < 1000 are in SM 2. (They were saved in the same Emlid project as SM1)
emlid = emlid %>%
  mutate(tree_id_int = as.integer(point_name))

emlid[which(emlid$fire == "Chips" & emlid$tree_id_int > 352 & emlid$tree_id_int < 1000),"stem_map_id"] = 2

emlid = emlid %>%
  select(-tree_id_int)



## Check for duplicates
emlid_dup = emlid %>%
  group_by(point_name,fire,stem_map_id) %>%
  summarize(n = n())



## Shift emlid trees and plot centers based on base offsets

# Pull in plot shifts
emlid = left_join(emlid,base_shifts)

# Temporary inspection of base locs
st_write(emlid,data("surveys/main/intermediate/temp_stemmap_emlid.geojson"), delete_dsn = TRUE)


## Get the geometries, to shift them (in m coord system)
emlid_geom = st_geometry(emlid) %>% st_transform(3310)

## Do the shifts, for each fire & base loc
for(fire in unique(emlid$fire)) {
  emlid_fire = emlid[emlid$fire == fire,]
  for(base_loc in unique(emlid_fire$base_loc)) {
    rows_to_shift = which(emlid$fire == fire & emlid$base_loc == base_loc)
    shifts = base_shifts[base_shifts$fire == fire & base_shifts$base_loc == base_loc,]
    emlid_geom[rows_to_shift] = emlid_geom[rows_to_shift] + c(shifts$base_shift_x,shifts$base_shift_y)
  }
}

## Bring the shifted geom back in, remove shift columns
emlid = st_set_geometry(emlid,emlid_geom %>% st_transform(4326)) %>%
  select(-base_shift_x,-base_shift_y)



## Separate out the emlid trees and emlid plot centers
emlid = emlid %>%
  filter(!str_starts(point_name, "point")) # these were imported points to define the bounds of the Chips ABCO plot, not used for mapping

emlid_centers = emlid %>%
  filter(str_starts(point_name,"C")) %>%
  mutate(point_name = str_replace_all(point_name, fixed("-"),""))

emlid_trees = emlid %>%
  filter(!str_starts(point_name,"C")) %>%
  rename(tree_id = point_name)


## Rename an incorrectly named plot center
emlid_centers[emlid_centers$fire == "Delta" & emlid_centers$point_name == "C16","point_name"] = "C16A"



## Compute each manual tree's position relative to plot center, then add the plot center loc
manual_trees = ds_stem_tree %>%
  filter(!is.na(center_id)) %>%
  mutate(center_id = toupper(center_id)) %>%
  mutate(x_offset = sin(deg2rad(azimuth)) * distance,
         y_offset = cos(deg2rad(azimuth)) * distance)

## For each manual tree, get the x and y of the corresponding manual plot center
coords = st_coordinates(emlid_centers %>% st_transform(3310)) %>% as.data.frame
emlid_centers_foc = emlid_centers %>%
  mutate(center_x = coords$X,
         center_y = coords$Y) %>%
  select(center_id = point_name,
         fire, center_x, center_y)

st_geometry(emlid_centers_foc) = NULL

manual_trees = left_join(manual_trees, emlid_centers_foc, by=c("fire","center_id"))

manual_trees_locs = manual_trees %>%
  mutate(x = center_x + x_offset,
         y = center_y + y_offset) %>%
  select(fire, stem_map_id, tree_id, x, y)

manual_trees_locs = st_as_sf(manual_trees_locs, coords = c("x","y"), crs=3310) %>% st_transform(4326) %>%
  mutate(manual = TRUE)


### Append the manual and emlid tree locs
emlid_trees_locs = emlid_trees %>%
  select(fire, stem_map_id, tree_id) %>%
  st_zm(drop=TRUE) %>%
  mutate(manual=FALSE)

tree_locs = rbind(manual_trees_locs,emlid_trees_locs)

## get x and y
coords = st_coordinates(tree_locs) %>% as.data.frame
tree_locs$x = coords$X
tree_locs$y = coords$Y
st_geometry(tree_locs) = NULL

tree_locs = tree_locs %>%
  mutate(tree_id = as.integer(tree_id))







### Pull the coords into the tree data
trees = left_join(ds_stem_tree,tree_locs, by=c("fire","stem_map_id","tree_id"))


### Make sure there are no duplicates
trees_dup = trees %>%
  group_by(fire, stem_map_id, tree_id) %>%
  summarize(n = n())
# No duplicates!


## Make it spatial
trees_sf = st_as_sf(trees,coords=c("x","y"),crs=4326)

## Write
write_csv(trees,data("surveys/main/processed/stems.csv"))
st_write(trees_sf,data("surveys/main/processed/stems.geojson"), delete_dsn = TRUE)




#####!!! Determine emlid Base 2 offsets relative to base 1 (Lassic and Eiler)
# Also apply to regen



