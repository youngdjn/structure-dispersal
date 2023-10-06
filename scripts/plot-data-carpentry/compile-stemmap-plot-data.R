library(tidyverse)
library(sf)
library(here)
library(readxl)
library(lubridate)

# The root of the data directory
data_dir <- readLines(here("data_dir.txt"), n = 1)

source(here("scripts/convenience_functions.R"))

#### Load data ####
### Datasheets and kobo
ds_basestation <- read_excel(datadir("surveys/main/unprocessed/datasheets/dispersal-data-entry_2020-2021.xlsx"), sheet = "base_station")
ds_species <- read_excel(datadir("surveys/main/unprocessed/datasheets/dispersal-data-entry_2020-2021.xlsx"), sheet = "seedl_cone", col_types = "text")
ds_stem_plot <- read_excel(datadir("surveys/main/unprocessed/datasheets/dispersal-data-entry_2020-2021.xlsx"), sheet = "stem_map_plot")
ds_stem_tree <- read_excel(datadir("surveys/main/unprocessed/datasheets/dispersal-data-entry_2020-2021.xlsx"), sheet = "stem_map_tree") |>
  mutate(base_station_loc = as.character(base_station_loc))

# Also Creek stem map data from 2021 (2 plots)
ds_stem_tree_creek <- read_csv(datadir("surveys/main/unprocessed/datasheets/stemmap_trees_creek2021.csv"))
# Need to get it in the same format as the other stem map data
ds_stem_tree_creek[which(ds_stem_tree_creek$ht_top == "17/1") , "ht_top"] = "17.1"
ds_stem_tree_creek = ds_stem_tree_creek |> select(fire, stem_map_id, base_station_loc, date, tree_id = tree_ID, species, ht_lowest_ndl, ht_top, pct_prefire_green, pct_current_green, notes, center_id, distance, azimuth) |>
  mutate(stem_map_id = as.character(stem_map_id),
         date = as.Date(date, format = "%m/%d/%Y"), 
         ht_top = as.numeric(ht_top))

# Combine with the rest of the data
ds_stem_tree <- bind_rows(ds_stem_tree, ds_stem_tree_creek)

base_shifts <- read.csv(datadir("surveys/main/unprocessed/base_shifts/base_shifts.csv"))

### Emlid data
chips1 <- st_read(datadir("surveys/main/unprocessed/emlid/Chips-stemmap.geojson")) %>% mutate(fire = "Chips", stem_map_id = "1", base_loc = 1, year = 2020)
chips1_abco <- st_read(datadir("surveys/main/unprocessed/emlid/Chips-abco-stemmap.geojson")) %>% mutate(fire = "Chips", stem_map_id = "1_ABCO", base_loc = 1, year = 2020)
delta1 <- st_read(datadir("surveys/main/unprocessed/emlid/Delta-trees.geojson")) %>% mutate(fire = "Delta", stem_map_id = "1", base_loc = 1, year = 2020)
delta2 <- st_read(datadir("surveys/main/unprocessed/emlid/Delta_trees_2.geojson")) %>% mutate(fire = "Delta", stem_map_id = "2", base_loc = 2, year = 2020)
lassic1 <- st_read(datadir("surveys/main/unprocessed/emlid/Lasic-stem-1.geojson")) %>% mutate(fire = "Lassic", stem_map_id = "1", base_loc = 1, year = 2020)
lassic2 <- st_read(datadir("surveys/main/unprocessed/emlid/Lassic-stem-2.geojson")) %>% mutate(fire = "Lassic", stem_map_id = "2", base_loc = 1, year = 2020)
valley1 <- st_read(datadir("surveys/main/unprocessed/emlid/Valley-trees2.geojson")) %>% mutate(fire = "Valley", stem_map_id = "1", base_loc = 2, year = 2020)


emlid_2020 <- rbind(chips1, chips1_abco, delta1, delta2, lassic1, lassic2, valley1) %>%
  select(name, fire:year, collection.start, collection.end) %>%
  st_transform(3310) %>%
  st_zm()

lassic_2021 <- read_csv(datadir("surveys/main/unprocessed/emlid/Lassic stemmap 2 continued.csv")) %>% mutate(fire = "Lassic_3", stem_map_id = "2", base_loc = 3, year = 2021) |>
  mutate(Name = as.character(Name))
delta_2021 <- read_csv(datadir("surveys/main/unprocessed/emlid/Delta 4 (on phone its 3) Stem Map.csv")) %>% mutate(fire = "Delta_3", stem_map_id = "3", base_loc = 6, year = 2021)
creek1 <- read_csv(datadir("surveys/main/unprocessed/emlid2/Creek stem map centers.csv")) %>% mutate(fire = "Creek", stem_map_id = "1", base_loc = 1, year = 2021)
creek2 <- read_csv(datadir("surveys/main/unprocessed/emlid2/Creek Stem Map 2 Centers.csv")) %>% mutate(fire = "Creek", stem_map_id = "2", base_loc = 2, year = 2021)

lassic_delta = bind_rows(lassic_2021, delta_2021)
creek <- bind_rows(creek1, creek2)

emlid_2021 <- bind_rows(lassic_delta, creek)
emlid_2021 <- st_as_sf(emlid_2021, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  select(name = Name, fire:year, collection.start = "Averaging start", collection.end = "Averaging end") %>%
  st_transform(3310) %>%
  st_zm()

emlid <- rbind(emlid_2020, emlid_2021)


## fix some emlid typos
emlid[emlid$collection.start == "2020-07-15 23:08:47", "name"] <- 151 # first instance of 152 should be 151, not 152
emlid[emlid$collection.start == "2020-07-02 20:36:24", "name"] <- 59 # second instance of 58 should be 59
emlid[emlid$collection.start == "2020-07-02 21:00:37", "name"] <- 65 # first instance of 66 should be 65
emlid[emlid$collection.start == "2020-08-20 22:50:48", "name"] <- 34 # renamed a lassic 43 (the one that came bt 33 and 35) to 34


# remove an extraneous point
emlid <- emlid %>%
  filter(name != "Point 90") %>%
  filter(collection.start != "2020-07-02 20:23:17") %>% # remove the first delta 50
  filter(collection.start != "2020-07-02 22:57:59") # remove the first delta 99

## get the datasheet stem map name in the right character format
ds_stem_tree <- ds_stem_tree %>%
  mutate(stem_map_id = stem_map_id %>% as.character() %>% str_replace(fixed(".0"), ""))

## for delta 3, the stem map ID was 3, not 1
ds_stem_tree[ds_stem_tree$fire == "Delta_3", "stem_map_id"] <- "3"

# Prep emlid data, with date in right format
emlid <- emlid %>%
  select(date = collection.end, point_name = name, fire, stem_map_id, base_loc) %>%
  mutate(date = date - hours(1) - hours(7)) %>% # subtract an hour because st_read seems to add an hour
  mutate(date = force_tz(date, "UTC")) %>%
  mutate(date = str_sub(date, 1, 10))

# Clean emlid data
emlid <- emlid %>%
  mutate(point_name = str_replace(point_name, fixed("Point "), ""))


# Chips trees > 353 and < 1000 are in SM 2. (They were saved in the same Emlid project as SM1)
emlid <- emlid %>%
  mutate(tree_id_int = as.integer(point_name))
# the coercion warning here is OK; it's because of plot centerpoints that start with a letter, for which tree_id_int is irrelevant

emlid[which(emlid$fire == "Chips" & emlid$tree_id_int > 352 & emlid$tree_id_int < 1000), "stem_map_id"] <- 2

emlid <- emlid %>%
  select(-tree_id_int)


## Check for duplicates
emlid_nosp <- emlid
st_geometry(emlid_nosp) <- NULL
emlid_dup <- emlid_nosp %>%
  group_by(point_name, fire, stem_map_id) %>%
  summarize(n = n())


## Shift emlid trees and plot centers based on base offsets

# Pull in plot shifts
emlid <- left_join(emlid, base_shifts)

# Temporary inspection of base locs
# st_write(emlid,data("surveys/main/intermediate/temp_stemmap_emlid.geojson"), delete_dsn = TRUE)


## Get the geometries, to shift them (in m coord system)
emlid_geom <- st_geometry(emlid) %>% st_transform(3310)

## Do the shifts, for each fire & base loc
for (fire in unique(emlid$fire)) {
  emlid_fire <- emlid[emlid$fire == fire, ]
  for (base_loc in unique(emlid_fire$base_loc)) {
    rows_to_shift <- which(emlid$fire == fire & emlid$base_loc == base_loc)
    shifts <- base_shifts[base_shifts$fire == fire & base_shifts$base_loc == base_loc, ]
    emlid_geom[rows_to_shift] <- emlid_geom[rows_to_shift] + c(shifts$base_shift_x, shifts$base_shift_y)
  }
}

## Bring the shifted geom back in, remove shift columns
emlid <- st_set_geometry(emlid, emlid_geom %>% st_transform(4326)) %>%
  select(-base_shift_x, -base_shift_y)



## Separate out the emlid trees and emlid plot centers
emlid <- emlid %>%
  filter(!str_starts(point_name, "point")) # these were imported points to define the bounds of the Chips ABCO plot, not used for mapping

emlid_centers <- emlid %>%
  filter(str_starts(point_name, "C") | str_starts(point_name, "c")) %>%
  mutate(point_name = str_replace_all(point_name, fixed("-"), ""))

emlid_trees <- emlid %>%
  filter(!str_starts(point_name, "[a-zA-Z]")) %>%
  rename(tree_id = point_name)


## Rename an incorrectly named plot center
emlid_centers[emlid_centers$fire == "Delta" & emlid_centers$point_name == "C16", "point_name"] <- "C16A"



## Compute each manual tree's position relative to plot center, then add the plot center loc

manual_trees <- ds_stem_tree %>%
  filter(!is.na(center_id)) %>%
  mutate(center_id = toupper(center_id))

# For Creek trees, need to adjust for declination
manual_trees[manual_trees$fire == "Creek", "azimuth"] <- ((manual_trees[manual_trees$fire == "Creek", "azimuth"] + 13.5) %% 360)

manual_trees <- manual_trees %>%
  mutate(
    x_offset = sin(deg2rad(azimuth)) * distance,
    y_offset = cos(deg2rad(azimuth)) * distance
  )

## For each manual tree, get the x and y of the corresponding manual mapping plot center, as recorded by emlid
coords <- st_coordinates(emlid_centers %>% st_transform(3310)) %>% as.data.frame()
emlid_centers_foc <- emlid_centers %>%
  mutate(
    center_x = coords$X,
    center_y = coords$Y
  ) %>%
  select(
    center_id = point_name,
    fire, stem_map_id, center_x, center_y
  ) |>
  mutate(
    center_id = toupper(center_id)
  )

st_geometry(emlid_centers_foc) <- NULL

manual_trees = manual_trees |>
  mutate(center_id = toupper(center_id))


## Need to correct a few Chips centerpoints which are actually in Stem Map 2 and not 1

emlid_centers_foc[emlid_centers_foc$fire == "Chips" & emlid_centers_foc$center_id %in% c("C12A", "C16A"), "stem_map_id"] = "2"
emlid_centers_foc[emlid_centers_foc$fire == "Chips" & emlid_centers_foc$center_id %in% c("CABCOA"), "stem_map_id"] = "1_ABCO"

## Write the centers
centers = st_as_sf(emlid_centers_foc, coords = c("center_x", "center_y"), crs = 3310) %>%
  st_transform(4326)
coords = st_coordinates(centers)
centers$lon = coords[,1]
centers$lat = coords[,2]
st_geometry(centers) <- NULL
centers = centers |>
  mutate(fire = recode(fire, Delta_3 = "Delta")) |>
  mutate(stem_map_name = paste0(fire, "_", stem_map_id)) |>
  rename(subplot_center_id = center_id) |>
  select(stem_map_name, subplot_center_id, lon, lat) |>
  filter(stem_map_name != "Delta_1") # this one was removed because it could not be aligned with the other (gps-mapped) trees in this plot
write_csv(centers, datadir("surveys/main/processed/subplot_centers_v3.csv"))

manual_trees <- left_join(manual_trees, emlid_centers_foc, by = c("fire", "center_id", "stem_map_id"))

manual_trees_locs <- manual_trees %>%
  mutate(
    x = center_x + x_offset,
    y = center_y + y_offset
  ) %>%
  select(fire, stem_map_id, tree_id, x, y)

manual_trees_locs <- st_as_sf(manual_trees_locs, coords = c("x", "y"), crs = 3310) %>%
  st_transform(4326) %>%
  mutate(manual = TRUE)


### Append the manual and emlid tree locs
emlid_trees_locs <- emlid_trees %>%
  select(fire, stem_map_id, tree_id) %>%
  st_zm(drop = TRUE) %>%
  mutate(manual = FALSE)

tree_locs <- rbind(manual_trees_locs, emlid_trees_locs)

## get x and y
coords <- st_coordinates(tree_locs) %>% as.data.frame()
tree_locs$x <- coords$X
tree_locs$y <- coords$Y
st_geometry(tree_locs) <- NULL

tree_locs <- tree_locs %>%
  mutate(tree_id = as.integer(tree_id))


### Pull the coords into the tree data
trees <- left_join(ds_stem_tree, tree_locs, by = c("fire", "stem_map_id", "tree_id"))

### Make sure there are no duplicates
trees_dup <- trees %>%
  group_by(fire, stem_map_id, tree_id) %>%
  summarize(n = n())
# No duplicates!


## Make it spatial
trees_sf <- st_as_sf(trees, coords = c("x", "y"), crs = 4326)

## Write
write_csv(trees, datadir("surveys/main/processed/stems_v3.csv"))
st_write(trees_sf, datadir("surveys/main/processed/stems_v3.gpkg"), delete_dsn = TRUE)
