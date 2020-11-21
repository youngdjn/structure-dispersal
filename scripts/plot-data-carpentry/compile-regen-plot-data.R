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
ds_plot = read_excel(data("surveys/main/unprocessed/datasheets/dispersal-data-entry.xlsx"),sheet="plot")
ds_seedsource = read_excel(data("surveys/main/unprocessed/datasheets/dispersal-data-entry.xlsx"),sheet="seed_source")
ds_basestation = read_excel(data("surveys/main/unprocessed/datasheets/dispersal-data-entry.xlsx"),sheet="base_station")
kb1 = read_excel(data("surveys/main/unprocessed/kobo/plot_survey_-_all_versions_-_labels_-_2020-10-26-00-30-55.xlsx"))
kb2 = read_excel(data("surveys/main/unprocessed/kobo/plot_survey_v2_-_latest_version_-_labels_-_2020-10-26-00-34-21.xlsx"))

base_shifts = read.csv(data("surveys/main/unprocessed/base_shifts/base_shifts.csv"))

### Emlid data
delta1 = st_read(data("surveys/main/unprocessed/emlid/Delta_regen.geojson"))
delta2 = st_read(data("surveys/main/unprocessed/emlid/Delta-regen2.geojson"))
delta3 = st_read(data("surveys/main/unprocessed/emlid/Delta_regen3.geojson"))

em_delta = rbind(delta1,delta2,delta3) %>%
  mutate(fire = "Delta")

valley1 = st_read(data("surveys/main/unprocessed/emlid/Valley_regen.geojson"))
valley2 = st_read(data("surveys/main/unprocessed/emlid/Valley_regen2.geojson"))

em_valley = rbind(valley1,valley2) %>%
  mutate(fire = "Valley")

em_chips = st_read(data("surveys/main/unprocessed/emlid/Chips-regen.geojson")) %>%
  mutate(fire = "Chips")

lassic1 = st_read(data("surveys/main/unprocessed/emlid/Lassic_regen.geojson"))
lassic2 = st_read(data("surveys/main/unprocessed/emlid/Lassic_regen_2.geojson"))

em_lassic = rbind(lassic1,lassic2) %>%
  mutate(fire = "Lassic")

em_eiler = st_read(data("surveys/main/unprocessed/emlid/Eiler_regen.geojson")) %>%
  mutate(fire = "Eiler")

emlid = rbind(em_delta,em_valley,em_chips,em_lassic,em_eiler) %>%
  rename(plot_id = name) %>%
  mutate(plot_id = as.character(plot_id))

### Planned plot layouts
delta1 = st_read(data("surveys/main/unprocessed/crew-plot-points/delta_plots_01.kml")) %>%
  mutate(fire = "Delta") %>% select(fire, plot_id = Name)
delta2 = st_read(data("surveys/main/unprocessed/crew-plot-points/delta_plots_02.kml")) %>%
  mutate(fire = "Delta") %>% select(fire, plot_id = Name)
valley = st_read(data("surveys/main/unprocessed/crew-plot-points/boggs_plots_01.kml")) %>%
  mutate(fire = "Valley") %>% select(fire, plot_id = Name)
chips = st_read(data("surveys/main/unprocessed/crew-plot-points/chips_plots_01.kml")) %>%
  mutate(fire = "Chips") %>% select(fire, plot_id = Name)
lassic = st_read(data("surveys/main/unprocessed/crew-plot-points/lassic_plots_01.kml")) %>%
  mutate(fire = "Lassic") %>% select(fire, plot_id = Name)
eiler = st_read(data("surveys/main/unprocessed/crew-plot-points/eiler_plots_01.kml")) %>%
  mutate(fire = "Eiler") %>% select(fire, plot_id = Name)

plot_layouts = rbind(delta1,delta2,valley,chips,lassic,eiler) %>%
  mutate(plot_id = as.character(plot_id))


#### Clean Kobo ####
## Column name dashes to underscores
names(kb1) = names(kb1) %>% str_replace_all(fixed("-"),"_")
names(kb2) = names(kb2) %>% str_replace_all(fixed("-"),"_")

# Combine both Kobos
kb = bind_rows(kb1,kb2)

# Remove testing plots
kb = kb %>%
  filter(!(Plot_ID %in% c("9999","9998")))

# Remove unused columns
kb = kb %>%
  select(-pine_needles,-fir_needles,-other_needles,-soil_litter_cover,
         -start,
         -end,
         -username,
         -deviceid,
         -GPS_point,
        -"_GPS_point_altitude",
        -"_GPS_point_precision",
        -"__version__",
        -"_id",
        -"_uuid",
        -"_submission_time",
        -"_validation_status",
        -"_index")

# Add a column for seed source beyond (which is always false because Kobo had no > x seed sources)
kb = kb %>%
  mutate(seed_source_any_beyond = FALSE)

# Rename columns for compatibility across data sources
kb = kb %>%
  rename(plot_id = Plot_ID,
         date = today)

# Rename columns that also occur in paper datasheets
cols_to_rename = c("non_growing_cover",
                   "shrub_cover",
                   "shrub_cover_prostrate",
                   "shrub_height_modal",
                   "shrub_species",
                   "herb_cover",
                   "herb_height_modal",
                   "herb_dominant_funct_grp",
                   "seed_source_any",
                   "seed_source_any_beyond",
                   "canopy_fisheye_photo",
                   "canopy_normal_photo",
                   "needle_photo",
                   "plot_photo")
kb = kb %>%
  rename_with(df, .fn = ~paste0("kobo_",.), .cols = all_of(cols_to_rename) )


## 2020-08-05 Kobo plot 3031 is really 3131
kb[kb$plot_id == "3031" & kb$date=="2020-08-05","plot_id"] = "3131"
kb[kb$plot_id == "3506" & kb$date=="2020-08-06","plot_id"] = "3056"



# ## Pull in fire name based on date
# kb = kb %>%
#   mutate(fire = ifelse(date < "2020-08-10","Chips",date),
#          fire = ifelse(date < "2020-09-07" & date > "2020-09-02","Eiler",date),
#          fire = ifelse(date < "2020-09-04" & date > "")) # Lassic 8/18 to 9/2


#### Clean paper datasheets ####

## Clean seed source
ds_seedsource = ds_seedsource %>%
  mutate(species = toupper(species)) %>%
  mutate(beyond = grepl(">",distance)) %>%
  mutate(distance = str_replace(distance,">","") %>% as.numeric)



## Plot column name dashes to underscores
names(ds_plot) = names(ds_plot) %>% str_replace_all(fixed("-"),"_")


### Compute seed source any from species-specific seed sources, and pull it in



ds_seedsource_any_computed = ds_seedsource %>%
  # get all the records that are the minimum
  group_by(fire,plot_id) %>%
  filter(distance == min(distance,na.rm=TRUE)) %>%
  # get the minium distance by plot, and whether all of them were beyond
  summarize(ds_seed_source_any = mean(distance), # mean not necessary because they should all be the same
            ds_seed_source_any_beyond = all(beyond)) 

ds_plot = left_join(ds_plot, ds_seedsource_any_computed)



## Rename columns that also occur in kobo
cols_to_rename = c("non_growing_cover",
                   "shrub_cover",
                   "shrub_cover_prostrate",
                   "shrub_height_modal",
                   "shrub_species",
                   "herb_cover",
                   "herb_height_modal",
                   "herb_dominant_funct_grp",
                   "plot_photo",
                   "canopy_fisheye_photo",
                   "canopy_normal_photo",
                   "needle_photo")

ds_plot = ds_plot %>%
  rename_with(df, .fn = ~paste0("ds_",.), .cols = all_of(cols_to_rename) )

ds_plot = ds_plot %>%
  mutate(plot_id = as.character(plot_id),
         date = as.character(date))


#### Merge paper datasheets and kobo

d = full_join(ds_plot,kb)

## Need to drop plots with two kobo records where it is impossible to tell which one is a typo
d = d %>%
  filter(!(fire == "Chips" & plot_id %in% c("5004","5005")))

## Need to drop the plots that have only kobo records and no paper records
d = d %>%
  filter(!is.na(fire))


#### Checking ####

# find the plots that don't have kobo data after 08-03 (no kobo plot and date to match paper plot and date)
d_missing = d %>%
  filter(date > "2020-08-03" & is.na(kobo_plot_photo))

# Look for paper datasheet plots that exist twice: same date and plot ID or same fire and plot ID
ds_plot_count = ds_plot %>%
  group_by(plot_id,fire) %>%
  summarize(n = n())

# Find plots that have both paper and kobo data (for shrub cover)
d_double = d %>%
  mutate(both = !is.na(ds_shrub_cover) & !is.na(kobo_shrub_cover)) %>%
  filter(both)
# (there are none)


#### Combine the paper and kobo columns into a final definitive column

cols = c("plot_photo","canopy_fisheye_photo","canopy_normal_photo","needle_photo","non_growing_cover","shrub_cover","shrub_cover_prostrate",  
  "herb_cover","shrub_height_modal","herb_height_modal","shrub_species",          
  "herb_dominant_funct_grp","seed_source_any_beyond","seed_source_any")

kb_cols = paste0("kobo_",cols)
ds_cols = paste0("ds_",cols)


for(i in 1:length(cols)) {
  
  col = cols[i]
  kb_col = kb_cols[i]
  ds_col = ds_cols[i]
  
  d = d %>%
    mutate((!!col) := ifelse(!is.na(!!sym(kb_col)), !!sym(kb_col), !!sym(ds_col))) %>%
    select(-!!sym(kb_col),-!!sym(ds_col))
}


### Set all plot radius to 8 m except Chips to 6.

d = d %>%
  mutate(plot_area = ifelse(fire == "Chips",3.14159*6^2, 3.14159*8^2))






  

#### Geospatial ####

### Make sure that each emlid plot id is next to its intended plot id

a = st_distance(emlid,plot_layouts)

## for each row get the column index of the element that was the closest
min_index = function(x) {
  return(which(x == min(x))[1])
}
closest_index = apply(a,1,min_index)
closest_plot_id = plot_layouts[closest_index,]$plot_id

emlid$closest_planned_plot = closest_plot_id
emlid = emlid %>%
  mutate(not_near_planned_plot = plot_id != closest_planned_plot)

## Check for Emlid duplicates

emlid_dup = emlid %>%
  group_by(fire,plot_id) %>%
  summarize(n = n()) %>%
  filter(n > 1)

## List plots not near, and duplicates
emlid_dup$plot_id

emlid[emlid$not_near_planned_plot,]$plot_id

## Write both plot layers for comparison
st_write(emlid,data("surveys/main/intermediate/emlid_compiled_uncleaned.geojson"),delete_dsn=TRUE)
st_write(plot_layouts,data("surveys/main/intermediate/crew_plot_layouts.geojson"),delete_dsn=TRUE)

## Correct the plots that were marked in the wrong spot
emlid[emlid$plot_id == "5033" & emlid$not_near_planned_plot,"plot_id"] = 5035
emlid[emlid$plot_id == "1027" & emlid$not_near_planned_plot,"plot_id"] = 2027
emlid[emlid$plot_id == "1063" & emlid$not_near_planned_plot,"plot_id"] = 2063
emlid[emlid$plot_id == "1053" & emlid$not_near_planned_plot,"plot_id"] = 3053
emlid[emlid$plot_id == "3048" & emlid$not_near_planned_plot,"plot_id"] = 2048

## Remove a plot that was taken twice
emlid = emlid %>%
  filter(collection.end != "2020-07-23 19:43:29")

### Shift the plot locs as specified for each base and day. For this, must pull in base loc for each day.

# Prep emlid data, with date in right format to pull in
emlid = emlid %>%
  select(date = collection.end, plot_id, fire) %>%
  mutate(date = date - hours(1) - hours(7)) %>% # subtract an hour because st_read seems to add an hour
  mutate(date = force_tz(date,"UTC")) %>%
  mutate(date = str_sub(date,1,10))



## Pull in date
ds_basestation_foc = ds_basestation %>%
  select(fire,
         date,
         base_loc = loc_number) %>%
         #base_lat = lat,
         #base_lon = long) %>%
  mutate(date = as.character(date))

emlid = left_join(emlid,ds_basestation_foc %>% filter (fire != "Valley")) # exclude Valley because there are two days with 2 base station locs

# manually set the base loc for Valley to 1 for all plots (assuming the log saying the last day was on Base 2 is a mistake because it was all plots, interspersed with plots that were on Base 1)
emlid[emlid$fire == "Valley","base_loc"] = 1


### Shift the base locs by the specified amount: this actually shifts the plot points based on which base they were from

## Pull in the shift amounts
emlid = left_join(emlid,base_shifts)

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


## Bring the shifted geom back in
emlid = st_set_geometry(emlid,emlid_geom %>% st_transform(4326))

## Get coords to pull in to plot data
coords = st_coordinates(emlid) %>% as.data.frame
emlid$lat = coords$Y
emlid$lon = coords$X

## Pull them in , first selecting only the relevant Emlid cols
emlid_foc = emlid %>%
  select(plot_id,fire,lat,lon)
st_geometry(emlid_foc) = NULL

d = left_join(d,emlid_foc)


## Find plots missing emlid points
d_missing = d %>%
  select(fire,plot_id,date,lat,lon,notes) %>%
  filter(is.na(lon)) %>%
  mutate(fire_plot = paste(fire,plot_id,sep="_"))

## Get the planned plot locs for the plots that are missing Emlid locs
plot_layouts_for_missing = plot_layouts %>%
  mutate(fire_plot = paste(fire,plot_id,sep="_")) %>%
  filter(fire_plot %in% d_missing$fire_plot)

missing_geom = st_geometry(plot_layouts_for_missing) %>% st_transform(3310)

# Shift Valley 1065 10 m N to accommodate for shifting done in field
plot_index = which(plot_layouts_for_missing$fire == "Valley" & plot_layouts_for_missing$plot_id == 1065)
missing_geom[plot_index] = missing_geom[plot_index] + c(0,10)
plot_layouts_for_missing = st_set_geometry(plot_layouts_for_missing, missing_geom %>% st_transform(4326))

coords = st_coordinates(plot_layouts_for_missing) %>% as.data.frame()

st_geometry(plot_layouts_for_missing) = NULL

## Pull in this data
plot_layouts_for_missing = plot_layouts_for_missing %>%
  mutate(lat_approx = coords$Y,
         lon_approx = coords$X) %>%
  select(fire, plot_id, lat_approx, lon_approx)

d = left_join(d,plot_layouts_for_missing)

d = d %>%
  mutate(coords_approx = is.na(lat),
         lat = ifelse(is.na(lat),lat_approx,lat),
         lon = ifelse(is.na(lon),lon_approx, lon)) %>%
  select(-lat_approx,-lon_approx)


## Pull in emlid plot loc data
## Pull in planned plot loc data
## Prepare a plot loc offset file to use when using planned plot loc based on comments about plot shifted (may need to check Valley datasheets for that)

## Make sure there is good matching plot - emlid, and each field plot has a planned loc.

## Make sure the emlid loc is close to the planned loc
## Make sure the kobo loc is close to the emlid loc





##!!!! for species, make sure that not more than one record per plot per species (or it means the plot id was entered wrong)

## export species-specific counts and seed trees

## add plot size to data table (just based on fire name)


#### For stem maps:
# Need a file for each center's offset
# Need a file for each base location's offset: allow it to add to another base's offset. For these offset bases, compute the offset manually.
# First correct base location, then centers
# Map out the emlid and manual trees (manual based on trig)

# Shapefile of trees
# Shapefile of plots

# Can we post-process base location using the emlid base logs?
