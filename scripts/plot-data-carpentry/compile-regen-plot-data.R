library(tidyverse)
library(sf)
library(here)
library(readxl)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

#### Load data ####
ds_plot = read_excel(data("surveys/unprocessed/main/datasheets/dispersal-data-entry.xlsx"),sheet="plot")
ds_seedsource = read_excel(data("surveys/unprocessed/main/datasheets/dispersal-data-entry.xlsx"),sheet="seed_source")
kb1 = read_excel(data("surveys/unprocessed/main/kobo/plot_survey_-_all_versions_-_labels_-_2020-10-26-00-30-55.xlsx"))
kb2 = read_excel(data("surveys/unprocessed/main/kobo/plot_survey_v2_-_latest_version_-_labels_-_2020-10-26-00-34-21.xlsx"))

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


## Check: find the plots that don't have kobo data after 08-03

d_missing = d %>%
  filter(date > "2020-08-03" & is.na(kobo_plot_photo))


# Make a final data column that takes from Kobo if it exists and paper otherwise.
###!!!!Also make sure it doesn't have both


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
