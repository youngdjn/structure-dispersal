# Take the seedling plot (field survey) and overstory tree (processed drone) data and produce data in the appropriate format for feeding to a Stan model. Prepped data saved in {datadir}/prepped-for-stan

library(here)
data_dir = readLines(here("data_dir.txt"), n=1)
data_dir = file.path(data_dir, "cross-site/")

## Convenience functions ####
source(here("scripts/convenience_functions.R"))
# ^ This defines the function 'datadir', which takes a string argument and prepends it with the path to the data directory.
#   It allows you to make all the paths in the script be relative to the data directory.


## Main functions for the tasks of this script
source(here("scripts/dispersal-modeling/dispersal-kernel-modeling/01_prep-data-for-model_functions.R"))


#### Run data prep for a specific site. See 01_prep-data-for-model_functions.R for parameter definitions.
prep_data(dataset_name = "crater-pipj-height-01",
          overstory_tree_filepath = "ttops-live/crater.gpkg",
          seedling_plot_filepath = "regen-plots-standardized/crater.gpkg",
          target_crs = 32611,
          seedling_plot_area = 900,
          #TODO: specify which species
          size_function_name = "height")


#### Run data prep for a specific site. See 01_prep-data-for-model_functions.R for parameter definitions.
prep_data(dataset_name = "valley-allsp-height-01",
          overstory_tree_filepath = "ttops-live/valley.gpkg",
          seedling_plot_filepath = "regen-plots-standardized/valley.gpkg",
          target_crs = 3310,
          seedling_plot_area = 201,
          #TODO: specify which species
          size_function_name = "height")

#### Run data prep for a specific site. See 01_prep-data-for-model_functions.R for parameter definitions.
prep_data(dataset_name = "delta-allsp-height-01",
          overstory_tree_filepath = "ttops-live/delta.gpkg",
          seedling_plot_filepath = "regen-plots-standardized/delta.gpkg",
          target_crs = 3310,
          seedling_plot_area = 201,
          #TODO: specify which species
          size_function_name = "height")

#### Run data prep for a specific site. See 01_prep-data-for-model_functions.R for parameter definitions.
prep_data(dataset_name = "chips-allsp-height-01",
          overstory_tree_filepath = "ttops-live/chips.gpkg",
          seedling_plot_filepath = "regen-plots-standardized/chips.gpkg",
          target_crs = 3310,
          seedling_plot_area = 113,
          #TODO: specify which species
          size_function_name = "height")
