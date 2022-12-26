# Take the seedling plot (field survey) and overstory tree (processed drone) data and produce data in the appropriate format for feeding to a Stan model. Prepped data saved in {datadir}/prepped-for-stan

library(here)
data_dir = readLines(here("data_dir.txt"), n=1)

## Convenience functions ####
source(here("scripts/convenience_functions.R"))
# ^ This defines the function 'datadir', which takes a string argument and prepends it with the path to the data directory.
#   It allows you to make all the paths in the script be relative to the data directory.


## Main functions for the tasks of this script
source(here("scripts/dispersal-modeling/dispersal-kernel-modeling/01_prep-data-for-model_functions.R"))


#### Run data prep for a specific site. See 01_prep-data-for-model_functions.R for parameter definitions.
prep_data(dataset_name = "crater-pipj-height-01",
          overstory_tree_filepath = "drone-products-prepped/crater/Crater120m_20210122T1219_ttops_vwf196.gpkg",
          seedling_plot_filepath = "surveys/crater/intermediate/crater_foc.geojson",
          target_crs = 32611,
          seedling_plot_area = 900,
          #TODO: specify which species
          size_function_name = "height")
