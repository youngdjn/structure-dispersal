### Take the seedling plot (field survey) and overstory tree (processed drone) data and produce data in the appropriate format for feeding to a Bayesian model using Stan, or a ML model using an optimizer.

# Data directory
library(here)
if (grep("latimer", here()) == 1) data_dir = readLines(here("data_dir_andrew.txt"), n = 1) else data_dir = readLines(here("data_dir.txt"), n = 1)

## Main functions for the tasks of this script
source(here("scripts/dispersal-modeling/01_prep-data-for-model_by-species_functions.R"))

prep_data_allspecies(data_dir = data_dir,
  site_name = "chips",
  overstory_tree_filepath = file.path("predicted-treecrowns-w-predicted-species/chips.geojson"),
  seedling_plot_filepath = file.path("regen-plots-standardized/chips.gpkg"),
  prepped_data_filepath = file.path("prepped-for-stan/max750"),
  target_crs = 3310,
  seedling_plot_area = 201, 
  tree_distance_cutoff = 750
)


prep_data_onespecies(data_dir = data_dir,
    site_name = "delta", # e.g. "delta"
    focal_species = "FIRS", # 4-letter code
    overstory_tree_filepath = file.path("predicted-treecrowns-w-predicted-species/delta.geojson"),
    seedling_plot_filepath = file.path("regen-plots-standardized/delta.gpkg"),
    prepped_data_filepath = file.path("prepped-for-stan/maxdist750"),
    target_crs = 3310, # target CRS (to project the raw data sources to)
    seedling_plot_area = 201, # area of the plot in sq m
    tree_distance_cutoff = 750
) 


test = get_dispdata(data_dir = data_dir,
                     site_name = "delta", # e.g. "delta"
                     focal_species = "FIRS", # 4-letter code
                     overstory_tree_filepath = file.path("predicted-treecrowns-w-predicted-species/delta.geojson"),
                     seedling_plot_filepath = file.path("regen-plots-standardized/delta.gpkg"),
                     target_crs = 3310, # target CRS (to project the raw data sources to)
                     seedling_plot_area = 201, # area of the plot in sq m
                     min_tree_height = 10, # min tree height to include in m
                     density_raster_resolution = 15, 
                     tree_distance_cutoff = 500
) 


names(dispdata)
length(dispdata$elev_diff_vector)
length(dispdata$dist_vector)
length(dispdata$tree_density_vector)
