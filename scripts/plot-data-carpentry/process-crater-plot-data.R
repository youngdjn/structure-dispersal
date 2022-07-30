## Take Crater Fire data as provided and make a merged CSV and shapefile.
# Note the dirs here need to be updated

library(tidyverse)
library(readxl)
library(sf)
library(here)


# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


plot_coords = read_excel(datadir("surveys/crater/unprocessed/plot-surveys/CraterFire2001_IntensificationPlots.xlsx")) %>%
  select(-Notes)
plot_data = read_excel(datadir("surveys/crater/unprocessed/plot-surveys/Crater_Intensification_09032019.xlsx"))


d = left_join(plot_data,plot_coords,by=c("plot.no" = "Plot no."))

write.csv(d,datadir("surveys/crater/intermediate/crater_all.csv"))


# 12.61 = 0.05 ha
# 16.92 = 0.09 ha

#filter to high sev, supported trees before fire.
d_foc = d %>%
  mutate(basal.area.mortality = as.numeric(basal.area.mortality)) %>%
  filter(basal.area.mortality >= 75,
         no.dead.trees > 0) %>%
  mutate(plot_area_ha = ifelse(large.regen.plot == "YES",0.09,0.05)) %>%
  mutate(plot_area_ha = as.numeric(plot_area_ha),
         saplings = as.numeric(saplings)) %>%
  mutate(sapling_density_ha = saplings/plot_area_ha)

#note this dire needs to be updated
write.csv(d_foc,datadir("surveys/crater/intermediate/crater_foc.csv"))

# make it spatial
d_sp = st_as_sf(d_foc,coords=c("Easting","Northing"))
st_crs(d_sp) = 32611
st_write(d_sp,datadir("surveys/crater/intermediate/crater_foc.geojson"),delete_dsn=TRUE)
