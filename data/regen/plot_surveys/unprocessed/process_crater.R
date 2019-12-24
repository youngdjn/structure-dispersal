## Take Crater Fire data as provided and make a merged CSV and shapefile.

library(tidyverse)
library(readxl)
library(sf)

plot_coords = read_excel("CraterFire2001_IntensificationPlots.xlsx") %>%
  select(-Notes)
plot_data = read_excel("Crater_Intensification_09032019.xlsx")

d = left_join(plot_data,plot_coords,by=c("plot.no" = "Plot no."))

write.csv(d,"../../../data/plot_surveys/regen/crater_all.csv")


# 12.61 = 0.05 ha
# 16.92 = 0.09 ha


d_foc = d %>%
  mutate(basal.area.mortality = as.numeric(basal.area.mortality)) %>%
  filter(basal.area.mortality >= 75,
         no.dead.trees > 0) %>%
  mutate(plot_area_ha = ifelse(large.regen.plot == "YES",0.05,0.09)) %>%
  mutate(plot_area_ha = as.numeric(plot_area_ha),
         saplings = as.numeric(saplings)) %>%
  mutate(sapling_density_ha = saplings/plot_area_ha)

write.csv(d_foc,"../../../data/plot_surveys/regen/crater_foc.csv")

# make it spatial
d_sp = st_as_sf(d_foc,coords=c("Easting","Northing"))
st_crs(d_sp) = 32611
st_write(d_sp,"../../../data/plot_surveys/regen/crater_foc.geojson",delete_dsn=TRUE)
