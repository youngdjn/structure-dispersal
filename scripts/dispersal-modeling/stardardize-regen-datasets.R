# Put the Crater and other regen datasets in standardized format

library(tidyverse)
library(here)
library(sf)

data_dir = readLines(here("data_dir.txt"), n=1)

# Crater

d = read_csv(file.path(data_dir, "cross-site/regen-plots/crater_foc.csv"))

d_sp = st_as_sf(d, coords = c("Easting", "Northing"), crs = 26911)

d_sp$observed_count = round(d_sp$sapling_density_ha * 0.09) # plots are 0.09 ha

d_sp = d_sp |>
  select(observed_count,
         BurnClass)

st_write(d_sp, file.path(data_dir, "cross-site/regen-plots-standardized/crater.gpkg"), delete_dsn = TRUE)




### Other datasets: general prep

seedl = read_csv(file.path(data_dir, "cross-site/regen-plots/species.csv"))

seedl_agg = seedl |>
  select(fire, plot_id, species_coarse,
          observed_count = seedl_analyze) |>
  group_by(fire, plot_id) |>
  summarize(observed_count = sum(observed_count, na.rm = TRUE))


## Delta

d = read_csv(file.path(data_dir, "cross-site/regen-plots/plots.csv")) |>
  filter(toupper(fire) == toupper("delta")) |>
  select(fire, plot_id, lon, lat)

# pull in seedl count
d = inner_join(d, seedl_agg, by = c("fire", "plot_id")) # TODO: figure out why two plots don't have cooresponding seedling data, so we don't lose them with the inner_join

# make spatial
d_sp = st_as_sf(d, coords = c("lon", "lat"), crs = 4326)

# write
st_write(d_sp, file.path(data_dir, "cross-site/regen-plots-standardized/delta.gpkg"), delete_dsn = TRUE)



## Valley

d = read_csv(file.path(data_dir, "cross-site/regen-plots/plots.csv")) |>
  filter(toupper(fire) == toupper("valley")) |>
  select(fire, plot_id, lon, lat)

# pull in seedl count
d = inner_join(d, seedl_agg, by = c("fire", "plot_id"))

# make spatial
d_sp = st_as_sf(d, coords = c("lon", "lat"), crs = 4326)

# write
st_write(d_sp, file.path(data_dir, "cross-site/regen-plots-standardized/valley.gpkg"), delete_dsn = TRUE)



## Chips

d = read_csv(file.path(data_dir, "cross-site/regen-plots/plots.csv")) |>
  filter(toupper(fire) == toupper("chips")) |>
  select(fire, plot_id, lon, lat)

# pull in seedl count
d = inner_join(d, seedl_agg, by = c("fire", "plot_id"))

# make spatial
d_sp = st_as_sf(d, coords = c("lon", "lat"), crs = 4326)

# write
st_write(d_sp, file.path(data_dir, "cross-site/regen-plots-standardized/chips.gpkg"), delete_dsn = TRUE)
