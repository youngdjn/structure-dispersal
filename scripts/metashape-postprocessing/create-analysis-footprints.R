# Creates analysis footprints based on the extent of drone photos and field plots for each site

library(sf)
library(tidyverse)
library(here)

data_dir = readLines(here("data_dir.txt"), n=1)

internal_buffer_from_photos = 30 # in m. to make sure the edges of the focal area still have sufficient overlap


site = "lassic"



photo_points = st_read(file.path(data_dir, paste0("cross-site/drone-photo-points/120m/", site, ".gpkg"))) |> st_zm() |> st_transform(3310)

if(site != "crater") {
  plot_points = read_csv(file.path(data_dir, paste0("cross-site/regen-plots/plots.csv")))
  plot_points = st_as_sf(plot_points, coords = c("lon", "lat"), crs = 4326) |> st_transform(3310) |>
    filter(toupper(fire) == toupper(site))
} else if(site == "crater") {
  plot_points = read_csv(file.path(data_dir, "cross-site/regen-plots/crater_foc.csv"))
  plot_points = st_as_sf(plot_points, coords = c("Easting", "Northing"), crs = 32611) |> st_transform(3310) |>
    filter(toupper(fire) == toupper(site))
}

photo_boundary = photo_points |> st_buffer(300) |> st_union() |> st_buffer(-300) |> st_buffer(-30)
plot_boundary = plot_points |> st_buffer(300) |> st_union() |> st_convex_hull()

plot(photo_points)
plot(photo_boundary)
plot(plot_boundary, border = "red", add = TRUE)

focal_boundary = st_intersection(photo_boundary, plot_boundary)

plot(focal_boundary, border = "blue", add = TRUE)

file_write = file.path(data_dir, paste0("cross-site/boundaries/", site, ".gpkg"))

st_write(focal_boundary, file_write)
