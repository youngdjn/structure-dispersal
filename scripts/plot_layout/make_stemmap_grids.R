library(sf)

point = st_read("/home/derek/Documents/data/str-disp_data/regen/plot_layout/boggs/boggs_stemmap_center.gpkg")

points_albers = st_transform(point,3310)


allcorners = NA

for(i in 1:nrow(point)) {
  
  point = points_albers[i,]
  
  # make a circle with 60.01 m radius, then the square that encompases it
  circle = st_buffer(point,60)
  square = st_make_grid(circle,cellsize = 121.0,what = "polygons")
  
  corners = st_make_grid(square,cellsize = 30,what = "corners")
  centers = st_make_grid(square,cellsize = 30,what = "centers")
  
  if(is.na(allcorners)) {
    allcorners = corners
    allcenters = centers
  } else {
    allcorners = c(allcorners,corners)
    allcenters = c(allcenters,centers)
  }
}

allcorners = st_as_sf(allcorners)
allcenters = st_as_sf(allcenters)

allcorners$id = allcorners$name = paste0("g",1:nrow(allcorners))
allcenters$id = allcenters$name = paste0("c",1:nrow(allcenters))

allpoints = rbind(allcorners,allcenters)

# add z dimension
allpoints = st_zm(allpoints, drop=FALSE, what = "Z")
allpoints = allpoints %>% st_transform(4326)

st_write(allpoints,"/home/derek/Documents/data/str-disp_data/regen/plot_layout/forfield/boggs/boggs_stemmap_grid.kml",delete_dsn = TRUE)
st_write(allpoints,"/home/derek/Documents/data/str-disp_data/regen/plot_layout/forfield/boggs/boggs_stemmap_grid.geojson",delete_dsn = TRUE)

