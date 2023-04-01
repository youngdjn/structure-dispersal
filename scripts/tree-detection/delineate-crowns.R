## Takes ttops and a CHM and makes a map of tree crowns (TAOs)
## Also saves a version with a tiny donut hole cut out of the top of each so IDP can include the treetop in the crop

library(sf)
library(terra)
library(here)
library(tidyverse)
library(lidR)
library(nngeo)
library(smoothr)

data_dir = readLines(here("data_dir.txt"), n=1)


chm_file = "chms/DeltaB-120m_20230310T1701_chm.tif"
treetop_file = "ttops/DeltaB-120m_20230310T1701_ttops.gpkg"
tao_out_file = "taos/DeltaB-120m_20230310T1701_taos.gpkg"
talltao_out_file = "taos/DeltaB-120m_20230310T1701_talltaos.gpkg"
idptaos_out_file =  "taos/DeltaB-120m_20230310T1701_idptaos.shp"
singletao_out_file =  "taos/DeltaB-120m_20230310T1701_singletao.gpkg"

chm = rast(file.path(data_dir, chm_file),)
ttops = st_read(file.path(data_dir, treetop_file))
st_crs(ttops) = 26910

mask_poly = st_buffer(ttops, 30) |> st_union()
chm = crop(chm, vect(mask_poly))
  
taos = silva2016(chm, ttops, max_cr_factor = 0.24, exclusion = 0.1)()

taos <- as.polygons(taos)
taos <- st_as_sf(taos)
taos <- st_cast(taos, "MULTIPOLYGON")
taos <- st_cast(taos, "POLYGON")
taos <- st_remove_holes(taos)
taos <- st_make_valid(taos)
taos <- smooth(taos, method = "ksmooth", smoothness = 3)
taos <- st_simplify(taos, preserveTopology = TRUE, dTolerance = 0.1)

# assign TAOs the treetop height and remove those that have no treetops in them
taos = st_join(taos, ttops)
taos = taos[,c(2,3)]
taos = taos[!is.na(taos$Z),]

st_write(taos, file.path(data_dir, tao_out_file), delete_dsn = TRUE)

# save a set of only TAOs > 10 m

talltaos = taos |>
  filter(Z > 10)

st_write(talltaos, file.path(data_dir, talltao_out_file), delete_dsn = TRUE)

# save a set of TAOs > 10 m with a tiny donut hole cut out of the top for IDP

# A helper function that erases all of y from x:
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

cutter = st_buffer(ttops, 0.2) # the shape to cut out of each TAO
idptaos = st_erase(talltaos, cutter)
idptaos <- st_simplify(idptaos, preserveTopology = TRUE, dTolerance = 0.1)

st_write(idptaos, file.path(data_dir, idptaos_out_file), delete_dsn = TRUE)

singletao = idptaos[100,]
singletao = st_remove_holes(singletao)

st_write(singletao, file.path(data_dir, singletao_out_file), delete_dsn = TRUE)

