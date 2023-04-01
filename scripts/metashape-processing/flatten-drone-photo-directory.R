## Flatten the photos from a complex directory structure into a single directory, with the folder names in the filename instead

library(tidyverse)
library(here)

data_dir = readLines(here("data_dir.txt"), n=1)

in_photo_dir = file.path(data_dir, "str-disp_drone-data_imagery-missions/Emerald/120m")
out_photo_dir = file.path(data_dir, "str-disp_drone-data_imagery-missions/Emerald/flattened-120m")
prefix = "120m--"

filenames = list.files(in_photo_dir, recursive = TRUE)

filenames_flattened = str_replace_all(filenames, fixed("/"), "--")

filenames_flattened = paste0(prefix, filenames_flattened)

for(i in 1:length(filenames_flattened)) {
  
  filename = filenames[i]
  filename_flattened = filenames_flattened[i]
  
  file.copy( file.path(in_photo_dir,filename), file.path(out_photo_dir,filename_flattened))

}
