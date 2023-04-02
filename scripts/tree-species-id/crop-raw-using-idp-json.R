### Take the image roi dict JSON computed by EasyIDP and crop the raw images to the individual trees

library(here)
library(tidyverse)
library(rjson)
library(purrr)
library(magick)
library(data.table)
library(furrr)

data_dir = readLines(here("data_dir.txt"), n=1)

dict_file = file.path(data_dir, "emerald/tao-img-dict.json")
img_base_path_in = file.path(data_dir, "str-disp_drone-data_imagery-missions/Emerald/flattened-120m")
img_base_path_out = file.path(data_dir, "emerald/cropped-tree-images")
  
dict = fromJSON(file = dict_file)

# get the list of ROI names (tree IDs) so we can make folders for the cropped images, one folder for each ROI
roi_names = map(dict, names) |> unlist() |> unique()


## create the directories
out_dirs = file.path(img_base_path_out, roi_names)
for(out_dir in out_dirs) {
  if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
}


image_names = names(dict)
image_names = file.path(img_base_path_in, image_names)


## For each image, crop to the ROIs and save into folders
crop_one_img_for_multiple_trees = function(img_name, img_rois) {
  
  roi_names = names(img_rois)
  
  # read image
  img <- image_read(img_name)
  
  for(j in seq_along(roi_names)) {
    
    roi_name = roi_names[j]
    
    img_basename = basename(img_name)
    img_basename_out = paste0("tree_", roi_name, "--", img_basename)
    img_path_out = file.path(img_base_path_out, roi_name, img_basename_out)
  
    if(file.exists(img_path_out)) next()
    
    img_roi = img_rois[[j]]
  
    if(is.null(img_roi)) {
      next()
    }
    
    cat("Image", i, "of", length(image_names), ": ROI", j, "of", length(roi_names), "\n")
  
    img_roi = img_roi |> simplify2array() |> t()
    
    xmin = min(img_roi[,1]) |> round()
    xmax = max(img_roi[,1]) |> round()
    ymin = min(img_roi[,2]) |> round()
    ymax = max(img_roi[,2]) |> round()
    
    # cropping params in format needed for magick
    width = xmax - xmin
    height = ymax - ymin
    
    # crop image
    crop_string = paste0(width, "x", height, "+", xmin, "+", ymin)
    img_crop = image_crop(img, crop_string)
    
    # write cropped image
    image_write(img_crop, img_path_out)
    
    gc()
  }
}

plan(multisession)

future_walk2(image_names, dict, crop_one_img_for_multiple_trees)
