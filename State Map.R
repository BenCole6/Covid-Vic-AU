library(pacman)
p_load(tidyverse,
       ggplot2,
       ggmap,
       rgdal)


zipped_files <- list.files(path = getwd())

zipped_files <- zipped_files[which(str_ends(zipped_files, ".zip"))]

dir.create(path = "Unzipped GIS Files")

for(zipped_folder in zipped_files) {
  
  unzip(zipfile = zipped_folder,
        exdir = file.path("Unzipped GIS Files",
                          str_remove(zipped_folder, ".zip")))
  
}

shape_files <- c()

shape_folders <- c()

unzipped_files <- list.files(file.path(getwd(),
                                       "Unzipped GIS Files"))

list.files(file.path(getwd(),
                     "Unzipped GIS Files",
                     unzipped_files[1],
                     "ll_gda94", "shape",
                     "lga_polygon"))

for(folder in unzipped_files) {
  
  shape_folders <- c(shape_folders,
                     list.files(file.path(getwd(),
                                          "Unzipped GIS Files",
                                          folder,
                                          "ll_gda94", "shape",
                                          "lga_polygon")))
  
}

lga_names <- str_remove_all(shape_folders,
                                " \\(uninc\\)|-\\d\\d\\d\\d")


for(folder in unzipped_files) {
  
  poly_files <- list.files(file.path(getwd(),
                                     "Unzipped GIS Files",
                                     folder,
                                     "ll_gda94", "shape",
                                     "lga_polygon"))
  
  shape_folders <- c(shape_folders,
                     poly_files)
  
  for(lga in poly_files) {
    
    lga_name <- str_remove_all(lga,
                               " \\(uninc\\)|-\\d\\d\\d\\d")
    
    parent_dir <- file.path(getwd(),
                            "Unzipped GIS Files",
                            folder,
                            "ll_gda94", "shape",
                            "lga_polygon",
                            lga)
    
    temp <- readOGR(file.path(parent_dir, "EXTRACT_POLYGON.shp"))
    
    assign(x = paste0(lga_name, "_shpfile"),
           value = temp)
    
  }
      
}

shp_objects <- ls()[which(str_ends(ls(),"_shpfile"))]

combined_lga_df <- data.frame()

for(shp_obj in shp_objects) {

  lga <- str_remove_all(shp_obj,
                        "_shpfile")
  
  temp_df <- cbind(fortify(get(shp_obj)),
                   "LGA" = lga)
  
  combined_lga_df <- rbind(combined_lga_df,
                           temp_df)

}

# ggplot(group_by(combined_lga_df,
#                 LGA),
#        aes(x = long, y = lat,
#            group = LGA)) +
#   geom_polygon(line = 1,
#                colour = "black",
#                fill = NA) +
#   coord_equal() +
#   theme_minimal()
  
