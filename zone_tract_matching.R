

setwd("~")
setwd("Downloads")


# Get shapefile of redlining by Durham zones 
durham_redline_shp <- rgdal::readOGR("durhamredliningzones", "durhamredliningzones")
# Merging in zone labels 
durham_redline_shp@data$id <- rownames(durham_redline_shp@data)


# Creating filepaths to tract shp files 
folder_filepaths <- list(c("nhgis0203_shapefile_tl2000_us_tract_1970", "US_tract_1970"),
                         c("nhgis0204_shapefile_tl2008_us_tract_1980", "US_tract_1980_conflated"),
                         c("nhgis0204_shapefile_tl2008_us_tract_1990", "US_tract_1990_conflated"),
                         c("nhgis0204_shapefile_tl2008_us_tract_2000", "US_tract_2000_conflated"),
                         c("nhgis0204_shapefile_tl2010_us_tract_2010", "US_tract_2010"))

count <- 1
for(year in seq(1970, 2010, 10)){
  
  # Tract shapes
  tract_shp <- rgdal::readOGR(paste0("~/Downloads/nhgis0204_shape/", folder_filepaths[[count]][1]), folder_filepaths[[count]][2])
  tract_shp <- spTransform(tract_shp, durham_redline_shp@proj4string)
  
  # Check for overlaps (takes a minute)
  overlapping_areas_shp <- rgeos::gIntersection(durham_redline_shp[c(1:15, 17:32),], tract_shp, byid = TRUE, 
                  id = paste0(rep(durham_redline_shp[c(1:15, 17:32),]$id, each = length(tract_shp)), " ", rep(tract_shp$GISJOIN, 31)))
  
  # One of the shapes is an invalid shapefile / Using gBuffer to fix this 
  overlapping_areas_fix <- rgeos::gIntersection(durham_redline_shp[16,] %>% rgeos::gBuffer(byid=TRUE, width=0), 
                                                tract_shp, byid = TRUE, 
                                                id = paste0(rep(durham_redline_shp[16,]$id, length(tract_shp)), " ", tract_shp$GISJOIN))
  
  # Combining matches and areas into a tibble
  matching_areas_tbl <- tibble(id = c(names(overlapping_areas_shp), names(overlapping_areas_fix)), 
            area = c(map(overlapping_areas_shp@polygons, ~.x@area) %>% unlist, map(overlapping_areas_fix@polygons, ~.x@area) %>% unlist))
  
  # Creating zone tbl 
  zone_by_tract_tbl <- matching_areas_tbl %>% 
    mutate(zone = str_extract(id, "^.*? ") %>% trimws, tract = str_replace(id, "^.*? ", "") %>% trimws) %>% 
    select(-id) %>% select(zone, tract, area)
  
  # Saving to disc
  setwd("~/redlining_project")
  write_csv(zone_by_tract_tbl, paste0("zone_by_tract_tbl_", year, ".csv"))

  count <- count + 1 
  print(year)
}


