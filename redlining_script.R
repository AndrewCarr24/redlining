library(ggplot2)
library(stringr)
library(dplyr)

# Takes shapefile and returns scaled dimensions for ggplot 
get_dims <- function(shp){
  
  long_dims <- shp$long %>% range
  lat_dims <- shp$lat %>% range
  
  if(abs(diff(long_dims)) < abs(diff(lat_dims))){
    
    mid_point <- (long_dims[1]+long_dims[2])/2
    long_dims[2] <- mid_point + abs(diff(lat_dims))/2
    long_dims[1] <- mid_point - abs(diff(lat_dims))/2
    
    
  }else{
    
    mid_point <- (lat_dims[1]+lat_dims[2])/2
    lat_dims[2] <- mid_point + abs(diff(long_dims))/2
    lat_dims[1] <- mid_point - abs(diff(long_dims))/2
    
  }
  
  return(list(long_dims, lat_dims))
  
}


setwd("~")
setwd("Downloads")

# Get shapefile of redlining by Durham zones 
durham_redline_shp <- rgdal::readOGR("durhamredliningzones", "durhamredliningzones")

# Turning shapefile into dataframe 
durham_redline_tbl <- fortify(durham_redline_shp) %>% as_tibble

# Merging in zone labels 
durham_redline_shp@data$id <- rownames(durham_redline_shp@data)
durham_redline_tbl <- left_join(durham_redline_tbl, durham_redline_shp@data, "id") 

# limiting to color-coded areas
durham_redline_tbl2 <- durham_redline_tbl %>% mutate(label = str_sub(label, 1, 1))

setwd("~/redlining_project")
write_csv(durham_redline_tbl2, 'durham_redline_tbl2.csv')


# Get shapefile of redlining by Durham zones 
durham_redline_shp <- rgdal::readOGR("~/Downloads/durhamredliningzones", "durhamredliningzones")

# Turning shapefile into dataframe 
durham_redline_tbl <- fortify(durham_redline_shp) %>% as_tibble

# Merging in zone labels 
durham_redline_shp@data$id <- rownames(durham_redline_shp@data)
durham_redline_tbl <- left_join(durham_redline_tbl, durham_redline_shp@data, "id") 








# # Second plot (limiting to color-coded areas)
# ggplot(durham_redline_tbl2 %>% filter(label %in% c("A", "B", "C", "D")), aes(x=long, y=lat, fill=id)) + geom_polygon() + 
#   xlim(get_dims(durham_redline_tbl)[[1]]) + ylim(get_dims(durham_redline_tbl)[[2]])
# 
# # Third plot - grouping by id but applying colors by label 
# ggplot(durham_redline_tbl2 %>% filter(label %in% c("A", "B", "C", "D")), aes(x=long, y=lat, group=id, fill=label)) + geom_polygon() + 
#   xlim(get_dims(durham_redline_tbl)[[1]]) + ylim(get_dims(durham_redline_tbl)[[2]])
# 
# # Getting colors right 
# ggplot(durham_redline_tbl2 %>% filter(label %in% c("A", "B", "C", "D")), aes(x=long, y=lat, group=id, fill=label)) + geom_polygon() + 
#   xlim(get_dims(durham_redline_tbl)[[1]]) + ylim(get_dims(durham_redline_tbl)[[2]]) + 
#   scale_fill_manual(values = c("#31ad71", "#719cd9", "#ffe482", "#fc6658")) + theme_void() + theme(legend.position="none")


