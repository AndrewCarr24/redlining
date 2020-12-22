library(highcharter)
library(jsonlite)

# Get shapefile of redlining by Durham zones 
durham_redline_shp <- rgdal::readOGR("~/Downloads/durhamredliningzones", "durhamredliningzones")

# Turning shapefile into dataframe 
durham_redline_tbl <- fortify(durham_redline_shp) %>% as_tibble

# Merging in zone labels 
durham_redline_shp@data$id <- rownames(durham_redline_shp@data)

# Merging shapefile data into tbl 
durham_redline_tbl <- left_join(durham_redline_tbl, durham_redline_shp@data, "id") 

# limiting to color-coded areas
durham_redline_tbl2 <- durham_redline_tbl %>% mutate(label = str_sub(label, 1, 1))

# Making id numeric 
durham_redline_tbl2 <- durham_redline_tbl2 %>% mutate(id = as.numeric(id))

# Script that merges Census data into redlining tbl  
source("~/redlining_project/redline_race_merge.R")

#####################

# Converting to geojson
durham_redline_json <- geojson::as.geojson(durham_redline_shp) %>% fromJSON(., simplifyVector = FALSE)

# Creating zone_idx -- ids of zones in final map 
zone_idx <- durham_redline_tbl2 %>% filter(label %in% c("A", "B", "C", "D")) %>% .[["id"]] %>% unique + 1 

# Adding id to json
count <- 1 
for(i in zone_idx){
  durham_redline_json$features[[i]]$properties$NAME <- paste0("zone", count)
  count <- count + 1 
}

# Creating color string
colors_str <- durham_redline_tbl2 %>% filter(label %in% c("A", "B", "C", "D")) %>% filter(!duplicated(id)) %>% 
  mutate(color_rgb = ifelse(label == "A", "#31ad71", 
                            ifelse(label == "B", "#719cd9", 
                                   ifelse(label == "C", "#ffe482", "#fc6658")))) %>% .[["color_rgb"]]

# Creating color string
names_str <- durham_redline_tbl2 %>% filter(label %in% c("A", "B", "C", "D")) %>% filter(!duplicated(id)) %>% .[["name"]]

# Converting durham_redline_tbl2 from wide to lng 
durham_redline_tbl2 <- durham_redline_tbl2 %>% filter(!duplicated(name)) %>% filter(label %in% c("A", "B", "C", "D")) %>% 
  select(id, name, white_prop_1970, white_prop_1980, white_prop_1990, white_prop_2000, white_prop_2010,
         black_prop_1970, black_prop_1980, black_prop_1990, black_prop_2000, black_prop_2010)

durham_redline_tbl_lng <- bind_rows(
  durham_redline_tbl2 %>% select(id, name, white_prop_1970) %>% rename(y = white_prop_1970) %>% add_column(x = "white_1970"), 
  durham_redline_tbl2 %>% select(id, name, white_prop_1980) %>% rename(y = white_prop_1980) %>% add_column(x = "white_1980"), 
  durham_redline_tbl2 %>% select(id, name, white_prop_1990) %>% rename(y = white_prop_1990) %>% add_column(x = "white_1990"), 
  durham_redline_tbl2 %>% select(id, name, white_prop_2000) %>% rename(y = white_prop_2000) %>% add_column(x = "white_2000"), 
  durham_redline_tbl2 %>% select(id, name, white_prop_2010) %>% rename(y = white_prop_2010) %>% add_column(x = "white_2010"),
  durham_redline_tbl2 %>% select(id, name, black_prop_1970) %>% rename(y = black_prop_1970) %>% add_column(x = "black_1970"), 
  durham_redline_tbl2 %>% select(id, name, black_prop_1980) %>% rename(y = black_prop_1980) %>% add_column(x = "black_1980"), 
  durham_redline_tbl2 %>% select(id, name, black_prop_1990) %>% rename(y = black_prop_1990) %>% add_column(x = "black_1990"), 
  durham_redline_tbl2 %>% select(id, name, black_prop_2000) %>% rename(y = black_prop_2000) %>% add_column(x = "black_2000"), 
  durham_redline_tbl2 %>% select(id, name, black_prop_2010) %>% rename(y = black_prop_2010) %>% add_column(x = "black_2010")
)

durham_redline_tbl_lng <- durham_redline_tbl_lng %>% 
  mutate(race = str_replace(x, "_.*?$", "") %>% trimws) %>% 
  mutate(x = str_extract(x, "_.*?$") %>% str_replace("_", "") %>% as.numeric) %>% 
  select(id, name, x, y, race)

# Getting JS 
source("~/redlining_project/redline_highcharts_js2.R")

for(i in 1:length(zone_idx)){
c(durham_redline_json$features[zone_idx[i]][[1]]$properties$label, names_str[i]) %>% print
}



##############
#### Plot ####
##############


# Highcharts global options 
hcoptslang <- getOption("highcharter.lang")
hcoptslang$drillUpText <- "Return to map"
options(highcharter.lang = hcoptslang)


highchart() %>% 
  hc_chart(events = list(
             drilldown = JS(zone_js), 
             drillup = JS(zone_js2))) %>% 
  hc_add_series(name = names_str[1], 
                data = list(list(drilldown = TRUE, drilldownName = "A_4", 
                                 NAME = "zone1")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[1], 
                mapData = list(x=1,type = "FeatureCollection", features = durham_redline_json$features[zone_idx[1]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = names_str[2], 
                data = list(list(drilldown = TRUE, drilldownName = "D_5", 
                                 NAME = "zone2")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[2], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[2]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = names_str[3], 
                data = list(list(drilldown = TRUE, drilldownName = "C_1", 
                                 NAME = "zone3")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[3], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[3]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = names_str[4], 
                data = list(list(drilldown = TRUE, drilldownName = "A_1", 
                                 NAME = "zone4")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[4], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[4]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = names_str[5], 
                data = list(list(drilldown = TRUE, drilldownName = "A_2", 
                                 NAME = "zone5")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[5], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[5]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = names_str[6], 
                data = list(list(drilldown = TRUE, drilldownName = "A_3", 
                                 NAME = "zone6")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[6], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[6]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = names_str[7], 
                data = list(list(drilldown = TRUE, drilldownName = "A_5", 
                                 NAME = "zone7")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[7], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[7]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = names_str[8], 
                data = list(list(drilldown = TRUE, drilldownName = "B_1", 
                                 NAME = "zone8")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[8], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[8]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[9], 
                data = list(list(drilldown = TRUE, drilldownName = "B_2", 
                                 NAME = "zone9")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[9], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[9]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = names_str[10], 
                data = list(list(drilldown = TRUE, drilldownName = "B_3", 
                                 NAME = "zone10")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[10], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[10]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[11], 
                data = list(list(drilldown = TRUE, drilldownName = "B_4", 
                                 NAME = "zone11")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[11], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[11]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[12], 
                data = list(list(drilldown = TRUE, drilldownName = "B_5", 
                                 NAME = "zone12")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[12], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[12]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[13], 
                data = list(list(drilldown = TRUE, drilldownName = "B_6", 
                                 NAME = "zone13")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[13], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[13]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[14], 
                data = list(list(drilldown = TRUE, drilldownName = "C_2", 
                                 NAME = "zone14")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[14], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[14]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[15], 
                data = list(list(drilldown = TRUE, drilldownName = "C_3", 
                                 NAME = "zone15")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[15], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[15]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[16], 
                data = list(list(drilldown = TRUE, drilldownName = "C_4", 
                                 NAME = "zone16")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[16], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[16]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[17], 
                data = list(list(drilldown = TRUE, drilldownName = "C_5", 
                                 NAME = "zone17")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[17], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[17]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[18], 
                data = list(list(drilldown = TRUE, drilldownName = "C_6", 
                                 NAME = "zone18")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[18], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[18]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = names_str[19], 
                data = list(list(drilldown = TRUE, drilldownName = "C_7", 
                                 NAME = "zone19")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[19], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[19]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[20], 
                data = list(list(drilldown = TRUE, drilldownName = "D_1", 
                                 NAME = "zone20")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[20], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[20]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = names_str[21], 
                data = list(list(drilldown = TRUE, drilldownName = "D_2", 
                                 NAME = "zone21")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[21], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[21]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[22], 
                data = list(list(drilldown = TRUE, drilldownName = "D_3", 
                                 NAME = "zone22")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[22], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[22]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = names_str[23], 
                data = list(list(drilldown = TRUE, drilldownName = "D_4", 
                                 NAME = "zone23")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[23], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[23]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = names_str[24], 
                data = list(list(drilldown = TRUE, drilldownName = "D_6", 
                                 NAME = "zone24")),
                joinBy = "NAME", 
                borderColor = "#c7c3c3",
                color = colors_str[24], 
                mapData = list(x = 1, type = "FeatureCollection", features = durham_redline_json$features[zone_idx[24]]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>% 
  hc_title(text = list("Redlining in Durham"), 
           style = list(fontFamily = "Montserrat", fontWeight = "bold")) %>% 
  hc_xAxis(visible = T) %>% 
  hc_yAxis(visible = T) 



##

# chart.yAxis[0].update({
#   min:-36.03726, max:-35.95999
# })

# chart.xAxis[0].update({
#   min:-78.94170, max:-78.86443
# })

#chart.update({
#  chart:{plotBackgroundImage: 'https://www.highcharts.com/samples/graphics/skies.jpg'}
#})
