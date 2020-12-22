# Runs inside redline_highcharts.R 

drilldown_data_lst <- list()
for(j in durham_redline_tbl_lng$name[1:24]){
  
  for(r in c("white", "black")){
    
    datums <- list() 
    tbl <- durham_redline_tbl_lng %>% filter(name == j & race == r)
    
    for(i in 1:nrow(tbl)){
      
      x_tooltip <- tbl %>% pull(x) %>% .[i]
      x <- x_tooltip
      
      y <- tbl %>% pull(y) %>% .[i]
      y_tooltip <- str_replace(as.character(y), " ", ",")
      y_rnd <- round(y, 3)
      
      name <- tbl %>% pull(name) %>% .[i]
      name <- str_replace_all(name, "'", "\\\\'")
      
      datums[[i]] <- paste0("{","name:", "'", name, "'", ",", "x:", x, ",", "x_tooltip:", x_tooltip, ",",
                            "y:", y, ",", "y_tooltip:", y_tooltip, ",", "y_rnd:", y_rnd, "}")
      
    }
    
    drilldown_data_lst[[(length(drilldown_data_lst)+1)]] <- paste(datums %>% unlist, collapse = ",")
  }
}


################
# Drilldown JS #
################

js_start <- "function(e) {
if (!e.seriesOptions) {
var chart = this,
drilldowns = {"

js_end <- "
}

series = [drilldowns[e.point.drilldownName]];

var str = e.point.name

series = [drilldowns[e.point.drilldownName], drilldowns[e.point.drilldownName + '2']];

chart.addSingleSeriesAsDrilldown(e.point, series[0]);
chart.addSingleSeriesAsDrilldown(e.point, series[1]);
chart.applyDrilldown();
}

chart.title.update({
text: 'Proportion Black and White Households 1970-2010'
})

}"

# Creating color string
colors_str_mod <- rep(c("#DDA77B", "#37123C"), 24)

# Creating names string
names_str_mod <- paste0(str_sub(names_str, 1, 1), "_", str_extract(names_str, "[0-9]+"))
names_str_mod <- paste0(rep(names_str_mod, each = 2), c("", "2"))

# Race str
race_str <- rep(c("White", "Black"), 24)

# Zone str
zone_str <- rep(paste0("zone", 1:24), each = 2)

# Creating drilldown json data 
drilldown_json <- paste0("
'", names_str_mod, "': {
name: '", race_str, "',
color: '", colors_str_mod, "',
tooltip: {headerFormat: '', pointFormat: '<b>Prop:</b> {point.y_rnd} <br><b>Year:</b> {point.x}'},
type: 'line',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '", zone_str, "',
data: [",
drilldown_data_lst
,"]
}")

zone_js <- paste0(js_start, paste(drilldown_json, collapse = ","), js_end)

##############
# Drillup JS #
##############

zone_js2 <- "function(e) {

var chart = this;

chart.title.update({
text: 'Redlining in Durham'
})


                  }"

