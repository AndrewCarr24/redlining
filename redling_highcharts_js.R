
durham_redline_tbl2 <- read_csv('durham_redline_tbl2.csv')

durham_redline_tbl2 <- durham_redline_tbl2 %>% filter(!duplicated(name)) %>% filter(label %in% c("A", "B", "C", "D")) %>% 
  select(id, name, white_prop_1970, white_prop_1980, white_prop_1990, white_prop_2000, white_prop_2010,
         black_prop_1970, black_prop_1980, black_prop_1990, black_prop_2000, black_prop_2010)

# Manual wide to lng 
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
 
drilldown_data_lst <- list()

j <- "A4"
r <- "white"

for(j in durham_redline_tbl_lng$name){
  
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



zone_js <- paste0("function(e) {
if (!e.seriesOptions) {
var chart = this,
drilldowns = {
'zone",1,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",1,"',
data: [",drilldown_data_lst[[1]],"]
},
'zone",2,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",2,"',
data: [",drilldown_data_lst[[2]],"]
},
'zone",3,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",3,"',
data: [",drilldown_data_lst[[3]],"]
},
'zone",4,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",4,"',
data: [",drilldown_data_lst[[4]],"]
},
'zone",5,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",5,"',
data: [",drilldown_data_lst[[5]],"]
},
'zone",6,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",6,"',
data: [",drilldown_data_lst[[6]],"]
},
'zone",7,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",7,"',
data: [",drilldown_data_lst[[7]],"]
},
'zone",8,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",8,"',
data: [",drilldown_data_lst[[8]],"]
},
'zone",9,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",9,"',
data: [",drilldown_data_lst[[9]],"]
},
'zone",10,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",10,"',
data: [",drilldown_data_lst[[10]],"]
},
'zone",11,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",11,"',
data: [",drilldown_data_lst[[11]],"]
},
'zone",12,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",12,"',
data: [",drilldown_data_lst[[12]],"]
},
'zone",13,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",13,"',
data: [",drilldown_data_lst[[13]],"]
},
'zone",14,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",14,"',
data: [",drilldown_data_lst[[14]],"]
},
'zone",15,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",15,"',
data: [",drilldown_data_lst[[15]],"]
},
'zone",16,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",16,"',
data: [",drilldown_data_lst[[16]],"]
},
'zone",17,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",17,"',
data: [",drilldown_data_lst[[17]],"]
},
'zone",18,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",18,"',
data: [",drilldown_data_lst[[18]],"]
},
'zone",19,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",19,"',
data: [",drilldown_data_lst[[19]],"]
},
'zone",20,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",20,"',
data: [",drilldown_data_lst[[20]],"]
},
'zone",21,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",21,"',
data: [",drilldown_data_lst[[21]],"]
},
'zone",22,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",22,"',
data: [",drilldown_data_lst[[22]],"]
},
'zone",23,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",23,"',
data: [",drilldown_data_lst[[23]],"]
},
'zone",24,"': {
name: '", "test", "',
color: 'rgba(237,7,7,.5)',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name}'},
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: 'zone",24,"',
data: [",drilldown_data_lst[[24]],"]
}
}

series = [drilldowns[e.point.drilldownName]];

var str = e.point.name

chart.addSingleSeriesAsDrilldown(e.point, series[0]);

chart.applyDrilldown();
}

}")
