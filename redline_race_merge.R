

##############
#### 1970 ####
##############

# Zone by tract 
zone_by_tract_tbl <- read_csv("~/redlining_project/zone_by_tract_tbl_1970.csv")

# Reading in race data 
race_tbl <- read_csv("~/Downloads/nhgis0202_csv/nhgis0202_ds95_1970_tract.csv")

race_tbl <- race_tbl %>% filter(STATE == "North Carolina" & COUNTY == "Durham") %>%  
  mutate(white = CEB001 + CEB010, black = CEB002 + CEB011, 
         tot = CEB001 + CEB002 + CEB003 + CEB004 + CEB005 + CEB006 + CEB007 + CEB008 + CEB009 + 
           CEB010 + CEB011 + CEB012 + CEB013 + CEB014 + CEB015 + CEB016 + CEB017 + CEB018) %>% 
  select(GISJOIN, tot, white, black) 

# Creating race_totals_by_zone_tbl
race_totals_by_zone_tbl <- zone_by_tract_tbl %>% group_by(zone) %>% mutate(area_prop = area/sum(area)) %>% 
  inner_join(race_tbl, c("tract" = "GISJOIN")) %>% 
  summarise(tot = sum(area_prop*tot), white = sum(area_prop*white), black = sum(area_prop*black), .groups = 'drop') %>% 
  mutate(white_prop = white/tot, black_prop = black/tot)

# Merging into durham_redline_tbl2
durham_redline_tbl2 <- left_join(durham_redline_tbl2, race_totals_by_zone_tbl %>% select(-c("tot", "white", "black")), c("id" = "zone")) %>% 
  rename(white_prop_1970 = white_prop, black_prop_1970 = black_prop)


##############
#### 1980 ####
##############

# Zone by tract 
zone_by_tract_tbl <- read_csv("~/redlining_project/zone_by_tract_tbl_1980.csv")

# Reading in race data 
race_tbl <- read_csv("~/Downloads/nhgis0205_csv/nhgis0205_ds104_1980_tract.csv")

race_tbl <- race_tbl %>% filter(STATE == "North Carolina" & COUNTY == "Durham") %>%  
  mutate(white = C9G001, black = C9G002, 
         tot = C9G001 + C9G002 + C9G003 + C9G004) %>% 
  select(GISJOIN, tot, white, black) 

# Creating race_totals_by_zone_tbl
race_totals_by_zone_tbl <- zone_by_tract_tbl %>% group_by(zone) %>% mutate(area_prop = area/sum(area)) %>% 
  inner_join(race_tbl, c("tract" = "GISJOIN")) %>% 
  summarise(tot = sum(area_prop*tot), white = sum(area_prop*white), black = sum(area_prop*black), .groups = 'drop') %>% 
  mutate(white_prop = white/tot, black_prop = black/tot)

# Merging into durham_redline_tbl2
durham_redline_tbl2 <- left_join(durham_redline_tbl2, race_totals_by_zone_tbl %>% select(-c("tot", "white", "black")), c("id" = "zone")) %>% 
  rename(white_prop_1980 = white_prop, black_prop_1980 = black_prop)


##############
#### 1990 ####
##############

# Zone by tract 
zone_by_tract_tbl <- read_csv("~/redlining_project/zone_by_tract_tbl_1990.csv")

# Reading in race data 
race_tbl <- read_csv("~/Downloads/nhgis0205_csv/nhgis0205_ds120_1990_tract.csv")

race_tbl <- race_tbl %>% filter(STATE == "North Carolina" & COUNTY == "Durham") %>%  
  mutate(white = EUY001, black = EUY002, 
         tot = EUY001 + EUY002 + EUY003 + EUY004 + EUY005) %>% 
  select(GISJOIN, tot, white, black) 

# Creating race_totals_by_zone_tbl
race_totals_by_zone_tbl <- zone_by_tract_tbl %>% group_by(zone) %>% mutate(area_prop = area/sum(area)) %>% 
  inner_join(race_tbl, c("tract" = "GISJOIN")) %>% 
  summarise(tot = sum(area_prop*tot), white = sum(area_prop*white), black = sum(area_prop*black), .groups = 'drop') %>% 
  mutate(white_prop = white/tot, black_prop = black/tot)

# Merging into durham_redline_tbl2
durham_redline_tbl2 <- left_join(durham_redline_tbl2, race_totals_by_zone_tbl %>% select(-c("tot", "white", "black")), c("id" = "zone")) %>% 
  rename(white_prop_1990 = white_prop, black_prop_1990 = black_prop)



##############
#### 2000 ####
##############

# Zone by tract 
zone_by_tract_tbl <- read_csv("~/redlining_project/zone_by_tract_tbl_2000.csv")

# Reading in race data 
race_tbl <- read_csv("~/Downloads/nhgis0205_csv/nhgis0205_ds146_2000_tract.csv")

race_tbl <- race_tbl %>% filter(STATE == "North Carolina" & COUNTY == "Durham") %>%  
  mutate(white = FMR001, black = FMR002, 
         tot = FMR001 + FMR002 + FMR003 + FMR004 + FMR005 + FMR006 + FMR007) %>% 
  select(GISJOIN, tot, white, black) 

# Creating race_totals_by_zone_tbl
race_totals_by_zone_tbl <- zone_by_tract_tbl %>% group_by(zone) %>% mutate(area_prop = area/sum(area)) %>% 
  inner_join(race_tbl, c("tract" = "GISJOIN")) %>% 
  summarise(tot = sum(area_prop*tot), white = sum(area_prop*white), black = sum(area_prop*black), .groups = 'drop') %>% 
  mutate(white_prop = white/tot, black_prop = black/tot)

# Merging into durham_redline_tbl2
durham_redline_tbl2 <- left_join(durham_redline_tbl2, race_totals_by_zone_tbl %>% select(-c("tot", "white", "black")), c("id" = "zone")) %>% 
  rename(white_prop_2000 = white_prop, black_prop_2000 = black_prop)


##############
#### 2010 ####
##############

# Zone by tract 
zone_by_tract_tbl <- read_csv("~/redlining_project/zone_by_tract_tbl_2010.csv")

# Reading in race data 
race_tbl <- read_csv("~/Downloads/nhgis0205_csv/nhgis0205_ds172_2010_tract.csv")

race_tbl <- race_tbl %>% filter(STATE == "North Carolina" & COUNTY == "Durham County") %>%  
  mutate(white = H7X002, black = H7X003, 
         tot = H7X001) %>% 
  select(GISJOIN, tot, white, black) 

# Creating race_totals_by_zone_tbl
race_totals_by_zone_tbl <- zone_by_tract_tbl %>% group_by(zone) %>% mutate(area_prop = area/sum(area)) %>% 
  inner_join(race_tbl, c("tract" = "GISJOIN")) %>% 
  summarise(tot = sum(area_prop*tot, na.rm=T), white = sum(area_prop*white, na.rm=T), black = sum(area_prop*black, na.rm=T), .groups = 'drop') %>% 
  mutate(white_prop = white/tot, black_prop = black/tot)

# Merging into durham_redline_tbl2
durham_redline_tbl2 <- left_join(durham_redline_tbl2, race_totals_by_zone_tbl %>% select(-c("tot", "white", "black")), c("id" = "zone")) %>% 
  rename(white_prop_2010 = white_prop, black_prop_2010 = black_prop)




