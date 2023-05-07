#-------------------------------------------------------------------------------
# SEXUAL VIOLENCE AS A TACTIC FOR TERRITORIAL CONTROL
# REPLICATION DATA: DATA PREP
# ALEX AVERY
# LAST UPDATED: 05/03/23
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# PREP
#-------------------------------------------------------------------------------
# load packages
library(geojsonio)
library(raster)
#library(rgeos)
library(sf)
library(geosphere)
library(readr)
library(lubridate)
library(tidyverse)
library(parallel)

# set wd
setwd("~/Documents/WUSTL/Third Year Paper/Data")
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# GRID CELL CREATION
#-------------------------------------------------------------------------------
# url for DRC provinces data
url <- 'https://raw.githubusercontent.com/opendatalabrdc/OSM_Exports/master/data/rd_congo_admin_4_provinces.geojson'
# create DRC provinces spatial object
drc_prov <- geojson_read(url, what = 'sp')

# now create grid cell for DRC
drc_rast <- raster(extent(drc_prov), res = 1, crs = proj4string(drc_prov))
drc_grid_polygon <- rasterToPolygons(drc_rast)
drc_grid <- raster::intersect(drc_prov, drc_grid_polygon)

# now plot DRC provinces with grid cell overlay 
# plot(drc_prov)
# plot(drc_grid, add = TRUE)

# create DRC grid sf object
drc_grid_sf <- st_as_sf(drc_grid, coords = c('lon', 'lat'),
                        crs = 4326)

# add unique id for each cell 
drc_grid_sf$grid_num <- row.names(drc_grid_sf)

# class(drc_grid_sf) <- 'sf'
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# GRID PANEL DATA CREATION
#-------------------------------------------------------------------------------
# Create sequence of dates
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2023-02-01")
dates <- seq(start_date, end_date, by = "month")

# Convert drc_grid_sf object to data frame
drc_grid_df <- drc_grid_sf %>%
  st_drop_geometry() %>%
  mutate(id = grid_num) # Add unique identifier for each grid cell

# Create panel data by merging grid cells and dates
panel_data <- expand.grid(id = unique(drc_grid_df$id), date = dates) %>%
  left_join(drc_grid_df, by = "id")

# clean up panel data
panel_data <- panel_data %>%
  dplyr::select(c(id, lat, lon, name, date)) %>%
  rename(province = name) %>%
  rename(grid_num = id) %>%
  rename(month_date = date)

panel_data$grid_num <- as.numeric(panel_data$grid_num)
panel_data$month_date <- as.character(panel_data$month_date)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# DRC CITIES
#-------------------------------------------------------------------------------
# drc city data
drc_cities <- geojson_read('~/Downloads/cod_cities_20180906h (1)/cod_cities_20180906h.shp', 
                           what = 'sp')

# create sf object
drc_cities_sf <- st_as_sf(drc_cities, coords = c('longitude', 'latitude'),
                          crs = 4326)

# 179 cities in dataset
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# CITY PANEL DATA CREATION
#-------------------------------------------------------------------------------
# Create sequence of dates
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2023-02-01")
dates <- seq(start_date, end_date, by = "month")

# Convert drc_grid_sf object to data frame
drc_cities_df <- drc_cities_sf %>%
  st_drop_geometry() %>%
  mutate(id = FID_1) # Add unique identifier for each grid cell

# Create panel data by merging grid cells and dates
drc_cities_panel <- expand.grid(id = unique(drc_cities_df$id), date = dates) %>%
  left_join(drc_cities_df, by = "id")

# remove intermediate step
rm(drc_cities_df)

# clean up panel data
drc_cities_panel <- drc_cities_panel %>%
  dplyr::select(c(id, date, name, latitude, longitude)) %>%
  rename(city_id = id) %>%
  rename(month_date = date) %>%
  rename(city_name = name) %>%
  rename(city_lon = longitude) %>%
  rename(city_lat = latitude)

drc_cities_panel$city_id <- as.numeric(drc_cities_panel$city_id )
drc_cities_panel$month_date <- as.character(drc_cities_panel$month_date)

# assign a unique id for each observation
drc_cities_panel$obs_id <- as.numeric(row.names(drc_cities_panel))

# 6802 observations
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# ACLED DATA
#-------------------------------------------------------------------------------
acled <- read_csv("acled_data_04_05_23.csv")

# subset down to changes in territory
acled <- acled[acled$disorder_type == 'Political violence' | 
                 acled$disorder_type == 'Strategic developments',]
acled <- acled[acled$event_type == 'Battles' | 
                 acled$event_type == 'Strategic developments',]
acled <- acled[acled$sub_event_type == 'Government regains territory' |
                 acled$sub_event_type == 'Non-state actor overtakes territory' |
                 acled$sub_event_type == 'Non-violent transfer of territory',]
acled <- acled[acled$sub_event_type != 'Non-violent transfer of territory',]

# create terr control variable
acled$terr_control <- ifelse(acled$sub_event_type == 'Government regains territory',
                             1, 2)

# subset down to covered time in crsv data
acled <- acled[acled$year >= 2020,]

# Format event date
acled$newdate <- lubridate::dmy(acled$event_date)
acled$sub_date <- substr(acled$newdate, start = 1, stop = 7)
acled$month_date <- paste0(acled$sub_date, '-01')

# clean up data and select relevant variables
acled <- acled %>%
  dplyr::select(c(event_id_cnty, sub_event_type, actor1, actor2, country, iso, latitude, 
           longitude, fatalities, newdate, month_date, terr_control)) %>%
  rename(acled_event_id = event_id_cnty) %>%
  rename(acled_event = sub_event_type) %>%
  rename(acled_actor1 = actor1) %>%
  rename(acled_actor2 = actor2) %>%
  rename(acled_lat = latitude) %>%
  rename(acled_lon = longitude) %>%
  rename(acled_fatalities = fatalities) %>%
  rename(acled_date = newdate) %>%
  rename(acled_terr_control = terr_control) %>%
  rename(num_iso = iso) %>%
  relocate(acled_event_id, acled_date, month_date, country)


# create number of acled events per month variable
acled <- acled %>%
  group_by(month_date) %>%
  mutate(acled_num_events = n())

# create number of fatalities per month variable
acled <- acled %>%
  group_by(month_date) %>%
  mutate(acled_fat_num = sum(acled_fatalities))

# how many state gains in territory a month
acled_gov <- acled %>%
  filter(acled_terr_control == 1) 

acled_gov <- acled_gov %>%
  group_by(month_date) %>%
  mutate(acled_gov_control = n()) %>%
  select(month_date, acled_gov_control) %>%
  distinct(month_date, acled_gov_control)

# how many nonstate gains in territory a month
acled_non_state <- acled %>%
  filter(acled_terr_control == 2) 

acled_non_state <- acled_non_state %>%
  group_by(month_date) %>%
  mutate(acled_nsa_control = n()) %>%
  select(month_date, acled_nsa_control) %>%
  distinct(month_date, acled_nsa_control)

# merge back in control variables
acled <- left_join(acled, acled_gov, by = 'month_date')
acled <- left_join(acled, acled_non_state, by = 'month_date')

rm(acled_gov)
rm(acled_non_state)

acled$acled_nsa_control <- ifelse(is.na(acled$acled_nsa_control) == TRUE, 0,
                                  acled$acled_nsa_control)

# let's just look at the drc for now
acled_drc <- acled[acled$country == 'Democratic Republic of Congo', ]

# create sf object
acled_drc_sf <- st_as_sf(acled_drc, coords = c('acled_lon', 'acled_lat'), crs = 4326)

# class(acled_drc_sf) <- 'sf'
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# CRSV DATA
#-------------------------------------------------------------------------------
# CRSV data from OCHA 
# This dataset contains publicly-reported cases of sexual violence 
# by conflict actors, security personnel, and sexual violence violence 
# that targets aid workers, educators, health workers and IDPS/refugees 
# in the DRC, Ethiopia, Nigeria, South Sudan, Sudan and Ukraine.

# load CRSV data
crsv <- read_csv("crsv_data_04_05_23.csv")

unique(crsv$country)
# drop Ukraine case
crsv <- crsv[crsv$country != 'Ukraine',]
# countries: DRC, South Sudan, Sudan, Nigeria, Ukraine, Ethiopia

# remove first row (header)
crsv <- crsv[-1,]

# remove some duplicated obs
crsv <- crsv %>%
  rename(crsv_event_id = 'SiND Event ID') %>%
  distinct(crsv_event_id, .keep_all = TRUE)

# create year variable
crsv$year <- substr(crsv$date, start = 7, stop = 10)

# change variables to numeric
crsv$Longitude <- as.numeric(crsv$Longitude)
crsv$Latitude <- as.numeric(crsv$Latitude)

# rename variables
crsv <- crsv %>%
  rename(longitude = Longitude) %>%
  rename(latitude = Latitude) %>%
  rename(admin1 = 'Admin 1') %>%
  rename(char_iso = `Country ISO`) 

# create new reported perpetrator factor
crsv$rep_perp <- crsv$'Reported Perpetrator'

unique(crsv$rep_perp)

crsv$rep_perp <- str_replace(crsv$rep_perp, 'State military', 'State Actor')
crsv$rep_perp <- str_replace(crsv$rep_perp, 'State Military', 'State Actor')
crsv$rep_perp <- str_replace(crsv$rep_perp, 'Police', 'State Actor')
crsv$rep_perp <- str_replace(crsv$rep_perp, 'NSA', 'Non State Actor')
crsv$rep_perp <- str_replace(crsv$rep_perp, 'NSAG', 'Non State Actor')
crsv$rep_perp <- str_replace(crsv$rep_perp, 'No Information', 'NA')
crsv$rep_perp <- str_replace(crsv$rep_perp, 'Employee', 'Other')
crsv$rep_perp <- str_replace(crsv$rep_perp, 'Criminal', 'Other')
crsv$rep_perp <- str_replace(crsv$rep_perp, 'Civilian', 'Other')
crsv$rep_perp <- str_replace(crsv$rep_perp, 'Non State ActorG', 'Non State Actor')

unique(crsv$rep_perp)

crsv$perp_fact <- as.factor(crsv$rep_perp)

# Format event Dates for CRSV 
crsv$newdate <-  strptime(as.character(crsv$date), "%d/%m/%Y")
crsv$newdate <- format(crsv$newdate, "%Y-%m-%d")
crsv$newdate <- lubridate::ymd(crsv$newdate)
crsv$sub_date <- substr(crsv$newdate, start = 1, stop = 7)
crsv$month_date <- paste0(crsv$sub_date, '-01')

# only keep relevant variables
crsv <- crsv %>%
  dplyr::select(country, char_iso, latitude, longitude, rep_perp, 'Survivor Or Victim Sex', 
         'Number of reported victims', newdate, month_date, crsv_event_id, perp_fact) %>%
  rename(crsv_victim_sex = 'Survivor Or Victim Sex') %>%
  rename(crsv_num_victim = 'Number of reported victims') %>%
  rename(crsv_date = newdate) %>% 
  rename(crsv_lat = latitude) %>%
  rename(crsv_lon = longitude) %>%
  rename(crsv_rep_perp = rep_perp) %>%
  rename(crsv_perp_fact = perp_fact) %>%
  relocate(crsv_event_id, crsv_date, month_date, crsv_num_victim)

# create number of crsv events & number of victims per month variable
crsv <- crsv %>%
  group_by(month_date) %>%
  mutate(crsv_num_events = n()) %>%
  mutate(crsv_month_vic = sum(crsv_num_victim))

# create number of reported government perps per month
crsv_gov <- crsv %>%
  filter(crsv_rep_perp == 'State Actor')

crsv_gov <- crsv_gov %>%
  group_by(month_date) %>%
  mutate(crsv_gov_per = n()) %>%
  dplyr::select(month_date, crsv_gov_per) %>%
  distinct(month_date, crsv_gov_per)

# create number of reported on state perps per month
crsv_nsa <- crsv %>%
  filter(crsv_rep_perp == 'Non State Actor')

crsv_nsa <- crsv_nsa %>%
  group_by(month_date) %>%
  mutate(crsv_nsa_per = n()) %>%
  dplyr::select(month_date, crsv_nsa_per) %>%
  distinct(month_date, crsv_nsa_per)

# rejoin data
crsv <- left_join(crsv, crsv_gov, by = 'month_date')
crsv <- left_join(crsv, crsv_nsa, by = 'month_date')

crsv$crsv_event_id <- as.numeric(crsv$crsv_event_id)

# again, let's just look at the DRC for now
crsv_drc <- crsv[crsv$country == 'DRC',]

# create sf object if working with grid data
crsv_drc_sf <- st_as_sf(crsv_drc, coords = c('crsv_lon', 'crsv_lat'), crs = 4326)

# class(crsv_drc_sf) <- 'sf'
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# JOINING ACLED & DRC PROVINCES GRID
#-------------------------------------------------------------------------------
# define the number of cores to use for parallel processing
num_cores <- 4

# Initialize the parallel processing cluster
cl <- makeCluster(num_cores)

# Export the required libraries to the parallel workers
clusterEvalQ(cl, {library(sf)})

# Distribute the sf objects across the parallel workers
clusterExport(cl, varlist = c("acled_drc_sf", "drc_grid_sf"))

# Define a function to perform intersection in parallel
intersect_parallel <- function(sf_obj1, sf_obj2){
  result <- st_intersects(sf_obj1, sf_obj2)
  return(result)
}

# Apply the intersection function in parallel using clusterApply()
result_list <- clusterApply(cl, list(acled_drc_sf = acled_drc_sf, drc_grid_sf = drc_grid_sf), 
                            intersect_parallel, sf_obj2 = drc_grid_sf)

# Close the parallel processing cluster
stopCluster(cl)

# unlist results
acled_drc_sf <- as.data.frame(unlist(result_list[[1]]))

# create row number variable
acled_drc$row_names <- row.names(acled_drc)
acled_drc_sf$row_names <- row.names(acled_drc_sf)
acled_drc <- left_join(acled_drc, acled_drc_sf, by = 'row_names')

# create grid number variable
colnames(acled_drc)[14] <- 'grid_num' 

# remove row names col 
acled_drc <- acled_drc %>%
  dplyr::select(!(row_names))
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# JOIN ACLED EVENTS & DRC CITIES
#-------------------------------------------------------------------------------
# Calculate the distances between the center point and each other point
distances <- apply(cbind(drc_cities_sf$longitude, drc_cities_sf$latitude), 1,
                   function(x) 
                     distGeo(x, cbind(acled_drc$acled_lon, acled_drc$acled_lat))) / 1000

# transpose distances matrix
distances <- t(distances)

# change to data frame
distances <- as.data.frame(distances)

# add column names
colnames(distances) <- acled_drc$acled_event_id

# add city ids
distances$FID_1 <- drc_cities_sf$FID_1

# Define the threshold radius
radius_km <- 100

# Find the minimum distance and corresponding FID_1 for each column
min_distances <- apply(distances, 2, function(x){
  min_dist <- min(x)
  if (min_dist > radius_km){
    NA
  }
  else{
    distances$FID_1[which.min(x)]
  }
})

# Replace observations with NA where the minimum distance is greater than the radius
for(i in 1:ncol(distances)){
  if(is.na(min_distances[i] == TRUE)){
    distances[, i] <- NA
  }
}

# re-transpose
distances <- t(distances)
distances <- as.data.frame(distances)

# add col names
colnames(distances) <- drc_cities_sf$FID_1

# add closest city id variable
distances$city_id <- min_distances

# add drc event id column for easy merging
distances$acled_event_id <- row.names(distances)

# only keep needed variables
distances <- distances %>%
  dplyr::select(acled_event_id, city_id)

# join datasets
acled_drc <- left_join(acled_drc, distances, by = 'acled_event_id')
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# JOINING CRSV & DRC PROVINCES GRID
#-------------------------------------------------------------------------------
# define the number of cores to use for parallel processing
num_cores <- 4

# Initialize the parallel processing cluster
cl <- makeCluster(num_cores)

# Export the required libraries to the parallel workers
clusterEvalQ(cl, {library(sf)})

# Distribute the sf objects across the parallel workers
clusterExport(cl, varlist = c("crsv_drc_sf", "drc_grid_sf"))

# Define a function to perform intersection in parallel
intersect_parallel <- function(sf_obj1, sf_obj2){
  result <- st_intersects(sf_obj1, sf_obj2)
  return(result)
}

# Apply the intersection function in parallel using clusterApply()
result_list <- clusterApply(cl, list(crsv_drc_sf = crsv_drc_sf, drc_grid_sf = drc_grid_sf), 
                            intersect_parallel, sf_obj2 = drc_grid_sf)

# Close the parallel processing cluster
stopCluster(cl)

# unlist results
crsv_drc_sf <- as.data.frame(unlist(result_list[[1]]))

# # create row number variable
crsv_drc$row_names <- row.names(crsv_drc)
crsv_drc_sf$row_names <- row.names(crsv_drc_sf)
crsv_drc <- left_join(crsv_drc, crsv_drc_sf, by = 'row_names')

# create grid number variable
colnames(crsv_drc)[14] <- 'grid_num'   

# remove row names col
crsv_drc <- crsv_drc %>%
  dplyr::select(!(row_names))
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# JOIN CRSV EVENTS & DRC CITIES
#-------------------------------------------------------------------------------
rm(distances)

# Calculate the distances between the center point and each other point
distances <- apply(cbind(drc_cities_sf$longitude, drc_cities_sf$latitude), 1,
                   function(x) 
                     distGeo(x, cbind(crsv_drc$crsv_lon, crsv_drc$crsv_lat))) / 1000

# transpose distances matrix
distances <- t(distances)

# change to data frame
distances <- as.data.frame(distances)

# add column names
colnames(distances) <- crsv_drc$crsv_event_id

# add city ids
distances$FID_1 <- drc_cities_sf$FID_1

# Define the threshold radius
radius_km <- 100

# Find the minimum distance and corresponding FID_1 for each column
min_distances <- apply(distances, 2, function(x){
  min_dist <- min(x)
  if (min_dist > radius_km){
    NA
  }
  else{
    distances$FID_1[which.min(x)]
  }
})

# Replace observations with NA where the minimum distance is greater than the radius
for(i in 1:ncol(distances)){
  if(is.na(min_distances[i] == TRUE)){
    distances[, i] <- NA
  }
}

# re-transpose
distances <- t(distances)
distances <- as.data.frame(distances)

# add col names
colnames(distances) <- drc_cities_sf$FID_1

# add closest city id variable
distances$city_id <- min_distances

# add drc event id column for easy merging
distances$crsv_event_id <- row.names(distances)
distances$crsv_event_id <- as.numeric(distances$crsv_event_id)

# only keep needed variables
distances <- distances %>%
  dplyr::select(crsv_event_id, city_id)

# join datasets
crsv_drc <- left_join(crsv_drc, distances, by = 'crsv_event_id')
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# CONTROL VARIABLES
#-------------------------------------------------------------------------------
# What would effect crsv levels during conflict?

# conflict level controls:
  # civilian fatalities

# country level controls:
  # Gender equality 
  # polity score 
  
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# COMBINE ALL GRID CELL DATA
#-------------------------------------------------------------------------------
# first join acled data
panel_drc_data <- full_join(panel_data, acled_drc, by = c('grid_num', 'month_date'))

# create binary variable for whether territorial change occured during the 
# specified month in each grid cell
panel_drc_data$terr_event <- ifelse(is.na(panel_drc_data$acled_event_id) == TRUE, 0, 1)   

# create number of territorial change events 
panel_drc_data$total_terr_event <- ifelse(is.na(panel_drc_data$acled_num_events) == TRUE,
                                         0, panel_drc_data$acled_num_events)

# next join crsv data
panel_drc_data <- full_join(panel_drc_data, crsv_drc, by = c('grid_num', 'month_date',
                                                             'country'))

# create binary variable for whether crsv event occured during the 
# specified month in each grid cell
panel_drc_data$crsv_event <- ifelse(is.na(panel_drc_data$crsv_event_id) == TRUE,
                                    0, 1)

# create number of crsv events 
panel_drc_data$total_crsv_event <- ifelse(is.na(panel_drc_data$crsv_num_events) == TRUE,
                                         0, panel_drc_data$crsv_num_events)

# create number of crsv victims
panel_drc_data$total_crsv_vics <- ifelse(is.na(panel_drc_data$crsv_num_victim) == TRUE,
                                        0, panel_drc_data$crsv_num_victim)

# remove duplicate obs
panel_drc_data <- panel_drc_data %>% 
  distinct(grid_num, month_date, 
           .keep_all = TRUE)


# create lagged terr variable 
panel_drc_data$terr_event_lag <- lag(panel_drc_data$terr_event, k = 1)
panel_drc_data$total_terr_event_lag <- lag(panel_drc_data$total_terr_event, k = 1)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# COMBINE ALL CITY DATA
#-------------------------------------------------------------------------------
# join with drc city panel data
drc_cities_df <- left_join(drc_cities_panel, acled_drc, by = c('city_id', 'month_date'))

# create binary variable for whether territorial change occured during the 
# specified month in each city
drc_cities_df$terr_event <- ifelse(is.na(drc_cities_df$acled_event_id) == TRUE, 0, 1) 

# create number of territorial change events 
drc_cities_df$total_terr_event <- ifelse(is.na(drc_cities_df$acled_num_events) == TRUE,
                                         0, drc_cities_df$acled_num_events)

# create number of fatalities 
drc_cities_df$total_acled_fat <- ifelse(is.na(drc_cities_df$acled_fat_num) == TRUE,
                                        0, drc_cities_df$acled_fat_num)

# binary variable of gov control 
drc_cities_df$gov_control <- ifelse(is.na(drc_cities_df$acled_gov_control) == TRUE, 0, 1)

# binary nonstate control
drc_cities_df$nsa_control <- ifelse(is.na(drc_cities_df$acled_nsa_control) == TRUE, 0, 1)

# number of gov control
drc_cities_df$total_gov_control <- ifelse(is.na(drc_cities_df$acled_gov_control) == TRUE,
                                          0, drc_cities_df$acled_gov_control)
# number of nonstate control
drc_cities_df$total_nsa_control <- ifelse(is.na(drc_cities_df$acled_nsa_control) == TRUE,
                                          0, drc_cities_df$acled_nsa_control)

# remove duplicate obs
drc_cities_df <- drc_cities_df %>%
  distinct(city_id, month_date, terr_event, total_terr_event, total_acled_fat, gov_control,
           nsa_control, total_gov_control, total_nsa_control, .keep_all = TRUE)

# next join crsv data
drc_cities_df <- left_join(drc_cities_df, crsv_drc, by = c('city_id', 'month_date'))
                                                            
# create binary variable for whether crsv event occured during the 
# specified month in each grid cell
drc_cities_df$crsv_event <- ifelse(is.na(drc_cities_df$crsv_event_id) == TRUE,
                                    0, 1)

# create number of crsv events 
drc_cities_df$total_crsv_event <- ifelse(is.na(drc_cities_df$crsv_num_events) == TRUE,
                                         0, drc_cities_df$crsv_num_events)

# create number of crsv victims
drc_cities_df$total_crsv_vics <- ifelse(is.na(drc_cities_df$crsv_num_victim) == TRUE,
                                         0, drc_cities_df$crsv_num_victim)
# create number of times gov was perp
drc_cities_df$total_gov_perp <- ifelse(is.na(drc_cities_df$crsv_gov_per) == TRUE,
                                       0, drc_cities_df$crsv_gov_per)

# create number of times nsa was perp
drc_cities_df$total_nsa_perp <- ifelse(is.na(drc_cities_df$crsv_nsa_per) == TRUE,
                                       0, drc_cities_df$crsv_nsa_per)

# remove duplicate obs
drc_cities_df <- drc_cities_df %>%
  distinct(city_id, month_date, crsv_event, total_crsv_event, total_crsv_vics, .keep_all = TRUE)
drc_cities_df <- drc_cities_df %>%
  distinct(city_id, month_date, .keep_all = TRUE)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# PRELIM MODELS
#-------------------------------------------------------------------------------
library(plm)
library(sandwich)
library(lmtest)

# possible outcome variables: total_crsv_event, total_crsv_vics, total_gov_perp, 
# total_nsa_perp
# possible predictor variables: total_gov_control, total_nsa_control, total_terr_event
# control: total_acled_fat

city_model <- plm(total_nsa_perp ~ total_gov_control + total_acled_fat, 
                   data = drc_cities_df,
                   model = 'within',
                   index = c('city_id', 'month_date'))
coeftest(city_model, vcov. = vcovHC(city_model, typw = 'HC2'))

city_model <- plm(total_gov_perp ~ total_nsa_control + total_acled_fat, 
                  data = drc_cities_df,
                  model = 'within',
                  index = c('city_id', 'month_date'))
coeftest(city_model, vcov. = vcovHC(city_model, typw = 'HC2'))


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# NOTES
#-------------------------------------------------------------------------------
#overlay city file: observation: town, month, control of territory, crsv, how long, 
#threshold, crsv lag by month 

#crsv~ violent exchange + violent exchange  t-1, which 

# Overall, an increase in violent exchanges in territory increases the number of crsv events, but 
# this does not carry statistical significance

# government regaining territory increases total crsv events, while non-state actor regaining
# territory decreases number of total crsv events, but neither are significant



















  