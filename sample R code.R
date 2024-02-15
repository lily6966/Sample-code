#This is a sample of my code only without required dependencies. Therefore, it cannot run appropriately. 
#load required libraries
library(fields)
require(rpart)
library(mgcv)
library(sf)
library(raster)
library(lubridate)
library(ranger)
library(scam)
library(PresenceAbsence)
library(verification)
library(ebirdst)
library(fields)
library(gridExtra)
library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(ranger)
library(scam)
library(PresenceAbsence)
library(verification)
library(ebirdst)
library(fields)
library(gridExtra)
library(tidyverse)
library(dggridR)
library(mlr)
library(dplyr)
library(ISOweek)

# resolve namespace conflicts
select <- dplyr::select
map <- purrr::map
projection <- raster::projection
#------------

# set random number seed to insure fully repeatable results
set.seed(1)


# -----------------------------------------------------------------
# pulling species and habitat data for training and testing data sets
# --------------------------------------------------------------------
species<-"dunlin"
# ebird data
ebird <- read_csv(paste0("data/ebd_", species, "_bbox_zf.csv")) %>% 
  # year required to join to habitat data
  mutate(year = year(observation_date))
ebird_count <- ebird %>% 
  mutate(protocol_type = factor(protocol_type, 
                                levels = c("Stationary" , "Traveling"))) %>%
  # remove observations with no count
  filter(!is.na(observation_count))
# habitat covariates with crops categorized 
habitat_crop <- read_csv("/Users/lily/Library/CloudStorage/Box-Box/Thesis/SDM/code/data//pland-elev_nort_east_canopy_location-10year_checklist_greyel.csv") %>% 
  mutate(year = as.integer(year))

# combine ebird_count and habitat data
ebird_habitat_count_crop <- inner_join(ebird_count, habitat_crop, by = c("locality_id", "year"))

# prediction surface

r <- raster("data/prediction-surface-cv.tif")

# load gis data for making maps
map_proj <- map_proj <- "ESRI:102003"
ne_land <- read_sf("data/gis-data.gpkg", "ne_land") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()
cv <- read_sf("data/gis-data.gpkg", "cv") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()
ne_country_lines <- read_sf("data/gis-data.gpkg", "ne_country_lines") %>% 
  st_transform(crs = map_proj) %>% 
  st_geometry()

# generate hexagonal grid with ~ 5 km betweeen cells
dggs <- dgconstruct(spacing = 5)
# get hexagonal cell id and week number for each checklist
checklist_cell_count <- ebird_habitat_count_crop %>% 
  mutate(cell = dgGEO_to_SEQNUM(dggs, longitude, latitude)$seqnum,
         year = year(observation_date),
         week = week(observation_date))

# sample one checklist per grid cell per week
# sample detection/non-detection independently 
ebird_ss_count <- checklist_cell_count %>% 
  group_by(species_observed, year, week, cell) %>% 
  sample_n(size = 1) %>% 
  ungroup() 

ebird_ss_count <- ebird_ss_count %>% 
  # select only the columns to be used in the model
  dplyr::select(-checklist_id, -observer_id, -sampling_event_identifier,
                -scientific_name, -species_observed, -state_code,
                -locality_id, -all_species_reported, -observation_date,   
                -cell) %>% 
  drop_na() 




# ------------------------------------------
#Evaluate the geographical scale effects on prediction performance
#aggregated the training data across a 25.2 × 25.2 km grid, 
#separately for each year. 
#The aggregation summed the counts of species seen, 
#the durations spent searching for birds, 
#the distances traveled during the search, 
#the numbers of people in the search party, 
#and the checklist calibration values 
#(weighted by search duration) across all checklists within each grid cell. 
#All other covariates were averaged across all checklists within each grid cell.
# ------------------------------------------

# load the landcover data to coerce crs and res
landcover <- list.files("data/landcover", "^cdl_cv_albers_", 
                        full.names = TRUE) %>% 
  stack()
# label layers with year
landcover <- names(landcover) %>% 
  str_extract("[0-9]{4}") %>% 
  paste0("y", .) %>% 
  setNames(landcover, .)


#create prediction surface using landcover buffered neighborhood
#to confine the prediction to the study area

# buffer to create neighborhood (10 km by 10 km) and (10km by 10 km) grids
# 

neighborhood_radius <- 5000

agg_factor <- round(2 * neighborhood_radius / res(landcover))
agg_factor
#a template raster with cells equal in size to the neighborhoods


r <- raster(landcover) %>% 
  aggregate(agg_factor)  

cv_cov1 <- read_sf("data/central_valley_aq.shp") 
r <- cv_cov1 %>% 
  
  st_transform(crs = projection(r)) %>% 
  rasterize(r, field = 1) %>% 
  # remove any empty cells at edges
  trim()

#load cv boundary and crop elev 
cv <-read_sf("data/gis-data.gpkg", "cv") %>% 
  # project to the cdl projection
  st_transform(crs = st_crs(ebird_ss_count_year))





# crop, buffer cv_cov by 10 km to provide a little wiggly room
cv_extent <- cv %>% 
  st_buffer(dist = 10000) 


cv_extent <-read_sf("data/gis-data.gpkg", "cv") %>% 
  st_buffer(dist = 10000)
grid <- st_make_grid(cv_extent, cellsize = c(25000, 25000))

# Clip the grid to the polygon
grid_clipped <- st_intersection(grid, cv_extent) %>%  #unique(pts$cell): This extracts the unique cell identifiers from the pts dataset.
  st_as_sf() %>% 
  # project to the cdl projection
  st_transform(crs = st_crs(ebird_ss_count_year)) %>%
  #This converts the hexagonal grid polygons to an sf object.
  #The resulting pts and hexagon objects will contain the sampled points and the hexagonal grid polygons
  transmute(id = row_number()) 
cell_buff_25 <- NULL
for (yr in 2011:2020) {
  
  r_cells <- grid_clipped %>% 
    mutate(year=as.character(yr), year_lc = paste0("y", yr)) %>%
    rename(geometry=x)
  cell_buff_25 <- bind_rows(cell_buff_25, r_cells)
  # nest by year to  match to land cover data from the corresponding year
}
cell_buff_25 <- nest(cell_buff_25, data = c(year, id, geometry)) #separately for each year. 

#plot check the built grid

ggplot() +
  geom_sf(data = regions, colour = "red", fill = NA ) +
  theme_bw()

# Create an sf data frame from training data
ebird_ss_count_25 <- ebird_ss_count %>% st_as_sf(coords = c("longitude", "latitude"), crs = projection(r_cells)) 



aggre_extract_pred <- NULL
for (yr in names(landcover)) {
  # Assuming 'geometry' is a list column in ebird_ss_count_year
  ebird_ss_count_year<- ebird_ss_count_25 %>% filter(year==substr(yr, 2,6)) %>% select(-year)%>%
    
    st_sf()
  # get the buffered checklists for a given year
  regions <- cell_buff_25$data[[which(yr == cell_buff_25$year_lc )]]
  
  count<-st_join(ebird_ss_count_year, regions, join=st_within)
  
  
  # count the number of each landcover class for each checklist buffer
  count_sum <- count %>% 
    group_by(day_of_year, week, protocol_type, id) %>%
    
    summarize(year=as.numeric(substr(yr, 2,6)),
              #id = id[1],  # Use the first value of 'id' within each group
              observation_count = sum(observation_count, na.rm = TRUE),
              time_observations_started = mean(time_observations_started, na.rm = TRUE), 
              duration_minutes = sum(duration_minutes, na.rm = TRUE), 
              effort_distance_km = sum(effort_distance_km, na.rm = TRUE), 
              number_observers = sum(number_observers, na.rm = TRUE),
              grain = mean(grain, na.rm = TRUE), alfalfa = mean(alfalfa, na.rm = TRUE),
              pasture = mean(pasture, na.rm = TRUE), barren = mean(barren, na.rm = TRUE),
              perennial = mean(perennial, na.rm = TRUE), water = mean(water, na.rm = TRUE),
              developed = mean(developed, na.rm = TRUE), forest = mean(forest, na.rm = TRUE),
              shrub = mean(shrub, na.rm = TRUE), semi_permanent_wetlands = mean(semi_permanent_wetlands, na.rm = TRUE),
              field_row = mean(field_row, na.rm = TRUE), seasonal_wetlands = mean(seasonal_wetlands, na.rm = TRUE),
              corn = mean(corn, na.rm = TRUE), rice = mean(rice, na.rm = TRUE),
              perennial_snow = mean(perennial_snow, na.rm = TRUE),
              elevation_median = mean(elevation_median, na.rm = TRUE),
              elevation_sd = sd(elevation_sd, na.rm = TRUE),
              northness_median = mean(northness_median, na.rm = TRUE),
              northness_sd = sd(northness_sd, na.rm = TRUE),
              eastness_median = mean(eastness_median, na.rm = TRUE),
              eastness_sd = sd(eastness_sd, na.rm = TRUE),
              canopy = mean(canopy, na.rm = TRUE)) 
  
  # bind to results
  aggre_extract_pred <- bind_rows(aggre_extract_pred, count_sum)
  
}

pred_coords <- NULL
for (yr in 2011:2020) {
  pred_year <- aggre_extract_pred %>% filter(year == as.character(yr))
  pred_coords_year <- grid_clipped %>% 
    st_centroid() %>%
    rename(geometry=x)%>%
    st_coordinates() %>%
    as.data.frame() %>%
    cbind(id = grid_clipped$id) %>% 
    rename(longitude = X, latitude = Y) %>% 
    inner_join(pred_year, by = "id")%>%
    select(-geometry)
  pred_coords <- bind_rows(pred_coords, pred_coords_year)
}

#random split data into 0.8 and 0.2 for training and testing model respectively
ebird_split_count_25  <- pred_coords %>% as_tibble%>%
  select(-id)%>%
  split(if_else(runif(nrow(.)) <= 0.8, "train", "test")) 
map_int(ebird_split_count_25, nrow)
# -----------------------------------------------------
# creating  data structure for 25km scale species distribution models
# ----------------------------------------------------------------
train.data.time1 <- NULL      #add a time column make a time column to count from the first day to the last day of the 10 years
for (i in 2011:2020) {
  if (i>=2011 & i<=2012) {
    train.data.time1.year <-ebird_split_count_25$train %>% 
      filter(year == i)  %>% 
      mutate(time = day_of_year + (i-2011)*365)
  } else if (i>2012 & i<=2016) {
    train.data.time1.year <-ebird_split_count_25$train %>% 
      filter(year == i)  %>% 
      mutate(time = day_of_year + (i-2011)*365 +1)
  } else {
    train.data.time1.year <-ebird_split_count_25$train %>% 
      filter(year == i)  %>% 
      mutate(time = day_of_year + (i-2011)*365 +2)
  }
  
  
  train.data.time1 <- bind_rows(train.data.time1, train.data.time1.year)
}



range01 <- function(x){(x-min(x))/(max(x)-min(x))} #function to normalize longitude, latitude, time

train.data1 <- train.data.time1%>% 
  mutate(x=range01(longitude), y=range01(latitude), t=range01(time)) %>% 
  select(-longitude, -latitude, -time) 

train.locations1 <- train.data.time1 %>% dplyr::select(longitude, latitude, time) %>% 
  mutate(x=range01(longitude), y=range01(latitude), t=range01(time)) %>% 
  select(-longitude, -latitude, -time) 

# ------------------------------------------
# repeat STEM Data structures for 25km test data
# ------------------------------------------
test.locs1 <- NULL
for (i in 2011:2020) {
  if (i>=2011 & i<=2012) {
    test.locs1.year <-ebird_split_count_25$test %>% 
      filter(year == i) %>% 
      mutate(time = day_of_year + (i-2011)*365)
  } else if (i>2012 & i<=2016) {
    test.locs1.year <-ebird_split_count_25$test %>% 
      filter(year == i) %>% 
      mutate(time = day_of_year + (i-2011)*365 +1)
  } else {
    test.locs1.year <-ebird_split_count_25$test %>% 
      filter(year == i) %>% 
      mutate(time = day_of_year + (i-2011)*365 +2)
  }
  
  
  test.locs1 <- bind_rows(test.locs1, test.locs1.year)
}

test.data1 <- test.locs1 %>% 
  mutate(x=range01(longitude), y=range01(latitude), t=range01(time)) %>% 
  select(-longitude, -latitude, -time) 

test.locations1 <- test.locs1 %>% dplyr::select(longitude, latitude, time) %>% 
  mutate(x=range01(longitude), y=range01(latitude), t=range01(time)) %>% 
  select(-longitude, -latitude, -time) 
#-------------------------------------------------
#The data is ready for training the model in the next step
#------------------------------------------------------------