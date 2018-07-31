library(tidyverse)
library(sf)
library(raster)
library(rgdal)
library(tmap)
library(tmaptools)
library(leaflet)
library(ggpomological)
library(scales)

options(scipen = 999)

## Loading in data ------------------------------------------------------------
df_17 <- read_csv('data-raw/2017-fordgobike-tripdata.csv.csv')

df_18_1 <- read_csv('data-raw/201801-fordgobike-tripdata.csv') %>%
  dplyr::select(-bike_share_for_all_trip)

df_18_2 <- read_csv('data-raw/201802-fordgobike-tripdata.csv') %>%
  dplyr::select(-bike_share_for_all_trip)

df_18_3 <- read_csv('data-raw/201803-fordgobike-tripdata.csv') %>%
  dplyr::select(-bike_share_for_all_trip)

df_18_4 <- read_csv('data-raw/201804-fordgobike-tripdata.csv') %>%
  dplyr::select(-bike_share_for_all_trip)

df_18_5 <- read_csv('data-raw/201805-fordgobike-tripdata.csv') %>%
  dplyr::select(-bike_share_for_all_trip)

df_18_6 <- read_csv('data-raw/201806-fordgobike-tripdata.csv') %>%
  dplyr::select(-bike_share_for_all_trip)

## Joining data together ------------------------------------------------------
all_data <- rbind(df_17, df_18_1, df_18_2, df_18_3, df_18_4, df_18_5, df_18_6)
rm(df_17, df_18_1, df_18_2, df_18_3, df_18_4, df_18_5, df_18_6)

## Reading in SF shapefile to attach neighborhoods to data --------------------
sf <- readOGR('sf_neighborhoods/planning_neighborhoods.shp')
names(sf)[1] <- 'neighborhood'

newShape <- spTransform(sf, CRS('+init=epsg:4326'))

# Start station neighborhoods
start <- all_data %>%
  rename(X = 'start_station_longitude', Y = 'start_station_latitude')

startmap <- start
coordinates(startmap) <- c('X', 'Y')
crs(startmap) <- '+init=epsg:4326'

StartTest <- over(startmap, newShape)

start$neighborhood <- StartTest$neighborhood


# End station neighborhoods
end <- all_data %>%
  rename(X = 'end_station_longitude', Y = 'end_station_latitude')

endmap <- end
coordinates(endmap) <- c('X', 'Y')
crs(endmap) <- '+init=epsg:4326'

EndTest <- over(endmap, newShape)

end$neighborhood <- EndTest$neighborhood

# Removing files from environment
rm(endmap)
rm(newShape)
rm(sf)
rm(startmap)

## Reading in shapefile as dataframe -------------------------------------------
sf_neighborhoods <- st_read('sf_neighborhoods/planning_neighborhoods.shp', stringsAsFactors = FALSE) %>%
  rename(neighborhood = 'neighborho')

## Start map ------------------------------------------------------------------
start_counts <- start %>%
  group_by(neighborhood) %>%
  summarise(count = n()) %>%
  filter(!is.na(neighborhood))

start_counts <- full_join(start_counts, sf_neighborhoods)
  

ggplot(start_counts) +
  geom_sf(aes(fill = count)) +
  scale_fill_gradient(high = "#132B43", low = "#56B1F7", labels = comma) +
  ggtitle('In which neighborhoods do bikeshare rides start?') +
  labs(fill = '# of Rides Started') +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

## End map --------------------------------------------------------------------
end_counts <- end %>%
  group_by(neighborhood) %>%
  summarise(count = n()) %>%
  filter(!is.na(neighborhood))

end_counts <- full_join(end_counts, sf_neighborhoods)

ggplot(end_counts) +
  geom_sf(aes(fill = count)) +
  scale_fill_gradient(high = "#421313", low = "#f75555", labels = comma) +
  ggtitle('In which neighborhoods do bikeshare rides end?') +
  labs(fill = '# of Rides Ended') +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())
