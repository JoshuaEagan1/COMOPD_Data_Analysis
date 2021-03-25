#cleaning the data for the COMOPD Traffic Stops Shiny App

#cleaning the data to make a shiny app of policing data

#packages
library(sf)
library(tidyverse)
library(Rcpp)
library(sp)
library(rgdal)
library(leaflet)
library(RColorBrewer)

#loading stops data
load("E:/Police_Work_2021/standardized_traffic_stops_data_20210218.R")

#loading census blocks shapefile
#zip file containing this map layer can be downloaded at:
#https://www2.census.gov/geo/tiger/TIGER2020PL/STATE/29_MISSOURI/29/tl_2020_29_tract20.zip
tracts<-readOGR(dsn="E:/Police_Work_2021/CPD_data_dashboard/data/tl_2020_29_tract20", layer="tl_2020_29_tract20")
tracts<-tracts%>%st_as_sf(crs="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

#edit later to create a single file to import geocoordinates with incident ID's

###

#importing geocoordinates for traffic stops
load(file="E:/Honors Thesis/R-files/R Data/last_year_coordinates.R")
load(file="E:/Honors Thesis/R-files/R Data/first_five_years_coordinates.R")
rm(df)
names(last_year_coordinates)[1]<-"Incident_Number"
points<-rbind(first_five_years_coordinates,last_year_coordinates)
rm(first_five_years_coordinates)
rm(last_year_coordinates)
rm(USmap)
rm(comomap)
names(points)[1]<-"inci_id"

###

#merging lat lon to stops data
stops<-merge(stops, points, all.x=T)

#dropping 1 observation with no lat and lon
stops<-stops %>% filter(!is.na(lat))

#making the sf dataframe
pointsSP<-SpatialPointsDataFrame(stops[,59:60], stops[,-(59:60)], proj4string=CRS("+proj=longlat +datum=WGS84"))
cord.lonlat<- spTransform(pointsSP, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
stops<-cord.lonlat%>%st_as_sf(crs="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
rm(cord.lonlat)
rm(points)
rm(pointsSP)

stops<-st_join(stops, tracts, join = st_within) 
stops<-stops %>% filter(GEOID20 !="29107090601")

#making a cleaner version of the data to download
download<-stops %>% as.data.frame() %>% select(-c(59:71)) 

save(download, stops, tracts, file="E:/Police_Work_2021/CPD_data_dashboard/data/app_data.R")