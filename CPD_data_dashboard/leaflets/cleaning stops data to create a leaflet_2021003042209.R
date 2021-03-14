#cleaning the data to make a leaflet of policing data

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

stops<-st_join(stops, tracts, join = st_within) #%>% select(-c(59:61, 63:70))

#caching tracts
restore_tracts<-tracts

#making leaflets

#################################################################################################

#hit rates
hit_rates_2019<-stops %>% filter(!what_searched=="" & year=="2019") %>% group_by(GEOID20) %>%
        summarise(hit_rate=mean(contraband_found=="Yes"), number_of_searches=n())

#merging hit rates back in
tracts<-tracts %>% filter(GEOID20 %in% hit_rates_2019$GEOID20)
hit_rates_2019<-hit_rates_2019 %>% as.data.frame() %>% select(1:3)
hit_rates_2019<-merge(tracts, hit_rates_2019)
hit_rates_2019 <- st_transform(hit_rates_2019, "+init=epsg:4326")

#dropping tract 29107090601 which was included by an error
hit_rates_2019<-hit_rates_2019 %>% filter(GEOID20 !="29107090601")

#making the leaflet

#making the popup
tract_popup <- paste0("<strong>Census Tract Identifier: </strong>", 
                      paste0(hit_rates_2019$GEOID20), 
                      "<br><strong>Percentage of Searches Discovering Contraband: </strong>", 
                      paste0(round(hit_rates_2019$hit_rate, 3)*100, "%"),
                      "<br><strong>Total Number of Searches: </strong>", 
                      paste0(hit_rates_2019$number_of_searches)
)

#making the color ramp
library(RColorBrewer)
pal<-colorNumeric(palette = c("red","green"), domain=hit_rates_2019$hit_rate)

l<-leaflet(data = hit_rates_2019) %>% addTiles() %>%
        addPolygons(color = "#444444", 
                    weight=1,
                    fillColor = pal(hit_rates_2019$hit_rate), 
                    popup=tract_popup,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE))
l %>% addLegend(position="bottomright", 
                pal=pal,
                values=~hit_rate,
                title = paste('Search Hit Rate by Census Tract in 2019'))



##########################################################################################

tracts<-restore_tracts

#race, Age, and gender (proportion non-white) 2014-2019
RAG_all_years<-stops %>% group_by(GEOID20) %>%
        summarise(White=mean(race=="W"), Black=mean(race=="B"), 
                  OtherRace=mean(race!="W"&race!="B"), Male=mean(gender=="M"), 
                  Female=mean(gender=="F"), Below_30=mean(age %in% c("18--29", "Under 18")),
                  Above_29=mean(age %in% c("40+", "30--39")),
                  propNonwhite=mean(race!="W"), total=n())

#merging hit rates back in
tracts<-tracts %>% filter(GEOID20 %in% RAG_all_years$GEOID20)
RAG_all_years<-RAG_all_years %>% as.data.frame() %>% select(1:10)
RAG_all_years<-merge(tracts, RAG_all_years)
RAG_all_years <- st_transform(RAG_all_years, "+init=epsg:4326")

#dropping tract 29107090601 which was included by an error
RAG_all_years<-RAG_all_years %>% filter(GEOID20 !="29107090601")

#making the leaflet

#making the popup
tract_popup <- paste0("<strong>Census Tract Identifier: </strong>", 
                      paste0(RAG_all_years$GEOID20), 
                      "<br><strong>% White Drivers Stopped: </strong>", 
                      paste0(round(RAG_all_years$White*100, 2), "%"),
                      "<br><strong>% Black Drivers Stopped: </strong>", 
                      paste0(round(RAG_all_years$Black*100, 2), "%"),
                      "<br><strong>% Other Race Drivers Stopped: </strong>", 
                      paste0(round(RAG_all_years$OtherRace*100, 2), "%"),
                      "<br><strong>% Male Drivers Stopped: </strong>", 
                      paste0(round(RAG_all_years$Male*100, 2), "%"),
                      "<br><strong>% Female Drivers Stopped: </strong>", 
                      paste0(round(RAG_all_years$Female*100, 2), "%"),
                      "<br><strong>% Drivers Under the Age of 30 Stopped: </strong>", 
                      paste0(round(RAG_all_years$Below_30*100, 2), "%"),
                      "<br><strong>% Drivers Over the Age of 29 Stopped: </strong>", 
                      paste0(round(RAG_all_years$Above_29*100, 2), "%"),
                      "<br><strong>Number of Drivers Stopped: </strong>", 
                      paste0(RAG_all_years$total)
)

#making the color ramp
library(RColorBrewer)
pal<-colorNumeric(palette = c("white","black"), domain=RAG_all_years$propNonwhite)

l<-leaflet(data = RAG_all_years) %>% addTiles() %>%
        addPolygons(color = "#444444", 
                    weight=1,
                    fillColor = pal(RAG_all_years$propNonwhite), 
                    popup=tract_popup,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE))
l %>% addLegend(position="bottomright", 
                pal=pal,
                values=~propNonwhite,
                title = paste('Proportion of non-white Drivers Stopped (2014-2019)'))


