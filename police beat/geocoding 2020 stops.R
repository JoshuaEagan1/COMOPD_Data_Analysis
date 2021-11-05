#setwd("D:/Honors Thesis/data")
df<-read.csv("https://www.como.gov/wp-content/uploads/2021/06/CPD_vehicle_stop_data_2020-6.csv")

library(ggmap)
library(Rcpp)

register_google(key="SECRET")

#begin geocoding

add<-as.character(df$address)
for (i in 1:length(add)){
        add[i]<-paste(add[i], " Columbia, MO", sep=",")
}

df$address<-add
rm(add)

GC<-1

for(i in 1:100){
        temp<-geocode(df$address[i]) #Geocode each location and store it in temp
        df$lon[i]<-temp$lon           #Assign the lon
        df$lat[i]<-temp$lat           #Assign the lat
        GC<-GC+1
}

for (i in 101:7772){
        df$lon[i]<-NA
        df$lat[i]<-NA  
}

save("df", "GC", file="E:/Police_Work_2021/police beat/GC.R")

#################################################################################

#continue geocoding

load(file="E:/Police_Work_2021/police beat/GC.R")
library(ggmap)
library(Rcpp)
register_google(key="SECRET")

Go.to<-GC+7671

for(i in GC:Go.to){
        temp<-geocode(df$address[i]) #Geocode each location and store it in temp
        df$lon[i]<-temp$lon           #Assign the lon
        df$lat[i]<-temp$lat           #Assign the lat
        GC<-GC+1
}

save("df", "GC" , file="E:/Police_Work_2021/police beat/GC.R")

table(is.na(df$lat))