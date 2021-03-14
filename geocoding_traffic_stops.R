#use this file to geocode the "stops" dataset
#file written on 02/19/2021

#instructions: 

#run lines 10:40 to begin geocoding. 

#run lines 46:62 to continue geocoding until all rows are finished.

load("E:/CPD data dashboard/data/standardized traffic stops data_20210218.R")

library(ggmap)
library(Rcpp)

#insert google key
register_google(key="")

#begin geocoding
add<-as.character(stops$address)
for (i in 1:length(add)){
        add[i]<-paste(add[i], " Columbia, MO", sep=",")
}

stops$address<-add
rm(add)

GC<-1

for(i in 1:100){
        temp<-geocode(stops$address[i]) #Geocode each location and store it in temp
        stops$lon[i]<-temp$lon           #Assign the lon
        stops$lat[i]<-temp$lat           #Assign the lat
        GC<-GC+1
}

for (i in 101:68682){
        stops$lon[i]<-NA
        stops$lat[i]<-NA  
}

save("stops", "GC", file="E:/CPD data dashboard/data/geocoding traffic stops/GC.R")

#################################################################################

#continue geocoding

load(file="E:/CPD data dashboard/data/geocoding traffic stops/GC.R")
library(ggmap)
library(Rcpp)
register_google(key="AIzaSyCJ2R_1YoJVkGpiFPzUCxIAOB6Th1J6q3I")

Go.to<-GC+2500

for(i in GC:Go.to){
        temp<-geocode(stops$address[i]) #Geocode each location and store it in temp
        stops$lon[i]<-temp$lon           #Assign the lon
        stops$lat[i]<-temp$lat           #Assign the lat
        GC<-GC+1
}

save("stops", "GC" , file="E:/CPD data dashboard/data/geocoding traffic stops/GC.R")

table(is.na(df$lat))

#run the rest of the script whenever you are finished geocoding all the observations

#fix the bottom part of the script if you plan on rerunning it.

#fixing bad obs...

latlong2state <- function(pointsDF) {
        # Prepare SpatialPolygons object with one SpatialPolygon
        # per state (plus DC, minus HI & AK)
        states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
        IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
        states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                         proj4string=CRS("+proj=longlat +datum=WGS84"))
        
        # Convert pointsDF to a SpatialPoints object 
        pointsSP <- SpatialPoints(pointsDF, 
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))
        
        # Use 'over' to get _indices_ of the Polygons object containing each point 
        indices <- over(pointsSP, states_sp)
        
        # Return the state names of the Polygons object containing each point
        stateNames <- sapply(states_sp@polygons, function(x) x@ID)
        stateNames[indices]
}

load(file="E:/CPD data dashboard/data/geocoding traffic stops/GC.R")
rm("GC")

bad_vect<-c()
for(i in 1:68682){
        if(is.na(df$lon[i])){
                bad_vect[i]<-1
        } else if(!(df$lon[i] > -92.44376 | df$lon[i]< -92.22404)|(df$lat[i]<38.86608 | df$lat[i]>39.03696)){
                bad_vect[i]<-1
        } else {
                bad_vect[i]<-0
        }
}

df[140]<-bad_vect
names(df)[140]<-"bad"
rm("bad_vect")

df$ADDRAPT<-sub("-CO/", " ", df$ADDRAPT)
df$ADDRAPT<-sub("-CO", " ", df$ADDRAPT)
df$ADDRAPT<-sub("E-BC/", "", df$ADDRAPT)

table(df$bad)

for(i in 1:68682){
        if(df$bad[i]==1){
                temp<-geocode(df$ADDRAPT[i]) #Geocode each location and store it in temp
                df$lon[i]<-temp$lon           #Assign the lon
                df$lat[i]<-temp$lat           #Assign the lat
        }
}
rm(temp, i)



comomap<-qmap("Columbia, Missouri", zoom = 12)
USmap<-qmap("USA", zoom = 4)

USmap+geom_point(data=subset(df, df$bad==1), aes(lon,lat))

bad_vect<-c()
for(i in 1:68682){
        if(is.na(df$lon[i])){
                bad_vect[i]<-1
        } else if(!(df$lon[i] > -92.44376 | df$lon[i]< -92.22404)|(df$lat[i]<38.86608 | df$lat[i]>39.03696)){
                bad_vect[i]<-1
        } else {
                bad_vect[i]<-0
        }
}

df<-df[-140]

df[140]<-bad_vect
names(df)[140]<-"bad"
rm("bad_vect")

df$ADDRAPT<-sub("ST ,", "ST,", df$ADDRAPT)
df$ADDRAPT<-sub("HIGHWAY 63 N", "HIGHWAY 63 N,", df$ADDRAPT)
df$ADDRAPT<-sub("/", ", ", df$ADDRAPT)
df$ADDRAPT<-sub("SE.BLK", "", df$ADDRAPT)
df$ADDRAPT<-sub("3855 3855-", "3855 ", df$ADDRAPT)
df$ADDRAPT<-sub("1244 1244", "1244", df$ADDRAPT)
df$ADDRAPT<-sub("4100 4100-", "4100 ", df$ADDRAPT)
df$ADDRAPT<-sub("5695 5695-I", "5695", df$ADDRAPT)
df$ADDRAPT<-sub("1283 1283", "1283", df$ADDRAPT)
df$ADDRAPT<-sub("5700 5700-", "5700 ", df$ADDRAPT)
df$ADDRAPT<-sub("1283 1283", "1283", df$ADDRAPT)
df$ADDRAPT<-sub("1275 1275", "1275", df$ADDRAPT)
df$ADDRAPT<-sub("1269 1269", "1269", df$ADDRAPT)
df$ADDRAPT<-sub("3822 3822", "3822", df$ADDRAPT)
df$ADDRAPT<-sub("4000 4000-", "4000 ", df$ADDRAPT)
df$ADDRAPT<-sub("1293 1293", "1293", df$ADDRAPT)
df$ADDRAPT<-sub("1303 1303", "1303", df$ADDRAPT)
df$ADDRAPT<-sub("., ", ", ", df$ADDRAPT)
df$ADDRAPT<-sub(" ., ", ", ", df$ADDRAPT)
df$ADDRAPT<-sub(" ,", ",", df$ADDRAPT)

table(df$bad==1)

for(i in 1:68682){
        if(df$bad[i]==1){
                temp<-geocode(df$ADDRAPT[i]) #Geocode each location and store it in temp
                df$lon[i]<-temp$lon           #Assign the lon
                df$lat[i]<-temp$lat           #Assign the lat
        }
}
rm(temp, i)

USmap+geom_point(data=subset(df, df$bad==1), aes(lon,lat))
comomap+geom_point(data=subset(df, df$bad==1), aes(lon,lat))
table(latlong2state(subset(df, df$bad==1)[138:139]))

for(i in 1:68682){
        if(df$Incident_Number[i]==2017050509){
                df$ADDRAPT[i]<-"220 N 10th St, Columbia, MO 65201"
                temp<-geocode(df$ADDRAPT[i]) #Geocode each location and store it in temp
                df$lon[i]<-temp$lon           #Assign the lon
                df$lat[i]<-temp$lat           #Assign the lat
        }
}

bad_vect<-c()
statevect<-latlong2state(df[138:139])
for(i in 1:68682){
        if(is.na(statevect[i])){
                bad_vect[i]<-1
        }
        else if(statevect[i]=="missouri"){
                bad_vect[i]<-0
        } else {
                bad_vect[i]<-1
        }
}

df<-df[-140]

df[140]<-bad_vect
names(df)[140]<-"bad"
rm("bad_vect", statevect)

bad<-subset(df, df$bad==1)

USmap+geom_point(data=subset(df, df$bad==1), aes(lon,lat))

###get here then decide how to proceed...
old<-c(
        "1285 I70, Columbia, MO",
        "ROUTE K S-B, LAKE FOREST DR E-BC, Columbia, MO",
        "HIGHWAY 163 S-B, ROUTE K S-BC, Columbia, MO",
        "1812 I70 DR SOUTHWEST, Columbia, MO",
        "HIGHWAY 163 S-B, ROUTE K S-BC., Columbia, MO",
        "WILDFLOWER CT W-B, DEAD END-BC., Columbia, MO",
        "1500 I70 DR SOUTHWEST, Columbia, MO",
        "ROUTE K S-B, LAKE FOREST DR E-BC., Columbia, MO",
        "2201 BLK I70 DR NORTHWEST, Columbia, MO",
        "1285 I70 W, Columbia, MO",
        "1510 I70 DR SOUTHWEST, Columbia, MO",
        "1800 I70 DR SOUTHWEST, Columbia, MO",
        "1700 BLK HIGHWAY 63, NB, Columbia, MO",
        "1800 I70 DR SOUTHWEST, Columbia, MO",
        "1800 I70 DR SOUTHWEST, Columbia, MO",
        "1800 I70 DR SOUTHWEST, Columbia, MO",
        "1800 I70 DR SOUTHWEST, Columbia, MO",
        "1800 BLK HIGHWAY 63, NB, Columbia, MO",
        "1230 BLK HIGHWAY 63 CONNECTOR SB, Columbia, MO",
        "FLANDERS CT HANOVER BLVD, Columbia, MO",
        "1510 I70 DR SOUTHWEST, Columbia, MO",
        "1510 I70 DR SOUTHWEST, Columbia, MO",
        "1244 I70 W, Columbia, MO",
        "1244 I70 W, Columbia, MO",
        "HIGHWAY 163 S-B, ROUTE K S-BC., Columbia, MO",
        "1900 BLK HIGHWAY 63, SB, Columbia, MO",
        "1500 I70 DR SOUTHWEST, Columbia, MO",
        "1279 I70 E, Columbia, MO",
        "2010 I70 DR SOUTHWEST, Columbia, MO",
        "1500 I70 DR SOUTHWEST, Columbia, MO",
        "1500 I70 DR SOUTHWEST, Columbia, MO",
        "1273 I70 E, Columbia, MO",
        "1265 I70 E, Columbia, MO",
        "1800 I70 DR SOUTHWEST, Columbia, MO",
        "1800 I70 DR SOUTHWEST, Columbia, MO",
        "1000 BLK HIGHWAY 63 CONNECTOR NB, Columbia, MO",
        "1800 BLK I70 DR SOUTHWEST, Columbia, MO",
        "1800 BLK HIGHWAY 63, NB, Columbia, MO",
        "1800 BLK I70 DR SOUTHWES, Columbia, MO",
        "1500 1500-6 I70 DR SW., Columbia, MO",
        "1244 I70, Columbia, MO",
        "LANCASTER, S SIXTH ST, Columbia, MO",
        "1900 1900-BLK I70 DR SW.BL, Columbia, MO",
        "1303 I70, Columbia, MO",
        "1244 I70, Columbia, MO",
        "1244 I70, Columbia, MO",
        "1244 I70, Columbia, MO",
        "1283 I70, Columbia, MO",
        "1283 I70, Columbia, MO",
        "1275 I70, Columbia, MO",
        "1269 I70, Columbia, MO",
        "1293 I70, Columbia, MO"
)

new<-c(
        "1285 I-70 BL Columbia, MO 65201, United States",
        "E Lake Forest Dr Columbia, MO 65203, United States",
        "S Mont St Columbia, MO 65203, United States",
        "1812 I-70 Dr SW Columbia, MO 65203, United States",
        "S Mont St Columbia, MO 65203, United States",
        "W Wildflower Ct Columbia, MO 65203, United States",
        "1500 I70 DR SOUTHWEST, Columbia, MO",
        "E Lake Forest Dr Columbia, MO 65203, United States",
        "2201 I-70 Dr NW Columbia, MO 65202, United States",
        "1285 I-70 BL Columbia, MO 65202, United States",
        "1510 I-70 Dr SW Columbia, MO 65203, United States",
        "1800 I-70 Dr SW Columbia, MO 65203, United States",
        "N Old Hwy 63 Columbia, MO 65202, United States",
        "1800 I-70 Dr SWColumbia, MO 65203, United States",
        "1800 I-70 Dr SW Columbia, MO 65203, United States",
        "1800 I-70 Dr SW Columbia, MO 65203, United States",
        "1800 I-70 Dr SW Columbia, MO 65203, United States",
        "N Old Hwy 63 Columbia, MO 65202, United States",
        "1230 Old 63 N Columbia, MO 65201, United States",
        "Flanders Ct Columbia, MO 65202, United States",
        "1510 I-70 Dr SW Columbia, MO 65203, United States",
        "1510 I-70 Dr SW Columbia, MO 65203, United States",
        "1244 I-70 BL Columbia, MO 65202, United States",
        "1244 I-70 Dr SW Columbia, MO 65203, United States",
        "S Mont St Columbia, MO 65203, United States",
        "US-63 Columbia, MO 65201, United States",
        "1500 I-70 Dr SW Columbia, MO 65203, United States",
        "1279 I-70 BL Columbia, MO 65201, United States",
        "2010 I-70 Dr SW Columbia, MO 65203, United States",
        "1500 I-70 Dr SW Columbia, MO 65203, United States",
        "1500 I-70 Dr SW Columbia, MO 65203, United States",
        "1273 I-70 BL Columbia, MO 65201, United States",
        "1265 I-70 BL Columbia, MO 65201, United States",
        "1800 I-70 Dr SW Columbia, MO 65203, United States",
        "1800 I-70 Dr SW Columbia, MO 65203, United States",
        "US-63 S Columbia, MO 65201, United States",
        "1800 I-70 Dr SW Columbia, MO 65203, United States",
        "N Old Hwy 63 Columbia, MO 65202, United States",
        "1800 I-70 Dr SW Columbia, MO 65203, United States",
        "1500 I-70 Dr SW Columbia, MO 65203, United States",
        "1244 I-70 BL Columbia, MO 65202, United States",
        "Lancaster Dr Columbia, MO 65201, United States",
        "1900 I-70 BL Columbia, MO 65201, United States",
        "1303 I-70 BL Columbia, MO 65201, United States",
        "1244 I-70 BL Columbia, MO 65201, United States",
        "1244 I-70 BL Columbia, MO 65201, United States",
        "1244 I-70 BL Columbia, MO 65201, United States",
        "1283 I-70 BL Columbia, MO 65201, United States",
        "1283 I-70 BL Columbia, MO 65201, United States",
        "1275 I-70 BL Columbia, MO 65201, United States",
        "1269 I-70 BL Columbia, MO 65201, United States",
        "1293 I-70 BL Columbia, MO 65201, United States"
)

for (i in 1:52){
        df$ADDRAPT<-sub(old[i], new[i], df$ADDRAPT)
}

bad<-subset(df, df$bad==1)

for(i in 1:68682){
        if(df$bad[i]==1){
                temp<-geocode(df$ADDRAPT[i]) #Geocode each location and store it in temp
                df$lon[i]<-temp$lon           #Assign the lon
                df$lat[i]<-temp$lat           #Assign the lat
        }
}
rm(temp, i)

bad<-subset(df, df$bad==1)
bad

df$ADDRAPT<-sub("BL", "Buisness Loop", df$ADDRAPT)

for(i in 1:68682){
        if(df$Incident_Number[i]==2014047906){
                temp<-geocode(df$ADDRAPT[i]) #Geocode each location and store it in temp
                df$lon[i]<-temp$lon           #Assign the lon
                df$lat[i]<-temp$lat           #Assign the lat
        }
}

for(i in 1:68682){
        if(df$Incident_Number[i]==2018061552){
                temp<-geocode(df$ADDRAPT[i]) #Geocode each location and store it in temp
                df$lon[i]<-temp$lon           #Assign the lon
                df$lat[i]<-temp$lat           #Assign the lat
        }
}

for(i in 1:68682){
        if(df$Incident_Number[i]==2018057959){
                temp<-geocode(df$ADDRAPT[i]) #Geocode each location and store it in temp
                df$lon[i]<-temp$lon           #Assign the lon
                df$lat[i]<-temp$lat           #Assign the lat
        }
}

for(i in 1:68682){
        if(df$Incident_Number[i]==2018257014){
                temp<-geocode(df$ADDRAPT[i]) #Geocode each location and store it in temp
                df$lon[i]<-temp$lon           #Assign the lon
                df$lat[i]<-temp$lat           #Assign the lat
        }
}

for(i in 1:68682){
        if(df$Incident_Number[i]==2018115633){
                temp<-geocode(df$ADDRAPT[i]) #Geocode each location and store it in temp
                df$lon[i]<-temp$lon           #Assign the lon
                df$lat[i]<-temp$lat           #Assign the lat
        }
}

for(i in 1:68682){
        if(df$Incident_Number[i]==2018104915){
                temp<-geocode(df$ADDRAPT[i]) #Geocode each location and store it in temp
                df$lon[i]<-temp$lon           #Assign the lon
                df$lat[i]<-temp$lat           #Assign the lat
        }
}

bad_vect<-c()
for(i in 1:68682){
        if(is.na(df$lon[i])){
                bad_vect[i]<-1
        } else if(!(df$lon[i] > -92.44376 | df$lon[i]< -92.22404)|(df$lat[i]<38.86608 | df$lat[i]>39.03696)){
                bad_vect[i]<-1
        } else {
                bad_vect[i]<-0
        }
}
df<-df[-140]

df[140]<-bad_vect
names(df)[140]<-"bad"
rm("bad_vect")

bad<-subset(df, df$bad==1)
bad
#perfect!

comomap+geom_point(data=df, aes(lon, lat))
USmap+geom_point(data=df, aes(lon,lat))

df<-df[-140]

names(df)

first_five_years_coordinates<-df[c(2, 138,139)]
save("df", "first_five_years_coordinates" , file="E:/Honors Thesis/R-files/R Data/first_five_years_coordinates.R")
E:/Honors Thesis/R-files/R Data