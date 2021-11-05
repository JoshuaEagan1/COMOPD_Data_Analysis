#creating variables for census block and census tract

#loading packages
library(sf)
library(dplyr)
library(ggmap)
library(Rcpp)
library(sp)
library(openxlsx)

#setting working directory
setwd("E:/Police_Work_2021/police beat")

register_google(key="SECRET")

#starting by creating the census blocks variable
shape<-st_read(dsn="E:/Police_Work_2021/police beat/tigerline data/tl_2021_29_tabblock20/tl_2021_29_tabblock20.shp",
               stringsAsFactors=FALSE, geometry_column="geometry", quiet=TRUE)
#shape@proj4string

#Importing geocoded traffic stops...
load(file="GC.R")
rm(GC)
coords_2020<-df %>% select(inci_id, lon, lat)
rm(df)
coords_2014_through_2019<-read.csv("coords_2014_through_2019.csv")
coords_2014_through_2019<-coords_2014_through_2019 %>% select(-1)
names(coords_2014_through_2019)[1]<-"inci_id"
coords<-rbind(coords_2014_through_2019, coords_2020)
rm(coords_2014_through_2019, coords_2020)

#cleaning spatial data and coordinate reference systems...
pointsSP<-SpatialPointsDataFrame(coords[-1], proj4string=CRS("+proj=longlat +datum=WGS84"), coords)
pointsSP<- spTransform(pointsSP, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
geoC<-pointsSP%>%st_as_sf(crs="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
rm(pointsSP)
st_crs(coords)<-st_crs(shape)

#intersection...
st_crs(shape)==st_crs(geoC)

intersect<- st_intersects(geoC, shape)
block <- shape[intersect[[1]],]
block$NAME20
rm(block)

int<-data.frame(intersect)

n_occur <- data.frame(table(int$row.id))
n_occur[n_occur$Freq > 1,]
int[int$row.id %in% n_occur$Var1[n_occur$Freq > 1],]

int<-slice(int, -c(25699, 53168, 54104, 74803))

blocks<-c(int$col.id)
rm(int)

#adding text variable and merging...

table(blocks)

uniqueBlock<-unique(blocks)
blockshp<-slice(shape, uniqueBlock)

#GEOID20-15 characters in this order:
# two characters: State ID
# three characters: County ID
# six characters: Census Tract ID
# four characters: block ID

GEOID20<-c()
for (i in 1:length(blocks)){
        row_position<-blocks[i]
        GEOID20[i]<-shape$GEOID20[row_position]
}
#ran instantly
length(table(GEOID20))

#adding geographic info to coords
coords$GEOID20<-GEOID20
rm(GEOID20)
coords<-coords %>% mutate(county=substr(GEOID20, 3, 5),
                  tract=substr(GEOID20, 6, 11),
                  block=substr(GEOID20, 12, 15))

rm(geoC, intersect, n_occur, shape, blocks, i, row_position, uniqueBlock)
####################################################################################

#Creating a beat variable using 2014-1018 and 2020 beat data

#importing beat data
beat_and_division_2014_2017<-read.xlsx("beat_and_division_2014_2017.xlsx")
beat_and_division_2014_2017<-beat_and_division_2014_2017 %>% select(Incident_Number, CPD_Beat)
names(beat_and_division_2014_2017)<-c("inci_id", "beat")
CPD_Beatdata_2018<-read.xlsx("CPD_Beatdata_2018.xlsx")
CPD_Beatdata_2018<-CPD_Beatdata_2018 %>% select(inci_id, CPD_Beat)
names(CPD_Beatdata_2018)[2]<-"beat"
data20<-read.csv("https://www.como.gov/wp-content/uploads/2021/06/CPD_vehicle_stop_data_2020-6.csv")
data20<-data20 %>% select(inci_id, beat)
beat<-rbind(beat_and_division_2014_2017, CPD_Beatdata_2018, data20)
rm(beat_and_division_2014_2017, CPD_Beatdata_2018, data20)

#importing stops data and pruning away 2019 from blocks data

geo<-merge(coords, beat, all.x=T)
rm(coords, beat)

#standardizing the beat variable (writing code to write code to do this)
corrections<-c("80", "MUPD", "70", "70D", "40", "OTHER", "60", "50", "20", "10", "10", "10", "30", "10", "10", "20", "OTHER", 
  "10", "10", "10", "10", "10", "60", "OTHER", "10", "OTHER", "10", "50", "10", "10", "50", "60", "80", "20", "10",
  "30", "30", "40", "80", "50", "40", "20", "60", "10", "10", "10", "10", "30", "10" , "30" , "20" , "50", "60" ,
  "40" , "80" , "20" , "10" , "80" , "50" , "OTHER", "MUPD", "60" , "70" , "MUPD", "40" , NA, "80")
cat("test<-geo %>% mutate(beat=case_when(",
       paste0("beat==\"", unique(geo$beat), "\" ~ \"", corrections, "\"", collapse=",
              "),
        ", 
    T ~ NA))")
geo<-geo %>% mutate(beat = case_when( beat=="80" ~ "80",
                       beat=="MUPD" ~ "MUPD",
                       beat=="70" ~ "70",
                       beat=="70D" ~ "70D",
                       beat=="40" ~ "40",
                       beat=="111" ~ "OTHER",
                       beat=="60" ~ "60",
                       beat=="50" ~ "50",
                       beat=="20" ~ "20",
                       beat=="10" ~ "10",
                       beat=="103" ~ "10",
                       beat=="104" ~ "10",
                       beat=="30" ~ "30",
                       beat=="112" ~ "10",
                       beat=="106" ~ "10",
                       beat=="210" ~ "20",
                       beat==" " ~ "OTHER",
                       beat=="105" ~ "10",
                       beat=="113" ~ "10",
                       beat=="114" ~ "10",
                       beat=="110" ~ "10",
                       beat=="115" ~ "10",
                       beat=="62" ~ "60",
                       beat=="CPD" ~ "OTHER",
                       beat=="150" ~ "10",
                       beat=="CITY" ~ "OTHER",
                       beat=="107" ~ "10",
                       beat=="506" ~ "50",
                       beat=="101" ~ "10",
                       beat=="170" ~ "10",
                       beat=="50E" ~ "50",
                       beat=="60E" ~ "60",
                       beat=="80S" ~ "80",
                       beat=="20W" ~ "20",
                       beat=="10E" ~ "10",
                       beat=="30S" ~ "30",
                       beat=="30N" ~ "30",
                       beat=="40E" ~ "40",
                       beat=="80N" ~ "80",
                       beat=="50W" ~ "50",
                       beat=="40W" ~ "40",
                       beat=="20E" ~ "20",
                       beat=="60W" ~ "60",
                       beat=="10W" ~ "10",
                       beat=="109" ~ "10",
                       beat=="102" ~ "10",
                       beat=="108" ~ "10",
                       beat=="30N " ~ "30",
                       beat=="10E " ~ "10",
                       beat=="30S " ~ "30",
                       beat=="20W " ~ "20",
                       beat=="50W " ~ "50",
                       beat=="60E " ~ "60",
                       beat=="40W " ~ "40",
                       beat=="80N " ~ "80",
                       beat=="20E " ~ "20",
                       beat=="10W " ~ "10",
                       beat=="80S " ~ "80",
                       beat=="50E " ~ "50",
                       beat=="    " ~ "OTHER",
                       beat=="MUG2" ~ "MUPD",
                       beat=="60W " ~ "60",
                       beat=="70D " ~ "70",
                       beat=="MUG1" ~ "MUPD",
                       beat=="40E " ~ "40",
                       beat=="80A " ~ "80"))

#Creating newbeat
ct<-unique(geo$GEOID20)
newbeat_assignments<-c()

mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
}

newbeat_assignments<-c()
for (i in 1:length(ct)){
        newbeat_assignments[i]<-geo$beat[geo$GEOID20==ct[i] & !is.na(geo$beat)] %>%
                mode()
}

ct<-data.frame(newbeat_assignments, ct)
names(ct)[2]<-"GEOID20"
geo<-merge(geo, ct, all.x=T)
names(geo)[9]<-"newbeat"

#testing newbeat for accuracy

table(geo$beat)
table(geo$newbeat)
table(geo$beat==geo$newbeat)

tbl<-table(geo$newbeat, geo$beat)
library(stats)
heatmap(tbl)

16421/(16421 + 60033)
#error rate- .21

####################################################################################

save(geo, file="E:/Police_Work_2021/police beat/geo_data.R")