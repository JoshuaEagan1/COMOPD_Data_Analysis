#loading and cleaning CPD calls data

#Joshua Eagan
#Work on this project began on January 19, 2021

#loading libraries
library(tidyverse)

#reading in the traffic stops data

#URL's and data was loaded from CPD's website https://www.como.gov/police/data-reporting-forms/ on January 19, 2021

#2014
data14<-read.csv("https://www.como.gov/wp-content/uploads/2020/10/CPD-vehicle-stop-data-2014.csv")

#2015
data15<-read.csv("https://www.como.gov/wp-content/uploads/2020/10/CPD-vehicle-stop-data-2015.csv")

#2016
data16<-read.csv("https://www.como.gov/wp-content/uploads/2020/10/CPD_vehicle_stop_data_2016.csv")

#2017
data17<-read.csv("https://www.como.gov/wp-content/uploads/2020/10/CPD_vehicle_stop_data_2017.csv")

#2018
data18<-read.csv("https://www.como.gov/wp-content/uploads/2020/10/CPD_vehicle_stop_data_2018-2.csv")

#2019
data19<-read.csv("https://www.como.gov/wp-content/uploads/2020/10/CPD_vehicle_stop_data_2019.csv")

#2020
data20<-read.csv("https://www.como.gov/wp-content/uploads/2021/06/CPD_vehicle_stop_data_2020-6.csv")

setwd("E:/Police_Work_2021")

#in case something goes wrong and data is no longer available
#save("data14", "data15", "data16", "data17", "data18", "data19", file="data backup_20210119.R")

#edits to make it easier to work with tables across years:
#       make all varbs lowercase
#       change all . to _

#load("data_backup_20210119.R")

#standardizing column names (. to _ and making names all lower case)
for(i in 14:20){
        obj<-get(paste0("data", i))
        n_vect<-tolower(names(get(paste0("data", i))))
        names(obj)<-gsub("[.]", "_", n_vect)
        assign(paste0("data", i), obj)
}
rm(i, obj, n_vect)

#names list
n<-list(names(data14), names(data15), names(data16), names(data17), names(data18), names(data19))

#all variable names are the same for 14-15
setdiff(names(data14), names(data15))
setdiff(names(data15), names(data14))

#there was a change in naming conventions between 15-16
setdiff(names(data15), names(data16))
setdiff(names(data16), names(data15))

#all variable names are the same for 16-17
setdiff(names(data16), names(data17))
setdiff(names(data17), names(data16))

#another change in naming conventions between 17-18
setdiff(names(data17), names(data18))
setdiff(names(data18), names(data17))

#another change in naming conventions between 18-19
setdiff(names(data18), names(data19))
setdiff(names(data19), names(data18))

#another change in naming conventions between 19-20
setdiff(names(data19), names(data20))
setdiff(names(data20), names(data19))

#changing some names...
names(data14)[5]<-"time_"
names(data15)[5]<-"time_"

data_14_17<-rbind(data14, data15, data16, data17)

#attaching 2018 to 2019
names(data19)[24]<-"what_searched"
names(data19)[49]<-"hour"
#reordering the 2019 data to combine with the 2018 data
data19<-data19 %>% select(names(data18)[1:51]) %>% 
        mutate(special_op=rep(NA, nrow(data19)),
               admit_prior_use=rep(NA, nrow(data19)))
data_18_19<-rbind(data18, data19)

#removing excess data
rm(list=(paste0("data", 14:19)))
rm(n)

#changes from 2017-2018:
#date_ and time_ from 2017 is combined into a datetime object in 2018
#resident added in 2018
#[25] "unk" and [26] "cancel_no_stop" available in 2017, 2018 has other_result
#2018 has [52] "special_op" and [53] "admit_prior_use"

setdiff(names(data_14_17), names(data_18_19))
setdiff(names(data_18_19), names(data_14_17))

#combining date and time columns in data_14_17
data_14_17<-data_14_17 %>% mutate(calltime=paste0(date_, " ", time_)) %>%
        select(-c(4,5))

#changing column names in data_14_17 to names in data_18_19.
names(data_14_17)[which(names(data_14_17) %in% setdiff(names(data_14_17), names(data_18_19)))]<-
        c("inci_id",          "address",          "hour",             "age",              "gender",          
          "follow_to_close",  "investigative",    "no_action",        "unk",       "cancel_no_stop",    
          "suspicion_weapon", "drug_dog",         "drug_alcohol",     "currency",         "driver_arrest",   
          "warrant")

data_14_17<- data_14_17 %>% mutate(resident=rep(NA, nrow(data_14_17)), 
                                   other_result=rep(NA, nrow(data_14_17)), 
                                   special_op=rep(NA, nrow(data_14_17)), 
                                   admit_prior_use=rep(NA, nrow(data_14_17)))

data_18_19<- data_18_19 %>% mutate(unk=rep(NA, nrow(data_18_19)),
                                   cancel_no_stop=rep(NA, nrow(data_18_19)))
data_18_19$calltime<-paste0(data_18_19$calltime, ":00")

stops<-rbind(data_14_17, data_18_19)

rm(data_14_17, data_18_19)

#standardizing each of the variables, beginning with the time variable

stops$calltime<-strptime(stops$calltime, format="%m/%e/%Y %H:%M:%S")

stops<- stops %>% mutate(year=lubridate::year(calltime), 
                         day=lubridate::day(calltime),
                         time=strftime(calltime, format="%H:%M:%S"),
                         month=lubridate::month(calltime))

#writting a function to break down different coding schemes by year in order to standardize
# variables accross years.


checker<-function(varbname){table(as.vector(t(stops[names(stops)==varbname])), stops$year, useNA="always")}

checker("hour")
#checking against the calltime variable
table(as.numeric(stops$hour)==as.character(stops$time) %>% str_sub(1, 2) %>% as.numeric(), useNA="always")

names(stops)
checker("dow")
checker("day_night")
#go back and improve this with time & date data

checker("race")
checker("age")

#fixing age variable
stops$age<-stops$age %>% as.character()
stops$age[stops$age=="<18"]<-"Under 18"
stops$age[stops$age==">40"]<-"40+"
stops$age[stops$age=="18-29"]<-"18--29"
stops$age[stops$age=="30-39"]<-"30--39"
stops$age[stops$age %in% c("#VALUE!", "")]<-NA

checker("gender")
stops$gender<-stops$gender %>% as.character()
stops$gender[stops$gender==""]<-"U"

checker("location")
#come back to location. This variable is unreliable, I may end up fixing it using code I wrote for
#my honors thesis. This would rely a set of rules to assign this variable based off of the address
#variable.

checker("speed")
stops$speed<-stops$speed %>% as.character()
stops$speed[stops$speed=="2"]<-"1"
stops$speed[is.na(stops$speed)]<-"0"

checker("lane_violation")
stops$lane_violation<-stops$lane_violation %>% as.character()
stops$lane_violation[stops$lane_violation=="2"]<-"1"
stops$lane_violation[is.na(stops$lane_violation)]<-"0"

checker("follow_to_close")
stops$follow_to_close<-stops$follow_to_close %>% as.character()
stops$follow_to_close[is.na(stops$follow_to_close)]<-"0"

for(i in 14:24){
        print(checker(names(stops)[i]))
}

#making a loop to make 2 or higher into 1 and NA into 0
for(i in 14:24){
        varbname<-names(stops)[i]
        stops[,names(stops)==varbname]<-as.character(stops[,names(stops)==varbname])
        col_to_fix<-as.vector(t(stops[names(stops)==varbname]))
        col_to_fix[col_to_fix>1]<-1
        col_to_fix[is.na(col_to_fix)]<-0
        stops[,names(stops)==varbname]<-col_to_fix
}

#drop the variable cancel_no_stop, it only contains NAs for all years
checker("cancel_no_stop")
stops<-stops %>% select(-cancel_no_stop)

checker("what_searched")
stops$what_searched<-stops$what_searched %>% as.character()
stops$what_searched[is.na(stops$what_searched)]<-""
stops$what_searched[stops$what_searched=="BOTH"]<-"Driver and Property"
stops$what_searched[stops$what_searched=="CAR"]<-"Property ONLY"
stops$what_searched[stops$what_searched=="DRIVER"]<-"Driver ONLY"

checker("consent")

for(i in 26:33){
        print(checker(names(stops)[i]))
}

#making another loop to make 2 or higher into 1 and NA into 0
for(i in 26:33){
        varbname<-names(stops)[i]
        stops[,names(stops)==varbname]<-as.character(stops[,names(stops)==varbname])
        col_to_fix<-as.vector(t(stops[names(stops)==varbname]))
        col_to_fix[col_to_fix>1]<-1
        col_to_fix[is.na(col_to_fix)]<-0
        stops[,names(stops)==varbname]<-col_to_fix
}

#search_duration
checker("search_duration")
stops$search_duration<-stops$search_duration %>% as.character()
stops$search_duration[stops$search_duration==" "]<-""
stops$search_duration[stops$search_duration=="0"]<-""
stops$search_duration[stops$search_duration=="N"]<-""
stops$search_duration[is.na(stops$search_duration)]<-""
stops$search_duration[stops$search_duration=="1"]<-"0--15"
stops$search_duration[stops$search_duration=="0-15"]<-"0--15"
stops$search_duration[stops$search_duration=="2"]<-"16--30"
stops$search_duration[stops$search_duration=="3"]<-"31+ minutes"

checker("contraband_found")
stops$contraband_found<-stops$contraband_found %>% as.character()
stops$contraband_found[stops$contraband_found==" "]<-""
stops$contraband_found[stops$contraband_found=="`"]<-""
stops$contraband_found[is.na(stops$contraband_found)]<-""
stops$contraband_found[stops$contraband_found %in% c("B","M")]<-""
stops$contraband_found[stops$contraband_found %in% c("B","M")]<-""
stops$contraband_found[stops$contraband_found %in% c("N","N1","NN","No","NQ")]<-"NONE"
stops$contraband_found[stops$contraband_found %in% c("Y","YCON","YCUR","YOTH","YSP", "YWEA")]<-"Yes"

#restart on drug_alcohol variable, number 36
checker("drug_alcohol")
stops$drug_alcohol<-stops$drug_alcohol %>% as.character()
stops$drug_alcohol[stops$drug_alcohol=="2"]<-"1"
stops$drug_alcohol[is.na(stops$drug_alcohol)]<-"0"

for(i in 37:40){
        print(checker(names(stops)[i]))
}

#making another loop to make 2 or higher into 1 and NA into 0
for(i in 37:40){
        varbname<-names(stops)[i]
        stops[,names(stops)==varbname]<-as.character(stops[,names(stops)==varbname])
        col_to_fix<-as.vector(t(stops[names(stops)==varbname]))
        col_to_fix[col_to_fix>1]<-1
        col_to_fix[is.na(col_to_fix)]<-0
        stops[,names(stops)==varbname]<-col_to_fix
}

checker("driver_arrest")
stops$driver_arrest<-stops$driver_arrest %>% as.character()
stops$driver_arrest[stops$driver_arrest %in% c("No", "")]<-"NO"
stops$driver_arrest[is.na(stops$driver_arrest)]<-"NO"
stops$driver_arrest[stops$driver_arrest %in% c("Yes", "Y")]<-"YES"

checker("warrant")
stops$warrant<-stops$warrant %>% as.character()
stops$warrant[!stops$warrant %in% c("1", "0")]<-"1"

for(i in 43:49){
        print(checker(names(stops)[i]))
}

#making another loop to make 2 or higher into 1 and NA into 0
for(i in 43:49){
        varbname<-names(stops)[i]
        stops[,names(stops)==varbname]<-as.character(stops[,names(stops)==varbname])
        col_to_fix<-as.vector(t(stops[names(stops)==varbname]))
        col_to_fix[col_to_fix>1]<-1
        col_to_fix[is.na(col_to_fix)]<-0
        stops[,names(stops)==varbname]<-col_to_fix
}

#resident only filled post 2017
checker("resident")
stops$resident<-stops$resident %>% as.character()
stops$resident[stops$resident=="No"]<-"N"
stops$resident[stops$resident=="Yes"]<-"Y"

#other_result only filled post 2017
checker("other_result")

#special_op only filled in 2018
checker("special_op")

#admit_prior_use only filled in 2018
checker("admit_prior_use")
stops$admit_prior_use<-stops$admit_prior_use %>% as.character()
stops$admit_prior_use[stops$admit_prior_use=="No"]<-"NO"
stops$admit_prior_use[stops$admit_prior_use=="Yes"]<-"YES"

#adding the data from 2020
setdiff(names(stops), names(data20))
setdiff(names(data20), names(stops))

names(data20)[c(13, 47)]<-c("arrest_???", "hour")

data20$calltime<-strptime(data20$calltime, format="%m/%d/%Y %H:%M")
data20<- data20 %>% mutate(year=lubridate::year(calltime), 
                         day=lubridate::day(calltime),
                         time=strftime(calltime, format="%H:%M:%S"),
                         month=lubridate::month(calltime))

na_20<-data.frame(rep(NA, nrow(data20)),
                  rep(NA, nrow(data20)),
                  rep(NA, nrow(data20)),
                  rep(NA, nrow(data20)),
                  rep(NA, nrow(data20)),
                  rep(NA, nrow(data20)),
                  rep(NA, nrow(data20)),
                  rep(NA, nrow(data20)),
                  rep(NA, nrow(data20)),
                  rep(NA, nrow(data20)))
names(na_20)<- c("speed", "lane_violation", "follow_to_close", "fail_to_signal", "cve", "other", "unk", "what_searched", "special_op", "admit_prior_use")
data20<-cbind(data20, na_20)

na_stops<-data.frame(rep(NA, nrow(stops)),
                     rep(NA, nrow(stops)),
                     rep(NA, nrow(stops)),
                     rep(NA, nrow(stops)),
                     rep(NA, nrow(stops)))
names(na_stops)<-c("beat", "arrest_???", "search", "drug", "alcohol")
stops<-cbind(stops, na_stops)

setdiff(names(stops), names(data20))
setdiff(names(data20), names(stops))

stops<-rbind(stops, data20)

#checking the merge
checker(names(stops)[63])

#changing nature
stops$nature[stops$nature=="TRAFFIC STOP"]<-"T TRFC STOP"

#changing age
stops$age[stops$age=="40--64"|stops$age=="65+"]<-"40+"

#changing moving
stops$moving[stops$moving>1]<-1

#changing citation
stops$citation[stops$citation>1]<-1

#changing warning
stops$warning[stops$warning>1]<-1

#changing search_duration
stops$search_duration[stops$search_duration=="10"|stops$search_duration=="0-15 minutes"]<-"0--15"

#changing controband found
stops$contraband_found[is.na(stops$contraband_found)]<-""
stops$contraband_found[stops$contraband_found=="YSTL"]<-"Yes"

#implement fix on location variable

#*generating the new Location variable: loc
loc<-rep("city", nrow(stops))

#replace loc ="state"
loc[grepl("GRINDSTONE PKWY|COLLEGE AV|STRAWN|BALLENGER|WW|HIGHWAY KK|LOOP|HIGHWAY 763|CLARK|ROUTE K|ROUTE B|SOUTHEAST|SOUTHWEST|I70 DR SOUTHEAST", stops$address)]<-"state"
loc[grepl("BROADWAY", stops$address) & !grepl("WEST BLVD|TENTH|NINTH|EIGHTH|SEVENTH|SIXTH|FIFTH|FOURTH|THIRD|SECOND|FIRST|OLD 63|WILLIAM|WILLIS|WAUGH|TRIMBLE|HITT|GARTH|BLUFFS|ROCKINGHAM|PERSHING|DORSEY|SHORT|RIPLEY|ANN|HINKSON AV|MCBAINE|CLINKSCALES|GLENWOOD|EDGEWOOD|BRIARWOOD|ALDEAH|GREENWOOD", stops$address)]<-"state"
loc[grepl("SCOTT", stops$address) & !grepl("BROOKVIEW|COPPERSTONE|VAWTER SCHOOL RD|PRESCOTT", stops$address)]<-"state"
loc[grepl("PARIS", stops$address) & !grepl("MONROE ST|WILKES BLVD|HINKSON AV|WILLIAM ST|AMMONETTE ST|GORDON ST|ANN ST N|COURT ST|PARIS CT", stops$address)]<-"state"
loc[grepl("PROVIDENCE", stops$address) & !grepl("VANDIVER|BLUE RIDGE|TEXAS AV|SMILEY LN|ARMADILLO DR|BIG BEAR BLVD|BROWN SCHOOL RD|RAIN FOREST PKWY|LESLIE LN|1612 PROVIDENCE RD N-CO.|1704 PROVIDENCE RD N-CO.|1704 PROVIDENCE RD N-CO|LESLIE LN", stops$address)]<-"state"
loc[grepl("STADIUM", stops$address) & !grepl("OLD 63")]<-"state"
loc[grepl("RANGE LINE", stops$address) & !grepl("ROGERS ST|WILKES BLVD|SMITH ST")]<-"state"

#replace loc ="i70"
loc[grepl("I70", stops$address) & !grepl("I70 DR", stops$address)]<-"i70"

#replace loc ="US"
loc[grepl("HIGHWAY 63", stops$address) & !grepl("OLD HIGHWAY 63|I70|HWY 63", stops$address)]<-"US"

#replace loc = "boone"
loc[grepl("SCOTT BLVD S-BC/THORNBROOK RDG-BC.|MERIDETH|BELLVIEW DR|AIRPORT DR|5695 CLARK LN|CLARK LN E-BC/LAKEWOOD DR|BELLVIEW DR", stops$address)]<-"boone"
loc[grepl("LAKE OF THE WOODS", stops$address) & !grepl("EXIT", stops$address)]<-"boone"

stops<- stops %>% mutate(newLocation=loc)

###
cache<-stops
stops<-cache
###

#adding variables for `intertwilight` and `cdark`
load("como_twilight_data_20210919.rda")
stops<-stops %>% mutate(month=as.character(as.numeric(month)))
stops$month<-month.abb[as.numeric(stops$month)]
stops<-merge(stops, twilight)

#makes all times relative to the same day: 2021-10-10
#this is for comparison to the sunlight data
stops<-stops %>% mutate(comp_time=strptime(time, format="%H:%M:%S"))
stops$comp_time<-stops$comp_time-as.difftime(Sys.Date()-as.Date("2021-10-10"))

max_ctwi_e<-max(stops$civilTwilightEnd)
min_ctwi_e<-min(stops$civilTwilightEnd)
max_ctwi_s<-max(stops$civilTwilightStart)
min_ctwi_s<-min(stops$civilTwilightStart)
max_sunr<-max(stops$sunrise)
min_sunr<-min(stops$sunrise)
max_suns<-max(stops$sunset)
min_suns<-min(stops$sunset)

stops<-stops %>% mutate(cdark=case_when((comp_time>sunrise)&(comp_time<sunset)~0, T~1),
                        intertwilight=case_when(((comp_time>min_ctwi_s)&(comp_time<max_ctwi_s))|((comp_time>min_ctwi_e)&(comp_time<max_ctwi_e))~1,T~0),
                        twilight=case_when(((comp_time>civilTwilightStart)&(comp_time<sunrise))|((comp_time<civilTwilightEnd)&(comp_time>sunset))~1,T~0))
table(stops$twilight, stops$intertwilight)

#merging in geography information
load("E:/Police_Work_2021/police beat/geo_data.R")
geo<-geo %>% select(-8)
stops<-merge(stops, geo)
stops<-stops %>% select(-c(65:69))

#saving clean data
save("stops", file="standardized_traffic_stops_data_20211014.R")
