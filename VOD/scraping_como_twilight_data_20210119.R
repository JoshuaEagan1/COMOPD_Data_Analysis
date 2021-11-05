#scrape the twilight data for como
#this file was last ran on January 19,2021

#loading libraries
library(rvest)
library(tidyverse)
library(lubridate)

setwd("E:/CPD data dashboard/data")

#grabbing time and date info
#https://www.timeanddate.com/sun/usa/columbia-mo?month=1&year=2014

yearRange<-2014:2020
twilight<-c()

for(i in 2014:2020){
        for (j in 1:12){
                fileurl<-paste0("https://www.timeanddate.com/sun/usa/columbia-mo?month=", j, "&year=", i)
                text<-fileurl %>% read_html() %>% html_nodes(".sep-l , .sep, th") %>% html_text()
                text<-text[28:length(text)]
                yrmnth<-data.frame(rep(i, length(text)/13),
                                   rep(month.abb[j], length(text)/13),          
                                   text[((0:((length(text)/13)-1))*13)+1],
                                   text[((0:((length(text)/13)-1))*13)+2],
                                   text[((0:((length(text)/13)-1))*13)+3],
                                   text[((0:((length(text)/13)-1))*13)+4],
                                   text[((0:((length(text)/13)-1))*13)+5],
                                   text[((0:((length(text)/13)-1))*13)+6],
                                   text[((0:((length(text)/13)-1))*13)+7],
                                   text[((0:((length(text)/13)-1))*13)+8],
                                   text[((0:((length(text)/13)-1))*13)+9],
                                   text[((0:((length(text)/13)-1))*13)+10],
                                   text[((0:((length(text)/13)-1))*13)+11],
                                   text[((0:((length(text)/13)-1))*13)+12],
                                   text[((0:((length(text)/13)-1))*13)+13]
                )
                names(yrmnth)<-c("year", "month", "day", "sunrise", "sunset", "dayLength", "dayLengthDiff",
                                 "astronomicalTwilightStart", "astronomicalTwilightEnd", "nauticalTwilightStart",
                                 "nauticalTwilightEnd", "civilTwilightStart", "civilTwilightEnd", 
                                 "solarNoonTime", "SolarNoonMilMi")
                twilight<-rbind(twilight, yrmnth)
                Sys.sleep(1)
        }
}

twilight<-twilight[,c(1,2,3,4,5,12,13)]

#twilight_cache<-twilight
twilight<-twilight_cache

#making the time twilight variables:
twilight<-twilight %>% mutate(sunrise=gsub("(m).*", "\\1", sunrise),
                              sunset=gsub("(m).*", "\\1", sunset))

twilight<-twilight %>% mutate(civilTwilightStart=strptime(civilTwilightStart, "%I:%M %p"),
                      civilTwilightEnd=strptime(civilTwilightEnd, "%I:%M %p"),
                      sunrise=strptime(sunrise, "%I:%M %p"),
                      sunset=strptime(sunset, "%I:%M %p"))

save("twilight", file="como_twilight_data_20210919.rda")