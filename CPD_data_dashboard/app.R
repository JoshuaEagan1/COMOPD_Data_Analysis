#COMOPD Traffic Stops Shiny App

###############################################################################################

#packages
library(sf)
library(tidyverse)
library(Rcpp)
library(sp)
library(rgdal)
library(leaflet)
library(RColorBrewer)
library(leaflet)
library(shiny)
library(shinydashboard)

##############################################################################

#step 1: create the basic UI, get a basic leaflet (no map layers) embedded into each tab.
#step 2: begin writing the server. Embed the data processing into the app.
#step 3: build the leaflets. One for stops by race, one for search hit rates, and one for infraction type
#step 4: allow for filtering, build in reactivity button
#step 5: make UI pretty
#step 6: allow for a data download

#header
header <- dashboardHeader(
        title = 'COMOPD Data'
        # task list for status of data processing
        , dropdownMenuOutput('task_menu'))

# Side bar
sidebar <- dashboardSidebar(
        sidebarMenu(
                id = 'menu_tabs'
                , menuItem("Filters", tabName = "filters")
                , menuItem("Hit Rates", tabName = "hit_rates")
                , menuItem("Demographics", tabName = "by_race")
                , menuItem('Download the Raw Data', tabName = 'rawdata')
                
        )
)

#Body
body <- dashboardBody(
        tabItems(
                
                #filter tab
                tabItem(

                                tabName = 'filters'
                        ,tags$h1("Filter the Data")
                        ,tags$h6("For each category, you can remove different types of motorists from the
                                maps by unchecking their corresponding box. For an example, if you would 
                                like to see only stops of non-white motorists, uncheck the box labeled White.")

                        ,tags$h3("Demographics")
                        ,checkboxInput("white_internal", "White")
                        ,checkboxInput("black_internal", "Black")
                        ,checkboxInput("hispanic_internal", "Hispanic")
                        ,checkboxInput("asian_internal", "Asian")
                        ,checkboxInput("native_american_internal", "Native American")
                        ,checkboxInput("male_internal", "Male")
                        ,checkboxInput("female_internal", "Female")
                        ,checkboxInput("under_18_internal", "Under 18 Years Old")
                        ,checkboxInput("under_29_internal", "18 to 29 Years Old")
                        ,checkboxInput("under_39_internal", "30 to 39 Years Old")
                        ,checkboxInput("above_40_internal", "40 Years Old or Older")
                        
                        ,tags$h3("Reason for Stop")
                        ,checkboxInput("speed_internal", "Speeding")
                        ,checkboxInput("lane_violation_internal", "Lane Violation")
                        ,checkboxInput("follow_to_close_internal", "Follow to Close")
                        ,checkboxInput("fail_to_signal_internal", "Fail to Signal")
                        ,checkboxInput("equipment_internal", "Equipment Violation")
                        ,checkboxInput("license_internal", "License")
                        
                        ,tags$h3("Result of Stop")
                        ,checkboxInput("search_internal", "Search")
                        ,checkboxInput("citation_internal", "Citation")
                        ,checkboxInput("warning_internal", "Warning")
                        ,checkboxInput("driver_arrest_internal", "Arrest")
                        
                        ,tags$h3("Apply Filters")
                        ,actionButton("filter_button", "GO"))
                
                #hit rates tab
                ,tabItem(
                        tabName = 'hit_rates'
                        ,tags$h3("Hit Rates by Census Tract")
                        ,leafletOutput("map1"))
                
                
                #stops by race tab
                ,tabItem(
                        tabName = 'by_race'
                        ,tags$h3("Stop Demographics by Census Tract")
                        ,leafletOutput('map2')
                )
                
                ,tabItem(tabName = "rawdata"
                         , box(width = 8, title="Download the Raw Data"
                         , tags$h6("Click the link below to download the traffic stops data used to generate
                                   these maps. The output file will be a .csv file, openable by excel, as well as
                                   statistical programs like R and STATA.")
                        ,downloadLink("COMOPDTrafficStopsData.csv", "Download CSV")))
                
                #
                #,tabItem(
                #        tabName = 'by_infraction'
                #        , leafletOutput('map')
                #)
        )
)

server <- function(input, output, session) {
        
        #importing the app data
        load("./data/app_data.R")
        
        observeEvent(input$filter_button, {
                
        ### filtering the stops data
        if(!(input$white_internal|input$black_internal|input$hispanic_internal|input$asian_internal|
           input$native_american_internal|input$male_internal|input$female_internal|
           input$under_18_internal|input$under_29_internal|input$under_39_internal|input$above_40_internal|
           input$speed_internal|input$lane_violation_internal|input$follow_to_close_internal|
           input$fail_to_signal_internal|input$equipment_internal|input$license_internal|
           input$search_internal|input$citation_internal|input$warning_internal|
           input$driver_arrest_internal)){
                print("yes")
                filterer<-rep(T, nrow(stops))
        } else {
                filterer<-rep(F, nrow(stops)) 
                if(input$white_internal){filterer[stops$race=="W"]<-T}
                if(input$black_internal){filterer[stops$race=="B"]<-T}
                if(input$hispanic_internal){filterer[stops$race=="H"]<-T}
                if(input$asian_internal){filterer[stops$race=="A"]<-T}
                if(input$native_american_internal){filterer[stops$race=="I"]<-T}
                if(input$male_internal){filterer[stops$gender=="M"]<-T}
                if(input$female_internal){filterer[stops$gender=="F"]<-T}
                if(input$under_18_internal){filterer[stops$age=="Under 18"]<-T}
                if(input$under_29_internal){filterer[stops$age=="18--29"]<-T}
                if(input$under_39_internal){filterer[stops$age=="30--39"]<-T}
                if(input$above_40_internal){filterer[stops$age=="40+"]<-T}
                if(input$speed_internal){filterer[stops$speed=="1"]<-T}
                if(input$lane_violation_internal){filterer[stops$lane_violation=="1"]<-T}
                if(input$follow_to_close_internal){filterer[stops$follow_to_close=="1"]<-T}
                if(input$fail_to_signal_internal){filterer[stops$fail_to_signal=="1"]<-T}
                if(input$equipment_internal){filterer[stops$equipment=="1"]<-T}
                if(input$license_internal){filterer[stops$license=="1"]<-T}
                if(input$search_internal){filterer[stops$what_searched!=""]<-T}
                if(input$citation_internal){filterer[stops$citation=="1"]<-T}
                if(input$warning_internal){filterer[stops$warning=="1"]<-T}
                if(input$driver_arrest_internal){filterer[stops$driver_arrest=="YES"]<-T}
        }
        
        filtered_stops<-stops %>% filter(filterer)
        
        ###
        
        #aggregate the stops
        grouped_stops<- filtered_stops %>% group_by(GEOID20) %>%
                summarise(hit_rate=mean(contraband_found=="Yes")/mean(!what_searched==""), number_of_searches=sum(what_searched!=""),
                          White=mean(race=="W"), Black=mean(race=="B"), 
                          OtherRace=mean(race!="W"&race!="B"), Male=mean(gender=="M"), 
                          Female=mean(gender=="F"), Below_30=mean(age %in% c("18--29", "Under 18")),
                          Above_29=mean(age %in% c("40+", "30--39")),
                          propNonwhite=mean(race!="W"), total=n())
        
        #merging hit rates back in
        tracts<-tracts %>% filter(GEOID20 %in% grouped_stops$GEOID20)
        grouped_stops<-grouped_stops %>% as.data.frame() %>% select(-13)
        grouped_stops<-merge(tracts, grouped_stops)
        grouped_stops<-st_transform(grouped_stops, "+init=epsg:4326")
        
        
        #making the leaflets
        
        #making the hit rates popup
        hit_rate_popup <- paste0("<strong>Census Tract Identifier: </strong>", 
                              paste0(grouped_stops$GEOID20), 
                              "<br><strong>Percentage of Searches Discovering Contraband: </strong>", 
                              paste0(round(grouped_stops$hit_rate, 3)*100, "%"),
                              "<br><strong>Total Number of Searches: </strong>", 
                              paste0(grouped_stops$number_of_searches))
        
        #making the color ramp
        pal1<-colorNumeric(palette = c("red","green"), domain=grouped_stops$hit_rate)
        
        #compiling hit rates leaflet
        output$map1<-renderLeaflet({
                l<-leaflet(data = grouped_stops) %>% addTiles() %>%
                        addPolygons(color = "#444444", 
                                    weight=1,
                                    fillColor = pal1(grouped_stops$hit_rate), 
                                    popup=hit_rate_popup,
                                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                        bringToFront = TRUE))
                l %>% addLegend(position="bottomright", 
                                pal=pal1,
                                values=~hit_rate,
                                title = paste('Search Hit Rate by Census Tract'))
                })
        
        #race age gender
        
        #making the popup
        RAG_popup <- paste0("<strong>Census Tract Identifier: </strong>", 
                              paste0(grouped_stops$GEOID20), 
                              "<br><strong>% White Drivers Stopped: </strong>", 
                              paste0(round(grouped_stops$White*100, 2), "%"),
                              "<br><strong>% Black Drivers Stopped: </strong>", 
                              paste0(round(grouped_stops$Black*100, 2), "%"),
                              "<br><strong>% Other Race Drivers Stopped: </strong>", 
                              paste0(round(grouped_stops$OtherRace*100, 2), "%"),
                              "<br><strong>% Male Drivers Stopped: </strong>", 
                              paste0(round(grouped_stops$Male*100, 2), "%"),
                              "<br><strong>% Female Drivers Stopped: </strong>", 
                              paste0(round(grouped_stops$Female*100, 2), "%"),
                              "<br><strong>% Drivers Under the Age of 30 Stopped: </strong>", 
                              paste0(round(grouped_stops$Below_30*100, 2), "%"),
                              "<br><strong>% Drivers Over the Age of 29 Stopped: </strong>", 
                              paste0(round(grouped_stops$Above_29*100, 2), "%"),
                              "<br><strong>Number of Drivers Stopped: </strong>" ,
                              paste0(grouped_stops$total)
        )
        
        #making the color ramp
        pal2<-colorNumeric(palette = c("white","black"), domain=grouped_stops$propNonwhite)
        
        #compiling hit rates leaflet
        output$map2<-renderLeaflet({
                l<-leaflet(data = grouped_stops) %>% addTiles() %>%
                        addPolygons(color = "#444444", 
                                    weight=1,
                                    fillColor = pal2(grouped_stops$propNonwhite), 
                                    popup=RAG_popup,
                                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                        bringToFront = TRUE))
                l %>% addLegend(position="bottomright", 
                                pal=pal2,
                                values=~propNonwhite,
                                title = paste('Proportion Non-White Stopped'))
        })
        
        }) #end observe event button
        
        output$COMOPDTrafficStopsData.csv <- downloadHandler(
                filename = "COMOPDTrafficStopsData.csv",
                content = function(file){write.csv(download, file)}
        )
        
}#end server

ui <- dashboardPage(
        header,
        sidebar,
        body
)

shinyApp(ui, server)
