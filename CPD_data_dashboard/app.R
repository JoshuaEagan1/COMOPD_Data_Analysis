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
library(shinycssloaders)

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
                , menuItem("How to Use this Tool", tabName = "How_to_Use_this_Tool")
                , menuItem("Filters", tabName = "filters")
                , menuItem("Hit Rates", tabName = "hit_rates")
                , menuItem("Demographics", tabName = "by_race")
                , menuItem("Reason for Stop", tabName = "by_reason")
                , menuItem('Download the Raw Data', tabName = 'rawdata')
                
        )
)

#Body
body <- dashboardBody(
        tabItems(
                
                #description tab
                tabItem(tabName = 'How_to_Use_this_Tool'
                        ,tags$h1("How to Use this Dashboard")
                        ,tags$h5("This app is intended to help you explore the publicly released data on traffic
                                stops from Columbia, Missouri's Police Department from 2014-2019. Stops are displayed at the
                                2020 Census tract level in the ", tags$b("Hit Rates by Census Tract"),
                                ", ", tags$b("Demographics"), ", and ", tags$b("Reason for Stop"), " tabs. The ", tags$b("GO")," button in the ", tags$b("filters"), " tab of this app recreates the
                                maps with the currently selected filters applied.")
                        ,tags$h3("Filtering the Data")
                        ,tags$h5("Users can apply filters to the data to restrict down to only stops with certain
                                 characteristics. For an example: if you would like to see maps of only white female 
                                 drivers who were stopped for speeding and arrested as a result of the stop, you would
                                 check the boxes for: ", tags$b("White"), ", " , tags$b("Female"), ", ", tags$b("Speeding"), ", and ", tags$b("Arrest"),
                                ". If you check two boxes for mutually explusive categories (Male and Female or warning and arrest) there will
                                 be no stops left to create the map. To apply filters to the data, click the ", tags$b("GO"), " button 
                                 in the ", tags$b("filters"), " tab of this app.")
                        ,tags$h3("Hit Rates")
                        ,tags$h5("This map shows the percentage of searches which led to finding contraband (illegal items
                        such as scheduled substances or unregistered weapons.) A low search hit rate can imply that police are 
                        oversearching motorists in that area of the city or of a certain demographic. Keep in mind that in areas
                        with a small number of stops might have an abnormally low or high hit rate due to statistical noise. Click
                        on each census tract to reveal it's numerical identifier, exact search hit rate, and the number of searches
                        conducted there. If the map isn't loading, revisit the ", tags$b("filters"), " tab and click the ", tags$b("GO"), " button.")
                        ,tags$h3("Demographics")
                        ,tags$h5("This map shows the demographics of stopped drivers in different areas of the Columbia. The shading
                        of each census tract represents the share of stopped drivers who are non-white in that area. Click on each census tract to 
                        reveal information about the drivers who were stopped there. If the map isn't loading, revisit the ", tags$b("filters"), " tab and click the ", tags$b("GO"), " button.")
                        ,tags$h3("Reason for Stop")
                        ,tags$h5("This map shows the reasons motorists were stopped in different areas of the Columbia. The purple shading
                        of each census tract represents the share of drivers stopped for equipment violations in that area. 
                        Keep in mind that drivers can be stopped for multiple reasons. Click on each census tract to 
                        see a breakdown of the reasons drivers were stopped there. If the map isn't loading, revisit the ",
                        tags$b("filters"), " tab and click the ", tags$b("GO"), " button.")
                        
                        ,tags$h3("Download the Raw Data")
                        ,tags$h5("Click the ", tags$b("Download CSV"), " button to download the raw data used to create this map as a .csv file. 
                        Your filters will not affect the downloaded copy of the data."))
        
                #filter tab
                ,tabItem(tabName = 'filters'
                        ,fluidRow(
                                column(width = 12
                        ,tags$h1("Filter the Data")
                        ,tags$h5("For each category, you can filter the stops data to include only stops where
                                 a selected attribute applies. Don't select mutually exclusive categories: there will be no stops left to make the maps.")))
                        , fluidRow(
                                column(4
                                        ,box(
                                title = "Demographics", width = 12, solidHeader = TRUE
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
                                ,checkboxInput("above_40_internal", "40 Years Old or Older"))
                                )
                                ,column(4
                        ,box(
                                title = "Reason for Stop", width = 12, solidHeader = TRUE
                                ,checkboxInput("speed_internal", "Speeding")
                                ,checkboxInput("lane_violation_internal", "Lane Violation")
                                ,checkboxInput("follow_to_close_internal", "Follow to Close")
                                ,checkboxInput("fail_to_signal_internal", "Fail to Signal")
                                ,checkboxInput("equipment_internal", "Equipment Violation")
                                ,checkboxInput("license_internal", "License"))
                        ,box(
                                title = "Apply Filters", width = 12, solidHeader = TRUE
                                ,actionButton("filter_button", "GO"))
                                )
                                ,column(4
                        ,box(
                                title = "Time Filter", width = 12, solidHeader = TRUE, color = "blue"
                        ,dateInput("date1", "Beginning Date:", value = "2014-01-01")
                        
                        # Default value is the date in client's time zone
                        ,dateInput("date2", "Ending Date:", value = "2019-12-31"))
                        
                        ,box(
                                title = "Result of Stop", width = 12, solidHeader = TRUE
                                ,checkboxInput("search_internal", "Search")
                                ,checkboxInput("citation_internal", "Citation")
                                ,checkboxInput("warning_internal", "Warning")
                                ,checkboxInput("driver_arrest_internal", "Arrest"))
                                )
                        )
                )
                
                #hit rates tab
                ,tabItem(
                        tabName = 'hit_rates'
                        ,tags$h3("Hit Rates by Census Tract")
                        ,leafletOutput("map1") %>% withSpinner(type=4))
                
                
                #stops by race tab
                ,tabItem(
                        tabName = 'by_race'
                        ,tags$h3("Stop Demographics by Census Tract")
                        ,leafletOutput('map2') %>% withSpinner(type=4)
                )
                
                #stops by reason tab
                ,tabItem(
                        tabName = 'by_reason'
                        ,tags$h3("Reason for Stop by Census Tract")
                        ,leafletOutput('map3') %>% withSpinner(type=4)
                )
                
                ,tabItem(tabName = "rawdata"
                         , box(width = 8, title="Download the Raw Data"
                         , tags$h5("Click the link below to download the traffic stops data used to generate
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
                filterer<-rep(T, nrow(stops)) 
                if(input$white_internal){filterer[stops$race!="W"]<-F}
                if(input$black_internal){filterer[stops$race!="B"]<-F}
                if(input$hispanic_internal){filterer[stops$race!="H"]<-F}
                if(input$asian_internal){filterer[stops$race!="A"]<-F}
                if(input$native_american_internal){filterer[stops$race!="I"]<-F}
                if(input$male_internal){filterer[stops$gender!="M"]<-F}
                if(input$female_internal){filterer[stops$gender!="F"]<-F}
                if(input$under_18_internal){filterer[stops$age!="Under 18"]<-F}
                if(input$under_29_internal){filterer[stops$age!="18--29"]<-F}
                if(input$under_39_internal){filterer[stops$age!="30--39"]<-F}
                if(input$above_40_internal){filterer[stops$age!="40+"]<-F}
                if(input$speed_internal){filterer[stops$speed!="1"]<-F}
                if(input$lane_violation_internal){filterer[stops$lane_violation!="1"]<-F}
                if(input$follow_to_close_internal){filterer[stops$follow_to_close!="1"]<-F}
                if(input$fail_to_signal_internal){filterer[stops$fail_to_signal!="1"]<-F}
                if(input$equipment_internal){filterer[stops$equipment!="1"]<-F}
                if(input$license_internal){filterer[stops$license!="1"]<-F}
                if(input$search_internal){filterer[stops$what_searched!=""]<-F}
                if(input$citation_internal){filterer[stops$citation!="1"]<-F}
                if(input$warning_internal){filterer[stops$warning!="1"]<-F}
                if(input$driver_arrest_internal){filterer[stops$driver_arrest!="YES"]<-F}
        }
        
        filtered_stops<-stops %>% filter(filterer)
        
        #filtering by time
        filtered_stops<-filtered_stops %>% filter((filtered_stops$calltime >= input$date1)&(filtered_stops$calltime <= input$date2))

        #aggregate the stops
        grouped_stops<- filtered_stops %>% group_by(GEOID20) %>%
                summarise(hit_rate=mean(contraband_found=="Yes")/mean(!what_searched==""), number_of_searches=sum(what_searched!=""),
                          White=mean(race=="W"), Black=mean(race=="B"), 
                          OtherRace=mean(race!="W"&race!="B"), Male=mean(gender=="M"), 
                          Female=mean(gender=="F"), Below_30=mean(age %in% c("18--29", "Under 18")),
                          Above_29=mean(age %in% c("40+", "30--39")),
                          propNonwhite=mean(race!="W"), total=n(),
                          propSpeed=mean(speed=="1", na.rm=T),
                          propLV=mean(lane_violation=="1", na.rm=T),
                          propFTC=mean(follow_to_close=="1", na.rm=T),
                          propFTS=mean(fail_to_signal=="1", na.rm=T),
                          propMoving=mean(moving=="1", na.rm=T),
                          propEquipment=mean(equipment=="1", na.rm=T),
                          propLicense=mean(license=="1", na.rm=T),
                          propInvestigative=mean(investigative=="1", na.rm=T)

                          )
        
        #merging hit rates back in
        tracts<-tracts %>% filter(GEOID20 %in% grouped_stops$GEOID20)
        grouped_stops<-grouped_stops %>% as.data.frame() %>% select(-21) #remove geometry
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
        
        #making the hit rates popup
        violation_popup <- paste0("<strong>Census Tract Identifier: </strong>", 
                                 paste0(grouped_stops$GEOID20), 
                                 "<br><strong>Percent Stopped for Speeding: </strong>", 
                                 paste0(round(grouped_stops$propSpeed, 3)*100, "%"),
                                 "<br><strong>Percent Stopped for Lane Violation: </strong>", 
                                 paste0(round(grouped_stops$propLV, 3)*100, "%"),
                                 "<br><strong>Percent Stopped for Following to Close: </strong>", 
                                 paste0(round(grouped_stops$propFTC, 3)*100, "%"),
                                 "<br><strong>Percent Stopped for Failure to Signal: </strong>", 
                                 paste0(round(grouped_stops$propFTC, 3)*100, "%"),
                                 "<br><strong>Percent Stopped for Moving Violation: </strong>", 
                                 paste0(round(grouped_stops$propMoving, 3)*100, "%"),
                                 "<br><strong>Percent Stopped for Equipment Violation: </strong>", 
                                 paste0(round(grouped_stops$propEquipment, 3)*100, "%"),
                                 "<br><strong>Percent Stopped for License: </strong>", 
                                 paste0(round(grouped_stops$propLicense, 3)*100, "%"),
                                 "<br><strong>Percent Stopped for Investigation: </strong>", 
                                 paste0(round(grouped_stops$propInvestigative, 3)*100, "%"),
                                 "<br><strong>Total Number of Stops: </strong>", 
                                 paste0(grouped_stops$total))
        
        #making the color ramp
        pal3<-colorNumeric(palette = c("white","purple"), domain=grouped_stops$propEquipment)
        
        #compiling hit rates leaflet
        output$map3<-renderLeaflet({
                l<-leaflet(data = grouped_stops) %>% addTiles() %>%
                        addPolygons(color = "#444444", 
                                    weight=1,
                                    fillColor = pal3(grouped_stops$propEquipment), 
                                    popup=violation_popup,
                                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                        bringToFront = TRUE))
                l %>% addLegend(position="bottomright", 
                                pal=pal3,
                                values=~hit_rate,
                                title = paste('Proportion of Stops for Equipment Violation by Census Tract'))
        })
        
        }, ignoreNULL = FALSE) #end observe event button
        
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
