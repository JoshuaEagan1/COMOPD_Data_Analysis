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

stops<-st_join(stops, tracts, join = st_within) #%>% select(-c(59:61, 63:70))
stops<-stops %>% filter(GEOID20 !="29107090601")


#caching tracts
restore_tracts<-tracts

###############################################################################################

library(leaflet)
library(shiny)
library(shinydashboard)


# header board
header <- dashboardHeader(
        title = 'COMOPD Data'
        # task list for status of data processing
        , dropdownMenuOutput('task_menu'))

# Side bar boardy
sidebar <- dashboardSidebar(
        sidebarMenu(
                id = 'menu_tabs'
                , menuItem('menu1', tabName = 'menu1')
                , menuItem('menu2', tabName = 'menu2')
                
        )
)

# Body board
body <- dashboardBody(
        tabItems(
                
                tabItem(
                        tabName = 'menu1'
                        , tags$a(
                                id = "mydiv", href = "#", 'click me', 
                                onclick = 'Shiny.onInputChange("mydata", Math.random());')
                ),
                tabItem(
                        tabName = 'menu2'
                        , leafletOutput('map')
                        , verbatimTextOutput('summary')
                )
        )
)

# Shiny UI
ui <- dashboardPage(
        title = 'test',
        header,
        sidebar,
        body
)

server <- function(input, output, session) {
        observe({
                req(input$mydata)
                updateTabItems(session, 'menu_tabs', 'menu2')
        })
        output$map <- renderLeaflet({
                leaflet() %>%  
                        addTiles(options = tileOptions(maxZoom = 28, maxNativeZoom = 19),
                                 group = 'OSM')
        })
        output$summary <- renderPrint({
                print(input$mydata)
                print(leafletProxy('map')$id)
        })
        observe({
                req(input$mydata)
                proxy <- leafletProxy('map')
                print(proxy$id)
                proxy %>% 
                        setView(runif(1) * 30 +2, runif(1) * 30 + 2, 7)
        })
}


shinyApp(ui, server)


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
                , menuItem("filters", tabName = "filters")
                , menuItem("hit rates", tabName = "hit_rates")
                , menuItem("by race", tabName = "by_race")
                , menuItem('by infraction', tabName = 'by_infraction')
                
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
                        ,checkboxInput("18-29_internal", "18 to 29 Years Old")
                        ,checkboxInput("30-39_internal", "30 to 39 Years Old")
                        ,checkboxInput("40+_internal", "40 Years Old or Older")
                        
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
                        ,actionButton("filter_button", "GO")
                        , verbatimTextOutput("summary")
                        , leafletOutput("map")

                )
                
                #hit rates tab
                #,tabItem(
                #        tabName = 'hit_rates'
                #        , box(width = 8, title="Hit Rates by Census Tract", leafletOutput("map"))
                #)
                
                #
                #,tabItem(
                #        tabName = 'by_race'
                #        , leafletOutput('map')
                #),
                
                #
                #tabItem(
                #        tabName = 'by_infraction'
                #        , leafletOutput('map')
                #)
        )
)

server <- function(input, output, session) {
        
        #filtering stops data
        #filteredstops <- reactive({
                #input$filter_button
        #        stops
        #})
        
        observeEvent(input$filter_button, {
        
        #aggregate the stops
        grouped_stops<- stops %>% group_by(GEOID20) %>%
                summarise(hit_rate=mean(contraband_found=="Yes")/mean(!what_searched==""), number_of_searches=n(),
                          White=mean(race=="W"), Black=mean(race=="B"), 
                          OtherRace=mean(race!="W"&race!="B"), Male=mean(gender=="M"), 
                          Female=mean(gender=="F"), Below_30=mean(age %in% c("18--29", "Under 18")),
                          Above_29=mean(age %in% c("40+", "30--39")),
                          propNonwhite=mean(race!="W"))
        
        #merging hit rates back in
        tracts<-tracts %>% filter(GEOID20 %in% grouped_stops$GEOID20)
        grouped_stops<-grouped_stops %>% as.data.frame() %>% select(1:3)
        grouped_stops<-merge(tracts, grouped_stops)
        grouped_stops<-st_transform(grouped_stops, "+init=epsg:4326")
        
        
        #making the leaflets
        
        #making the popup
        hit_rate_popup <- paste0("<strong>Census Tract Identifier: </strong>", 
                              paste0(grouped_stops$GEOID20), 
                              "<br><strong>Percentage of Searches Discovering Contraband: </strong>", 
                              paste0(round(grouped_stops$hit_rate, 3)*100, "%"),
                              "<br><strong>Total Number of Searches: </strong>", 
                              paste0(grouped_stops$number_of_searches))
        
        #making the color ramp
        pal<-colorNumeric(palette = c("red","green"), domain=grouped_stops$hit_rate)
        
        #compiling leaflet
        output$map<-renderLeaflet({
                l<-leaflet(data = grouped_stops) %>% addTiles() %>%
                        addPolygons(color = "#444444", 
                                    weight=1,
                                    fillColor = pal(grouped_stops$hit_rate), 
                                    popup=hit_rate_popup,
                                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                        bringToFront = TRUE))
                l %>% addLegend(position="bottomright", 
                                pal=pal,
                                values=~hit_rate,
                                title = paste('Search Hit Rate by Census Tract'))
                })
        }) #end observe event button
        
}#end server

ui <- dashboardPage(
        header,
        sidebar,
        body
)

shinyApp(ui, server)