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
                , menuItem("hit rates", tabName = "hit rates")
                , menuItem("by race", tabName = "by race")
                , menuItem('by infraction', tabName = 'by infraction')
                
        )
)

#Body
body <- dashboardBody(
        tabItems(
                tabItem(
                        tabName = 'filters'
                        ,tags$h1("Filter the Data")
                        ,tags$h6("For each category, you can remove different types of motorists from the
                                maps by unchecking their corresponding box. For an example, if you would 
                                like to see only stops of non-white motorists, uncheck the box labeled White.")

                        ,tags$h3("Demographics")
                        ,checkboxInput("white_internal", "White", TRUE)
                        ,checkboxInput("black_internal", "Black", TRUE)
                        ,checkboxInput("hispanic_internal", "Hispanic", TRUE)
                        ,checkboxInput("asian_internal", "Asian", TRUE)
                        ,checkboxInput("native_american_internal", "Native American", TRUE)
                        ,checkboxInput("male_internal", "Male", TRUE)
                        
                ),
                tabItem(
                        tabName = 'menu2'
                        , leafletOutput('map')
                        , verbatimTextOutput('summary')
                )
        )
)

#checkboxInput


ui <- dashboardPage(
        header,
        sidebar,
        body
)