# Columbia (Missouri) Police Department Data Analysis

These projects will shed light on different aspects of traffic policing in Columbia, Missouri. The goal of these projects is to make data and insights from the traffic stops data released by the Columbia Police Department (COMOPD) easily accessible. My hope is that this will provide a common understanding between citizens and law enforcement to serve as a starting point for a conversation about what criminal justice enforcement should look like in Columbia, Missouri.

## Projects

There are two main products I am making available to COMOPD, Columbia's Civilian Police Review Board, and the public. 

### COMOPD Data Dashboard
I designed a data dashboard that will empower people who want to know more about what traffic stops look like to explore the data in user friendly, customizable environment. <https://jeagan.shinyapps.io/cpd_data_dashboard/>

### Veil of Darkness Analysis
I released a report that analyses Columbia's traffic stops data for discrimination against black drivers. This test leverages as-good-as-random variation in visibility between daylight and darkness to test for discrimination against Black motorists. Controlling for where traffic stops take place and when they take place, officers should be less able to observe the race of motorists just after sunset relative to just before sunset. Therefore, if the likelihood that a stopped driver is Black decreases significantly after sunset, this is evidence of racial discrimination against black motorists.

## Work-flow

The code for this project was written in R version 3.6.1 on a Windows machine.

To reproduce the entire data cleaning procedure: 

1. Run the file: `\VoD\scraping_como_twilight_data_20210119.R`
2. Run the file `cleaning_traffic_stops_data.R` through line 433 , skip until the end of the file, and save the data using the last line of code
3. Visit [this website](https://developers.google.com/maps/documentation/geocoding/overview) to create a Google Cloud account and find your geocoding API key
4. Next, open the file `\police beat\geocoding 2020 stops.R` and replace both instances of 'SECRET' with your API key
5. Open the `\police beat\tigerline data\notes.txt` file and follow the instructions to download the shape files and demographic information
6. Use the `\police beat\geocoding 2020 stops.R` script to add latitude and longitude to the traffic stops data. If you want to re-geocode all the stops, you will need to edit this file to include all the stops, not just the ones from 2020. Otherwise, the pre-2020 stops are included in a .csv file in the `\police beat` directory.
7. Use the `\police beat\creating geographic variables.R` script to assign a police beat to the traffic stops data
8. Run the file `cleaning_traffic_stops_data.R` all the way through.

If you want to use the data that I have already cleaned, it is available in the main project directory, the most recent version is called `standardized_traffic_stops_data_20211014.R`. Some additional cleaning is needed to process the data into the form it is used in the dashboard. Run the clean data through the `\CPD_data_dashboard\cleaning app data.R` script to prepare the data to run the shiny app. To rerun the Veil of Darkness analysis, just run the clean data through the script `\VOD\analysis.Rmd`, and it will produce the report `\VOD\analysis.pdf`.
