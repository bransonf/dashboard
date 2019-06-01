# Dashboard User Interface

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf)
library(timevis)
library(dygraphs)

# Get Current Month
cur_month <- month.name[as.numeric(format(Sys.Date(), "%m"))]
# Set Cardiff Text Here
cardiff <- HTML("<p class=sans> The U.S. Department of Justice found that more than half of violent crime in the United States goes unreported to law enforcement. This is a clear limitation to understanding where violence occurs. The Cardiff Model combines police and hospital data on violence. As a result, key stakeholders from policy makers to public health experts can more accurately assess violence in the community. <a href='https://www.cdc.gov/violenceprevention/publichealthissue/fundedprograms/cardiffmodel/whatis.html'>Read More at the CDC</a></p>")

# Begin Navbar page
shinyUI(
  navbarPage("Cardiff STL", fluid = TRUE,
           
           tabPanel("Map",
                    headerPanel(HTML("<h1 class=title>Cardiff Map</h1>")),
                    # Add a Row
                    fluidRow(
                      # Add Columns Within Row
                      column(9, leafletOutput("map", height = "600px")),
                      column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
                             selectInput("base", "Basemap", c("Terrain", "No Labels"), selected = "Terrain"),
                             pickerInput("crime_chk", "Crime",
                                         choices = c("Homicide", "Rape", "Robbery", "Assault"),
                                         options = list(
                                           `actions-box` = TRUE, 
                                           size = 10,
                                           `selected-text-format` = "count > 3"
                                         ), 
                                         multiple = TRUE),
                             pickerInput("inj_chk", "Violent Injury",
                                         choices = c("Gun Shot", "Stabbing", "Rape"),
                                         options = list(
                                           `actions-box` = TRUE, 
                                           size = 10,
                                           `selected-text-format` = "count > 3"
                                         ), 
                                         multiple = TRUE
                             ),
                             pickerInput("env_chk", "Environment",
                                         choices = c("ATMs", "Bars", "Clubs", "Liquor Stores", "Gas Stations", "Grocery Stores", "Bus Stops", "Schools", "Vacancy", "Venues", "Parks"),
                                         options = list(
                                           `actions-box` = TRUE, 
                                           size = 10,
                                           `selected-text-format` = "count > 3"
                                         ), 
                                         multiple = TRUE
                             ),
                             selectInput("demog_select", "Demographic",
                                         choices = c("Median Income", "Poverty Rate", "High School Attainment", "Bachelors Attainment", "Unemployment Rate", "Home Ownership", "None"),
                                         selected = "None"),
                             checkboxInput("legend", "Show Legend"),
                             sliderTextInput("month", "Select a Month:", month.name, cur_month),
                             submitButton("Update")
                      )
                    )
           ),
           tabPanel("About",
                    # This links to the CSS stylesheet
                    tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                      tags$title("Cardiff Dashboard") # Page Title. Need to Add Favicon Still
                      
                    ),
                    headerPanel(HTML("<h1 class=title>The Cardiff Model</h1>")),
                    cardiff
           ),
           
           tabPanel("Timeline",
                    headerPanel(HTML("<h1 class=title>Timeline</h1>")),
                    timevisOutput("time"),
                    dygraphOutput("n_murders")
             
           ),
           tabPanel("Methods",
                    headerPanel(HTML("<h1 class=title>Data and Methodology</h1>")),
                    HTML("<p class = sans>Currently, Demographic data comes from the 2017 American Community Survey 5 Year estimates (2013-2017 ACS5). Poverty was calculated as number of individuals living below the 2017 poverty line out of poverty calculable population. Educational Attainment was calculated for the population above the age of 18. Unemployment rate was calculated as number of individuals reporting unemployment out of total individuals reporting currently being in the workforce. Home ownership was calculated as number of individuals reporting ownership of the home they occupy out of total number of homes.</br>
                          Crime data comes from the City of Saint Louis and is only representative of 2018 in this demo. There were no cases of rape in the crime data for 2018. Assaults excluded those against police.</br>
                          Environmental data for schools, parks and bus stops were provided by the City of Saint Louis. Other environmental data were scraped from various web sources.</br>
                          Choropleth maps use Natural Jenks 5 class Breaks.</p>")
           )
  )
)

   
    
