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

# load copy
load("copy.rda")

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
           tabPanel("Timeline",
                    headerPanel(HTML("<h1 class=title>Timeline</h1>")),
                    timevisOutput("time"),
                    dygraphOutput("n_murders"),
                    dygraphOutput("funding_yr")
                    
           ),
           tabPanel("About",
                    # This links to the CSS stylesheet
                    tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                      tags$title("Cardiff Dashboard") # Page Title. Need to Add Favicon Still
                      
                    ),
                    headerPanel(HTML("<h1 class=title>The Cardiff Model</h1>")),
                    cardiff,
                    HTML("<h2>Violence Prevention Programs</h2>"),
                    vp_orgs
           ),
           tabPanel("Methods",
                    headerPanel(HTML("<h1 class=title>Data and Methodology</h1>")),
                    methods
           )
  )
)

   
    
