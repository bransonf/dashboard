# Dashboard User Interface

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dygraphs)
library(timevis)

# Get Current Month (Which is last month for this dashboard)
cur_month <- month.name[as.numeric(format(Sys.Date(), "%m")) - 1]

# load copy
load("copy.rda")

# begin page  
navbarPage("Cardiff STL", fluid = TRUE,
           tabPanel("Map", icon = icon("map"),
                    
                    tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"), # This links to the CSS stylesheet
                      tags$title("Cardiff Dashboard"), # Page Title
                      tags$script(src = "customHref.js"), # And to import the custom href function
                      tags$link(rel="shortcut icon", href="favicon.ico") # Import favicon
                    ),
                    headerPanel(HTML("<h1 class=title>Cardiff Map</h1>")),
                    
                    fluidRow(
                     
                      column(9, leafletOutput("map", height = "650px")),
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
                                         choices = c("Gun Shot *", "Stabbing *", "Rape *"),
                                         options = list(
                                           `actions-box` = TRUE, 
                                           size = 10,
                                           `selected-text-format` = "count > 3"
                                         ), 
                                         multiple = TRUE
                             ),
                             pickerInput("env_chk", "Environment",
                                         choices = c("ATMs", "Bars", "Clubs", "Liquor Stores", "Gas Stations", "Grocery Stores", "Bus Stops", "Schools", "Vacancy *", "Venues", "Parks"),
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
                             checkboxInput("legend", "Show Legend(s)"),
                             radioButtons("year", "Select a Year:", c(2018, 2019), 2019, inline = TRUE),
                             sliderTextInput("month", "Select a Month:", month.name, cur_month),
                             fluidRow(
                               #column(1, submitButton("Update")),
                               #column(1, offset = 3, dropdownButton(nav, icon = icon("question"), size = "sm", right = TRUE, up = TRUE))
                             )
                               
                      )
                    )
                    
           ),
           tabPanel("Timeline", icon = icon("clock"),
                    headerPanel(HTML("<h1 class=title>Timeline</h1>")),
                    timevisOutput("time"),
                    dygraphOutput("n_murders"),
                    dygraphOutput("funding_yr")
                    
           ),
           tabPanel("About", value = "about", icon = icon("sticky-note"),
                    headerPanel(HTML("<h1 class=title>The Cardiff Model</h1>")),
                    cardiff,
                    HTML("<h2>Violence Prevention Programs</h2>"),
                    vp_orgs
           ),
           tabPanel("Methods", icon = icon("book"),
                    headerPanel(HTML("<h1 class=title>Data and Methodology</h1>")),
                    methods
           ),
           tabPanel("Downloads", icon = icon("file-download"),
                    headerPanel(HTML("<h1 class=title>Data Downloads</h1>")),
                      column(12,
                             fluidRow(align = "center",
                                downloadButton('dlhmc', "Homicide Counts"),
                                downloadButton('dlfund', "Funding")
                             )
                      )
           ),
           tabPanel("Reports", icon = icon("file-alt"),
                    headerPanel(HTML("<h1 class=title>Generate a Report</h1>")),
                      column(12, align = 'center',
                             HTML("<h4 class=sans>Select Options Below</h4>"),
                             fluidRow(align = "center",
                                      pickerInput("rep_table", "Table(s)",
                                                  choices = c("Summary Table","District Table", "Neighborhood Table"),
                                                  options = list(
                                                    `actions-box` = TRUE, 
                                                    size = 10,
                                                    `selected-text-format` = "count > 3"
                                                  ),multiple = TRUE
                                      ),
                                      pickerInput("rep_maps", "Map(s)",
                                                  choices = c("District Map", "Neighborhood Map", "Point Map", "Heat Map"),
                                                  options = list(
                                                    `actions-box` = TRUE, 
                                                    size = 10,
                                                    `selected-text-format` = "count > 3"
                                                  ),multiple = TRUE
                                      ),
                                      pickerInput("rep_crime", "Crime(s)",
                                                  choices = c("Homicide","Rape", "Assault", "Robbery", "Non-Violent Crimes"),
                                                  options = list(
                                                    `actions-box` = TRUE, 
                                                    size = 10,
                                                    `selected-text-format` = "count > 3"
                                                  ),multiple = TRUE
                                      ),
                                      #checkboxInput("rep_", ""),
                                      radioButtons("rep_year", "Select a Year:", c(2018, 2019), 2019, inline = TRUE),
                                      sliderTextInput("rep_month", "Select a Month:", month.name, cur_month),
                                      HTML("<h4 class=sans>Generate Report</h4>"),
                                      downloadButton('report', "Download")
                             ),
                             fluidRow(rep_info)
                      )
           )
  )

   
    
