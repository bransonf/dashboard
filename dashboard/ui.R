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
# load Map UI
source("map_UI.R")

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
                    tabsetPanel(id = "map_op", type = "pills", # See Map_UI.R for MapUI components
                                tabPanel("Basic",
                                         fluidRow(
                                           column(9, leafletOutput("bas_map", height = "650px")),
                                           basMapUI(cur_month)
                                         )),
                                tabPanel("Advanced",
                                         fluidRow(
                                           column(9, leafletOutput("adv_map", height = "650px")),
                                           advMapUI(cur_month)
                                         )),
                                tabPanel("Density",
                                         fluidRow(
                                           column(9, leafletOutput("dns_map", height = "650px")),
                                           dnsMapUI(cur_month)
                                         )),
                                tabPanel("Comparison",
                                         fluidRow(
                                           column(9, htmlOutput("sbs_map", height = "650px")),
                                           sbsMapUI(cur_month)
                                         ))
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

   
    
