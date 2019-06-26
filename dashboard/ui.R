# Dashboard User Interface

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dygraphs)
library(timevis)

# load copy
load("copy.rda")
# load Map UI
source("map_UI.R")
source("report_UI.R")

# begin page  
navbarPage("Cardiff STL", fluid = TRUE, theme = "bootstrap.css",
           tabPanel("Map", icon = icon("map"),
                    tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"), # This links to the CSS stylesheet
                      tags$title("Cardiff Dashboard"), # Page Title
                      tags$script(src = "customHref.js"), # import the custom href function
                      # Import favicon package
                      tags$link(rel="apple-touch-icon", sizes="180x180", href="icon/apple-touch-icon.png"),
                      tags$link(rel="icon", type="image/png", sizes="32x32", href="icon/favicon-32x32.png"),
                      tags$link(rel="icon", type="image/png", sizes="16x16", href="icon/favicon-16x16.png"),
                      tags$link(rel="manifest", href="icon/site.webmanifest"),
                      tags$link(rel="mask-icon", href="icon/safari-pinned-tab.svg", color="#a50002"),
                      tags$meta(name ="msapplication-TileColor", content="#b91d47"),
                      tags$meta(name="theme-color", content="#ffffff")
                    ),
                    shinyjs::useShinyjs(), # enable js for debugging
                    headerPanel(HTML("<h1 class=title>Cardiff Map</h1>")),
                    tabsetPanel(id = "map_op", type = "pills", # See Map_UI.R for MapUI components
                                tabPanel("Basic",
                                         fluidRow(
                                           column(9, leafletOutput("bas_map", height = "650px")),
                                           basMapUI()
                                         )),
                                tabPanel("Advanced",
                                         fluidRow(
                                           column(9, leafletOutput("adv_map", height = "650px")),
                                           advMapUI()
                                         )),
                                tabPanel("Density",
                                         fluidRow(
                                           column(9, leafletOutput("dns_map", height = "650px")),
                                           dnsMapUI()
                                         )),
                                tabPanel("Comparison",
                                         fluidRow(
                                           column(9, htmlOutput("sbs_map", height = "650px")),
                                           sbsMapUI()
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
                    reportUI(rep_info) # see report_UI.R
           )
  )

   
    
