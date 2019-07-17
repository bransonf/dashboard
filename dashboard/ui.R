# Dashboard User Interface

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dygraphs)
library(timevis)

# load Map UI and copy
source("data/map_UI.R")
source("data/copy.R")

# begin page  
navbarPage("STL Crime", fluid = TRUE, theme = "bootstrap.css",
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
                      tags$meta(name="theme-color", content="#ffffff"),
                      # Import google fonts
                      tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Libre+Baskerville|Lora|Open+Sans&display=swap")
                    ),
                    headerPanel(HTML("<h1 class=title>Crime Map</h1>")),
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
                    fluidRow(
                      column(10, offset = 1,
                        timevisOutput("time")
                      )
                    ),
                    fluidRow(
                      column(10, offset = 1,
                        dygraphOutput("n_murders"),
                        dygraphOutput("funding_yr")
                      )
                    )
           ),
           tabPanel("Prevention", value = "prev", icon = icon("handshake"),
                    HTML("<h1 class=title>Violence Prevention Programs</h1>"),
                    fluidRow(
                      column(12,
                        vp_orgs
                      )
                    )
           ),
           tabPanel("About", icon = icon("book"),
                    headerPanel(HTML("<h1 class=title>About This Project</h1>")),
                    fluidRow(
                      column(10, offset = 1,
                             about
                      )
                    ),
                    fluidRow(
                      column(10, offset = 1,
                        HTML("<h2 class=title>Data & Methodology</h2>"),
                        methods
                      )
                    )
           )
           
  )

   
    
