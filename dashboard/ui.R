# Dashboard User Interface

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dygraphs)
library(timevis)
library(pushbar)
library(waiter)

# load Map UI and copy
source("functions.R")
source("data/map_UI.R")
source("data/copy.R")

# begin page  
navbarPage(HTML("<div><img src='favicon/favicon-32x32.png'> STL Crime</div>"), fluid = TRUE, theme = "bootstrap.css", collapsible = TRUE, windowTitle = "Crime Dashboard",
           tabPanel("Map", icon = icon("map"),
                    use_waiter(),
                    waiter_show_on_load(
                      tagList(spin_folding_cube(),
                              span("Loading...", style = "color:white;")
                      )
                    ),
                    tags$head(
                      tags$script(src = "customHref.js"), # import the custom href function
                      # Import favicon package
                      tags$link(rel="apple-touch-icon", sizes="180x180", href="favicon/apple-touch-icon.png"),
                      tags$link(rel="icon", type="image/png", sizes="32x32", href="favicon/favicon-32x32.png"),
                      tags$link(rel="icon", type="image/png", sizes="16x16", href="favicon/favicon-16x16.png"),
                      tags$link(rel="manifest", href="favicon/site.webmanifest"),
                      tags$link(rel="mask-icon", href="favicon/safari-pinned-tab.svg", color="#a50002"),
                      tags$meta(name ="msapplication-TileColor", content="#b91d47"),
                      tags$meta(name="theme-color", content="#ffffff"),
                      # Import google fonts
                      tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Libre+Baskerville|Lora|Open+Sans&display=swap")
                    ),
                    
                    # Render UI Based on Mobile/Desktop
                    mobileDetect('isMobile'),
                    pushbar_deps(),
                    uiOutput('ui')
           ),
           tabPanel("Trends", icon = icon("calendar-alt"),
                    headerPanel(HTML("<h1 class=title>Trends</h1>")),
                    fluidRow(
                      column(8, offset = 1,
                        dygraphOutput("custom_trend")
                      ),
                      column(2,
                        uiOutput('trend_ui')    
                      )
                    ),
                    HTML('<div id="tlbreak"></div>'),
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

   
    
