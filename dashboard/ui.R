# Dashboard User Interface

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dygraphs)
library(timevis)
library(pushbar)

# load Map UI and copy
source("functions.R")
source("data/map_UI.R")
source("data/copy.R")

# begin page  
navbarPage(HTML("<div><img src='favicon/favicon-32x32.png'> STL Crime</div>"), fluid = TRUE, theme = "bootstrap.css", collapsible = TRUE, windowTitle = "Crime Dashboard",
           tabPanel("Map", icon = icon("map"),
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
                    ),
                    fluidRow(
                      column(10,
                        dygraphOutput("category_trend")
                      ),
                      column(2,
                        pickerInput("cat_crime", "Crime",
                                    choices = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burglary", "Larceny", "Vehicle Theft", "Arson", "Simple Assault", "Forgery", "Fraud", "Embezzlement", "Stolen Property", "Destruction of Property", "Weapons Offense", "Sex Offense", "VMCSL", "Offense Against Family", "DWI/DUI", "Liquor Laws", "Disorderly Conduct", "Loitering/Begging", "Other"),
                                    options = list(
                                      `actions-box` = TRUE, 
                                      size = 10,
                                      `selected-text-format` = "count > 3"
                                    ), 
                                    multiple = TRUE, selected = "Homicide")
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

   
    
