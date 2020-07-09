# Dashboard Server Logic

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf) # spatial class (udunits and gdal depends)
library(dygraphs) # time-series line graphs
library(timevis) # timeline
library(dplyr) # data manipulation and summary
library(leafsync) # side by side
library(magrittr) # better syntax see ?`%<>%`
library(tidyr)
library(httr) # API requests (openssl depends)
library(jsonlite) # Parsing
library(mapview) # mapshot function
library(RColorBrewer)
library(pushbar) # JS Push bar for controls on mobile
library(lubridate) # date/time manipulation
library(waiter) # loading screens (GitHub Version!)
library(xts) # class for time series data

# source custom functions and load data
# source("functions.R")  Sourced in UI
load("data/rtm.rda")
load("data/bounds.rda")
source("data/time_data.R")
load("data/block_units.rda") # Block Units...

# API URL
apiURL <- "http://api.stldata.org/legacy/"

# package envrionmental data
env_data <- list(venues, park, hayden, wedge, atm, bar, club, liquor, gas, hotel, bus, school)

# Define server logic
shinyServer(function(input, output) {
  
  # Dynamic Filter for gun crime
    output$bas_gunf <- renderUI({
      gunFiltUI(input$bas_crime, "bas_gun")
      })
    output$adv_gunf <- renderUI({
      gunFiltUI(input$adv_crime, "adv_gun")
      })
    output$dns_gunf <- renderUI({
      gunFiltUI(input$dns_crime, "dns_gun")
      })
    output$sbs_gunf <- renderUI({
      gunFiltUI(input$sbs_crime, "sbs_gun")
      })
    output$mob_gunf <- renderUI({
      gunFiltUI(input$mob_crime, "mob_gun")
      })
    output$trend_gunf <- renderUI({
      gunFiltUI(input$trend_crime, "trend_gun")
    })
    
  # Dynamic Filter for seasonality toggle
    output$trend_seasf <- renderUI({
      seasUI(input$trend_interval, input$trend_date)
    })
    
  ## Basic Map
    basic_map <- reactive({
      bm <- basemap(input$bas_base)$bm
      at <- basemap(input$bas_base)$at
      
      leafInit(bm, at) -> leaf
      
      # parsing month and year
      bas_month <- month.name[lubridate::month(input$bas_date)]
      bas_year <- lubridate::year(input$bas_date)
      
      region_crime <- regionCrime(input$bas_region, bas_month, bas_year, input$bas_gun, nbhoods, districts)
      
      leaf %<>% choropleth(region_crime, input$bas_crime, input$bas_region)
      
      # add a legend
      if(input$bas_legend){
        leaf %<>% choroLegend(region_crime, input$bas_crime, input$bas_region)
      }
      
      return(leaf)
    })
    
    output$bas_map <- renderLeaflet({
      basic_map() %>%
        addFullscreenControl()
    })
    
    # stop waiter screen
    hide_waiter()
    
    ## Save Basic Map
    output$bas_save <- downloadHandler(
      filename = function(){paste(
        "STL", input$bas_month, input$bas_year, input$bas_crime,
        "Map.png", sep = "_")},
      content = function(file) {
        withProgress(message = 'Exporting Map...', {
        basic_map() %>% setView(input$bas_map_center[1],
                                input$bas_map_center[2],
                                input$bas_map_zoom) -> m
          incProgress(.6)
          mapshot(m, file = file)
          incProgress(.3, message = "Map Saved")
        })
      }
    )
    
  ## Advanced Map
    advanced_map <- reactive({
        # basemap and attribution case
        bm <- basemap(input$adv_base)$bm
        at <- basemap(input$adv_base)$at
      
        leafInit(bm, at) -> leaf
            
      # add demographic data
        leaf %<>% addDemographic(input$adv_demog, demog, boundary)
         
      # add environment variables
        leaf %<>% addEnvironment(input$adv_env, env_data, c(input$adv_crime, input$adv_env))
        
      # add crime Data
      if(length(input$adv_crime) > 0){
        
        crime <- api_call(apiURL, paste0("range",
                          "?start=", input$adv_date[1],
                          "&end=", input$adv_date[2],
                          "&gun=", ifelse(input$adv_gun, 'true', 'false'),
                          "&ucr=", jsonlite::toJSON(input$adv_crime)))
        
        # add points to map
          leaf %<>% addCrimePoints(input$adv_crime, crime, c(input$adv_crime, input$adv_env))

      }
          
      # add legend
      if(input$adv_legend){
        if(input$adv_demog != "None"){
          leaf %<>% demogLegend(demog, input$adv_demog)
        }
        if(length(input$adv_crime) > 0){
          leaf %<>% crimeLegend(input$adv_crime, c(input$adv_crime, input$adv_env))
        }
        if(length(input$adv_env) > 0){
          leaf %<>% envLegend(input$adv_env, c(input$adv_crime, input$adv_env))
        }
      }
      
        # blockunits
        if(input$adv_bunit){
          pops <- paste0("<b>Area Council: </b>", bunits$`Area Council`, "<br>",
                         "<b>Block Unit Number: </b>", bunits$`Block Unit Number`) %>% lapply(HTML)
          leaf %<>% addMarkers(bunits$lon, bunits$lat, popup = pops, icon = makeIcon("icons/home-15.svg", popupAnchorX = 10, popupAnchorY = 1))
        }
        
      # print map
      return(leaf)
    })
    
    output$adv_map <- renderLeaflet({
      advanced_map() %>%
        addFullscreenControl()
    })
  
    ## Save Advanced Map
    output$adv_save <- downloadHandler(
      filename = function(){paste(
        "STL", input$adv_month, input$adv_year,
        "Crime_Map.png", sep = "_")},
      content = function(file) {
        withProgress(message = 'Exporting Map...', {
          advanced_map() %>% setView(input$adv_map_center[1],
                                  input$adv_map_center[2],
                                  input$adv_map_zoom) -> m
          incProgress(.6)
          mapshot(m, file = file)
          incProgress(.3, message = "Map Saved")
        })
      }
    )
    
  ## Density Map
    density_map <- reactive({
      bm <- basemap(input$dns_base)$bm
      at <- basemap(input$dns_base)$at
      
      leafInit(bm, at) -> leaf
      
      # add crime Data
      if(length(input$dns_crime) > 0){
        crime <- api_call(apiURL, paste0("range",
                                         "?start=", input$dns_date[1],
                                         "&end=", input$dns_date[2],
                                         "&gun=", ifelse(input$dns_gun, 'true', 'false'),
                                         "&ucr=", jsonlite::toJSON(input$dns_crime)))
                   
        if(length(crime$wgs_x) < 1){NULL}
        else{
          crime %<>% filter(!is.na(wgs_x) & !is.na(wgs_y))
          leaf %<>% addHeatmap(lng = crime$wgs_x, lat = crime$wgs_y, radius = input$dns_size, gradient = RColorBrewer::brewer.pal(9,"YlOrRd")[1:8], blur = input$dns_blur)}
          
      }
      
        return(leaf)
    })
    
    output$dns_map <- renderLeaflet({
      density_map() %>%
        addFullscreenControl()
    })
    
    ## Save Density Map
    output$dns_save <- downloadHandler(
      filename = function(){paste(
        "STL", input$dns_month, input$dns_year,
        "Heat_Map.png", sep = "_")},
      content = function(file) {
        withProgress(message = 'Exporting Map...', {
          density_map() %>% setView(input$dns_map_center[1],
                                  input$dns_map_center[2],
                                  input$dns_map_zoom) -> m
          incProgress(.6)
          mapshot(m, file = file)
          incProgress(.3, message = "Map Saved")
        })
      }
    )
    
  ## Side by Side Map
    output$sbs_map <- renderUI({
      # basemap and attribution case
      bmL <- basemap(input$sbs_baseL)$bm
      atL <- basemap(input$sbs_baseL)$at
      bmR <- basemap(input$sbs_baseR)$bm
      atR <- basemap(input$sbs_baseR)$at
      
      # left
      leafInit(bmL, atL) %>% addFullscreenControl() -> leafL
      
      # add crime Data
      if(length(input$sbs_crime) > 0){
        
        crime <- api_call(apiURL, paste0("range",
                                         "?start=", input$sbs_date[1],
                                         "&end=", input$sbs_date[2],
                                         "&gun=", ifelse(input$sbs_gun, 'true', 'false'),
                                         "&ucr=", jsonlite::toJSON(input$sbs_crime)))
        
        # add points to map
        leafL %<>% addCrimePoints(input$sbs_crime, crime, c(input$sbs_crime, input$sbs_env))
        
      }
      
      # right
      leafInit(bmR, atR) %>% addFullscreenControl() -> leafR
      
      # add demographic layer
      leafR %<>% addDemographic(input$sbs_demog, demog, boundary)
      
      # add environment variables
      leafR %<>% addEnvironment(input$sbs_env, env_data, c(input$sbs_crime, input$sbs_env))
      
      # add legends
      if(input$sbs_legend){
        if(input$sbs_demog != "None"){
          leafR %<>% demogLegend(demog, input$sbs_demog)
        }
        if(length(input$sbs_crime) > 0){
          leafL %<>% crimeLegend(input$sbs_crime, c(input$sbs_crime, input$sbs_env))
        }
        if(length(input$sbs_env) > 0){
          leafR %<>% envLegend(input$sbs_env, c(input$sbs_crime, input$sbs_env))
        }
      }
      
      ## Create side by side
      s = leafsync::sync(leafL, leafR)
      
      # set height
      h = '60vh'
      s[[1]][[1]][["children"]][[1]][["sizingPolicy"]][["defaultHeight"]] <- h
      s[[1]][[2]][["children"]][[1]][["sizingPolicy"]][["defaultHeight"]] <- h
      
      # force inline width to be correct
      s[[1]][[1]][["attribs"]][["style"]] %<>% gsub("49%", "50%", .)
      s[[1]][[2]][["attribs"]][["style"]] %<>% gsub("49%", "50%", .)
      
      return(s)
      
    })
    
  ## Mobile Map
    output$mob_map <- renderLeaflet({
      # basemap and attribution case
      bm <- basemap(input$mob_base)$bm
      at <- "" # No Attribution on Mobile...
      
      leafInit(bm, at) %>%
        addFullscreenControl() -> leaf
      
      # add demographic data
      leaf %<>% addDemographic(input$mob_demog, demog, boundary)
      
      # add environment variables
      leaf %<>% addEnvironment(input$mob_env, env_data, c(input$mob_crime, input$mob_env))
      
      # add crime Data
      if(length(input$mob_crime) > 0){
        
        crime <- api_call(apiURL, paste0("range",
                                         "?start=", input$mob_date[1],
                                         "&end=", input$mob_date[2],
                                         "&gun=", ifelse(input$mob_gun, 'true', 'false'),
                                         "&ucr=", jsonlite::toJSON(input$mob_crime)))
        
        # add points to map
        leaf %<>% addCrimePoints(input$mob_crime, crime, c(input$mob_crime, input$mob_env))
        
      }
      
      # add legend
      if(input$mob_legend){
        if(input$mob_demog != "None"){
          leaf %<>% demogLegend(demog, input$mob_demog)
        }
        if(length(input$mob_crime) > 0){
          leaf %<>% crimeLegend(input$mob_crime, c(input$mob_crime, input$mob_env))
        }
        if(length(input$mob_env) > 0){
          leaf %<>% envLegend(input$mob_env, c(input$mob_crime, input$mob_env))
        }
      }
      
      # print map
      return(leaf)
  })
    
    # draw a timeline # https://visjs.org/docs/timeline/#Configuration_Options
    output$time <- renderTimevis({
      timevis(tvis, options = list(
        start = "1990-06-01",
        end = "1991-06-01",
        stack = TRUE,
        type = "point",
        maxHeight = "400px",
        minHeight = "300px",
        max = "2019-06-01",
        min = "1990-01-01",
        showCurrentTime = FALSE,
        zoomMax = 6.307e+11, # Approx 20 years
        zoomMin = 1.314e+9 # Approx .5 Month
      ))
    })
    
    # draw a custom plot based on user selections
    output$custom_trend <- renderDygraph({
      # API Call to Get Data
      api_data <- parseTrend(start = input$trend_date[1], end = input$trend_date[2], input$trend_interval, input$trend_gun, input$trend_crime, input$trend_seas)
      
      # Make DyGraph
      dg_c <- dygraph(api_data, ylab = "Number of Incidents")
      dg_c[["x"]][["attrs"]][["animatedZooms"]] <- TRUE # Force animated zooms
      return(dg_c)
    })
    
    #  draw a plot for murders
    output$n_murders <- renderDygraph({
      dg_m <- dygraph(n_homicides, xlab = "Year", ylab = "Number of Homicides",main = 'Homicides by Year', group = "tline")
      dg_m[["x"]][["attrs"]][["animatedZooms"]] <- TRUE # Force animated zooms
      return(dg_m)
    })

    # and for funding
    output$funding_yr <- renderDygraph({
      dg_f <- dygraph(vp_funding, xlab = "Year", ylab = "Total Funding (Thousands $)", main = "Violence Prevention Funding by Year", group = "tline") %>% dyGroup("Total", "Total Funding ($)")
      dg_f[["x"]][["attrs"]][["animatedZooms"]] <- TRUE # Force animated zooms
      return(dg_f)
    })

    # Setup Pushbar
    setup_pushbar(overlay = FALSE)
    
    
    # Mobile Push Bar
    observeEvent(input$open, {
      pushbar_open(id = "mobpush")
    })  
    
    observeEvent(input$close, {
      pushbar_close()
    })
    
    # Desktop Push Bar
    observeEvent(input$open_bas, {
      pushbar_open(id = "baspush")
    })
    
    observeEvent(input$basclose, {
      pushbar_close()
    })
    
    observeEvent(input$open_adv, {
      pushbar_open(id = "advpush")
    })
    
    observeEvent(input$advclose, {
      pushbar_close()
    })
    
    observeEvent(input$open_dns, {
      pushbar_open(id = "dnspush")
    })
    
    observeEvent(input$dnsclose, {
      pushbar_close()
    })
    
    
    # Render the UI based on Mobile or Desktop Status
    output$ui <- renderUI({
      if(input$isMobile){
        # Mobile Version
        div(
          div(class='outer_mobile',
              includeCSS("./www/bootstrap.css"),
              leafletOutput('mob_map', height = '100%', width = '100%')  
          ),
          div(class='mob-bttn',
              actionBttn("open", "Select Data", icon('server'), style = "jelly", block = TRUE, color = 'danger')),
              pushbar(
                mobMapUI(),
                id = "mobpush", # add id to get event
                actionButton("close", "Close"),
                from = 'bottom'
              )
        )
      }
      else{
        # Desktop Version
        div(
        headerPanel(HTML("<h1 class=title>Crime Map</h1>")),
        tabsetPanel(id = "map_op", type = "pills", # See Map_UI.R for MapUI components
                    tabPanel("Basic",
                              div(class='outer',
                                  includeCSS("./www/bootstrap.css"),
                                  leafletOutput("bas_map", height = "100%", width = "100%")
                              ),
                              div(class='data-btn',
                                  actionBttn("open_bas", "Select Data", icon('server'), style = 'jelly', color = 'danger'),
                                  pushbar(
                                    basMapUI(),
                                    id = "baspush",
                                    column(12,
                                      actionButton("basclose", "Close")
                                    ),
                                    from = 'right'
                                  )
                              )
                    ),
                    tabPanel("Advanced",
                            div(class='outer',
                                includeCSS("./www/bootstrap.css"),
                                leafletOutput("adv_map", height = "100%", width = "100%")
                            ),
                            div(class='data-btn',
                                actionBttn("open_adv", "Select Data", icon('server'), style = 'jelly', color = 'danger'),
                                pushbar(
                                  advMapUI(),
                                  id = "advpush",
                                  column(12,
                                    actionButton("advclose", "Close")
                                  ),
                                  from = 'right'
                                )
                            )
                    ),
                    tabPanel("Heatmap",
                            div(class='outer',
                                includeCSS("./www/bootstrap.css"),
                                leafletOutput("dns_map", height = "100%", width = "100%")
                            ),
                            div(class='data-btn',
                                actionBttn("open_dns", "Select Data", icon('server'), style = 'jelly', color = 'danger'),
                                pushbar(
                                  dnsMapUI(),
                                  id = "dnspush",
                                  column(12,
                                    actionButton("dnsclose", "Close")
                                  ),
                                  from = 'right'
                                )
                            )
                    ),
                    tabPanel("Side by Side",
                            fluidRow(
                              column(12,
                                htmlOutput("sbs_map")
                              )
                            ),
                            sbsMapUI()
                    )
        )) # end div
        
      }
    })
    
    output$trend_ui <- renderUI({
      trendUI()
    })
})
