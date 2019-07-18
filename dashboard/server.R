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

# source custom functions and load data
source("functions.R")
load("data/rtm.rda")
load("data/bounds.rda")
source("data/time_data.R")

# API URL
apiURL <- "api.bransonf.com/stlcrime/"

# package envrionmental data
env_data <- list(venues, park, hayden, wedge, atm, bar, club, liquor, gas, hotel, bus, school)

# Define server logic
shinyServer(function(input, output) {
  #  get current month
  cur_month <- strsplit(api_call(apiURL, "latest")," ")[[1]][1]
  
  ## Dynamic Month Slider based on year
    output$bas_month <- renderUI({
     monthSliderUI(input$bas_year, cur_month, "bas_month")
    })
    output$adv_month <- renderUI({
      monthSliderUI(input$adv_year, cur_month, "adv_month")
    })
    output$dns_month <- renderUI({
      monthSliderUI(input$dns_year, cur_month, "dns_month")
    })
    output$sbs_month <- renderUI({
      monthSliderUI(input$sbs_year, cur_month, "sbs_month")
    })
  
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
    
  ## Basic Map
    output$bas_map <- renderLeaflet({
      bm <- basemap(input$bas_base)$bm
      at <- basemap(input$bas_base)$at
      
      leafInit(bm, at) -> leaf
      
      
      region_crime <- regionCrime(input$bas_region, input$bas_month, input$bas_year, input$bas_gun, nbhoods, districts)
      
      leaf %<>% choropleth(region_crime, input$bas_crime, input$bas_region)

      # add a legend
      if(input$bas_legend){
        leaf %<>% choroLegend(region_crime, input$bas_crime, input$bas_region)
      }
      
      return(leaf)
    })
  
  ## Advanced Map
    output$adv_map <- renderLeaflet({
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
        
        crime <- api_call(apiURL, paste0("coords",
                          "?month=", input$adv_month,
                          "&year=", input$adv_year,
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
        if(length(input$adv_env) > 0){
          leaf %<>% envLegend(input$adv_env, c(input$adv_crime, input$adv_env))
        }
        if(length(input$adv_crime) > 0){
          leaf %<>% crimeLegend(input$adv_crime, c(input$adv_crime, input$adv_env))
        }
      }
      
        
      # print map
      return(leaf)
    })
  
  ## Density Map
    output$dns_map <- renderLeaflet({
      bm <- basemap(input$dns_base)$bm
      at <- basemap(input$dns_base)$at
      
      leafInit(bm, at) -> leaf
      
      # add crime Data
      if(length(input$dns_crime) > 0){
        crime <- api_call(apiURL, paste0("coords",
                                         "?month=", input$dns_month,
                                         "&year=", input$dns_year,
                                         "&gun=", ifelse(input$dns_gun, 'true', 'false'),
                                         "&ucr=", jsonlite::toJSON(input$dns_crime)))
                   
        if(length(crime$wgs_x) < 1){NULL}
        else{
          crime %<>% filter(!is.na(wgs_x) & !is.na(wgs_y))
          leaf %<>% addWebGLHeatmap(lng = crime$wgs_x, lat = crime$wgs_y, size = input$dns_size, units = "px")}
          
      }
      
        return(leaf)
    })
    
  ## Side by Side Map
    output$sbs_map <- renderUI({
      # basemap and attribution case
      bmL <- basemap(input$sbs_baseL)$bm
      atL <- basemap(input$sbs_baseL)$at
      bmR <- basemap(input$sbs_baseR)$bm
      atR <- basemap(input$sbs_baseR)$at
      
      # left
      leafInit(bmL, atL) -> leafL
      
      # add crime Data
      if(length(input$sbs_crime) > 0){
        
        crime <- api_call(apiURL, paste0("coords",
                                         "?month=", input$sbs_month,
                                         "&year=", input$sbs_year,
                                         "&gun=", ifelse(input$sbs_gun, 'true', 'false'),
                                         "&ucr=", jsonlite::toJSON(input$sbs_crime)))
        
        # add points to map
        leafL %<>% addCrimePoints(input$sbs_crime, crime, c(input$sbs_crime, input$sbs_env))
        
      }
      
      # right
      leafInit(bmR, atR) -> leafR
      
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
      h = 650
      s[[1]][[1]][["children"]][[1]][["sizingPolicy"]][["defaultHeight"]] <- h
      s[[1]][[2]][["children"]][[1]][["sizingPolicy"]][["defaultHeight"]] <- h
      
      return(s)
      
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

    
})
