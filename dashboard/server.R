# Dashboard Server Logic

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf) # polygons still depend on 
library(dygraphs) # time-series line graphs
library(timevis) # timeline
library(rmarkdown) # report generation
library(dplyr) # data manipulation and summary
library(htmltools) # forced EVAl of HTML
library(leafsync) # side by side
library(ggplot2) # report generation

library(magrittr) # better syntax see ?`%<>%`
library(tidyr)

library(httr)
library(jsonlite)

# source custom functions
source("functions.R")

# Load data and define palettes

load("cardiff.rda")
#load("crimes.rda") DEPRECATED WITH API
load("bounds.rda")
load("time_data.rda")

# API URL
apiURL <- "api.bransonf.com/stlcrime/"

# Define Palettes
inc_pal   <- colorBin("viridis", domain = 0:75000, bins = c(0,22880,32609,45375,58786,74425))
pov_pal   <- colorBin("viridis", domain = 0:100, bins = c(0,14,24,35,46,62))
hs_pal    <- colorBin("viridis", domain = 0:100, bins = c(0,71,79,86,91,99))
ba_pal    <- colorBin("viridis", domain = 0:100, bins = c(0,15,29,47,61,78))
unemp_pal <- colorBin("viridis", domain = 0:100, bins = c(0,6,11,18,26,36))
home_pal  <- colorBin("viridis", domain = 0:100, bins = c(0,19,38,51,67,86))

# package envrionmental data
env_data <- list(venues, park, hayden, wedge, atm, bar, club, liquor, gas, food, bus, school)

# Define server logic
shinyServer(function(input, output) {
  #  get current month
  cur_month <- strsplit(api_call(apiURL, "latest")," ")[[1]][1]
  
  ## Dynamic Month Slider based on year
    output$bas_month <- renderUI({
      # if this year, max is month - 1
      if(input$bas_year == as.numeric(format(Sys.Date(), "%Y"))){
        sliderTextInput("bas_month", "Select a Month:", month.name[1:which(month.name == cur_month)], cur_month)
      }
      else{
        sliderTextInput("bas_month", "Select a Month:", month.name, cur_month)
      }
    })
    output$adv_month <- renderUI({
      if(input$adv_year == as.numeric(format(Sys.Date(), "%Y"))){
        sliderTextInput("adv_month", "Select a Month:", month.name[1:which(month.name == cur_month)], cur_month)
      }
      else{
        sliderTextInput("adv_month", "Select a Month:", month.name, cur_month)
      }
    })
    output$dns_month <- renderUI({
      if(input$dns_year == as.numeric(format(Sys.Date(), "%Y"))){
        sliderTextInput("dns_month", "Select a Month:", month.name[1:which(month.name == cur_month)], cur_month)
      }
      else{
        sliderTextInput("dns_month", "Select a Month:", month.name, cur_month)
      }
    })
    output$sbs_month <- renderUI({
      if(input$sbs_year == as.numeric(format(Sys.Date(), "%Y"))){
        sliderTextInput("sbs_month", "Select a Month:", month.name[1:which(month.name == cur_month)], cur_month)
      }
      else{
        sliderTextInput("sbs_month", "Select a Month:", month.name, cur_month)
      }
    })
    output$rep_month <- renderUI({
      if(input$rep_year == as.numeric(format(Sys.Date(), "%Y"))){
        sliderTextInput("rep_month", "Select a Month:", month.name[1:which(month.name == cur_month)], cur_month)
      }
      else{
        sliderTextInput("rep_month", "Select a Month:", month.name, cur_month)
      }
    })
  
  
  ## Basic Map
    region_crime <- reactive({
      
      if(input$bas_region == "Police Districts"){

        crime <- api_call(apiURL, paste0("district",
                                         "?month=",input$bas_month,
                                         "&year=", input$bas_year,
                                         "&gun=",  ifelse(input$bas_gun, 'true', 'false'))) %>%
          dplyr::mutate(district = as.numeric(district)) %>%
          dplyr::left_join(districts, ., by = "district") %>%
          tidyr::spread("ucr_category", "Incidents")
        
        crime[is.na(crime)] <- 0
        
        return(crime)
      }
      else if(input$bas_region == "Neighborhoods"){
        
        crime <- api_call(apiURL, paste0("nbhood",
                                         "?month=",input$bas_month,
                                         "&year=", input$bas_year,
                                         "&gun=",  ifelse(input$bas_gun, 'true', 'false'))) %>%
          dplyr::mutate(neighborhood = as.numeric(neighborhood)) %>%
          dplyr::left_join(nbhoods, ., by = "neighborhood") %>%
          tidyr::spread("ucr_category", "Incidents")
        
        crime[is.na(crime)] <- 0
    
        return(crime)
      }
      })
    
      # define bin and pallete based on selection of crime and region (Not routinely generated, point of possible failure)
    region_bins <- reactive({
      binDict(input$bas_region, input$bas_crime)
    })
    
    region_pal <- reactive({
      pal <- colorBin("YlGnBu", bins = region_bins(), domain = switch (input$bas_crime,
                                                 "Homicide" = region_crime()$Homicide,
                                                 "Rape" = region_crime()$Rape,
                                                 "Robbery" = region_crime()$Robbery,
                                                 "Aggravated Assault" = region_crime()$`Aggravated Assault`))
      return(pal)
    })
    

    output$bas_map <- renderLeaflet({
      bm <- basemap(input$bas_base)$bm
      at <- basemap(input$bas_base)$at
      
      leafInit(bm, at) -> leaf
      
      labs <- paste0("<h4>",region_crime()$name, "</h4>",
                     "<b>Homicides: </b>", region_crime()$Homicide, "</br>",
                     "<b>Rapes: </b>", region_crime()$Rape, "</br>",
                     "<b>Robbery: </b>", region_crime()$Robbery, "</br>",
                     "<b>Assault: </b>", region_crime()$`Aggravated Assault`) %>% lapply(htmltools::HTML)
      
      
      if(input$bas_popups){
      leaf %<>% addPolygons(data = region_crime(), popup = labs,
                            fillColor = ~region_pal()(switch (input$bas_crime,
                                                              "Homicide" = region_crime()$Homicide,
                                                              "Rape" = region_crime()$Rape,
                                                              "Robbery" = region_crime()$Robbery,
                                                              "Aggravated Assault" = region_crime()$`Aggravated Assault`)),
                            weight = 2,
                            opacity = 1,
                            color = "white",
                            dashArray = "3",
                            fillOpacity = 0.7,
                            highlight = highlightOptions(
                              weight = 5,
                              color = "#666",
                              dashArray = "",
                              fillOpacity = 0.7,
                              bringToFront = TRUE))
      }
      else
      {
      leaf %<>% addPolygons(data = region_crime(), label = labs,
                            fillColor = ~region_pal()(switch (input$bas_crime,
                                                              "Homicide" = region_crime()$Homicide,
                                                              "Rape" = region_crime()$Rape,
                                                              "Robbery" = region_crime()$Robbery,
                                                              "Aggravated Assault" = region_crime()$`Aggravated Assault`)),
                            weight = 2,
                            opacity = 1,
                            color = "white",
                            dashArray = "3",
                            fillOpacity = 0.7,
                            highlight = highlightOptions(
                              weight = 5,
                              color = "#666",
                              dashArray = "",
                              fillOpacity = 0.7,
                              bringToFront = TRUE),
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"))
      }
      # add a legend
      if(input$bas_legend){
        leaf %>% addLegend("topleft", region_pal(),
                           title =  switch (input$bas_crime,
                                            "Homicide" = "Number of</br>Homicides",
                                            "Rape" = "Number of Rapes",
                                            "Robbery" = "Number of</br>Robberies",
                                            "Aggravated Assault" = "Number of</br>Assaults"
                                            ),
                           values = switch (input$bas_crime,
                                            "Homicide" = region_crime()$Homicide,
                                            "Rape" = region_crime()$Rape,
                                            "Robbery" = region_crime()$Robbery,
                                            "Aggravated Assault" = region_crime()$`Aggravated Assault`)) -> leaf
      }
      
      return(leaf)
    })
  
    observeEvent(NULL,{
      c = input$dns_map_center
      z = input$dns_map_zoom
      
      leafletProxy("bas_map") %>% setView(c$lng, c$lat, z)
    })
  ## Advanced Map
  ## TODO ADD better event reactions so that map zoom does not change (Using observe() and leafletProxy) #https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R
    output$adv_map <- renderLeaflet({
        # basemap and attribution case
        bm <- basemap(input$adv_base)$bm
        at <- basemap(input$adv_base)$at
      
        leafInit(bm, at) -> leaf
            
      # add demographic data
        leaf %<>% addDemographic(input$adv_demog, demog, boundary)
         
      # add environment variables
        leaf %<>% addEnvironment(input$adv_env, env_data)
        
      # add crime Data
      if(length(input$adv_crime) > 0){
        
        crime <- api_call(apiURL, paste0("coords",
                          "?month=", input$adv_month,
                          "&year=", input$adv_year,
                          "&gun=", ifelse(input$adv_gun, 'true', 'false'),
                          "&ucr=", jsonlite::toJSON(input$adv_crime)))
          
        # add points to map
          leaf %<>% addCrimePoints(input$adv_crime, crime)
        
      }
        
      #TODO add injury data
          
      # add legend
      if(input$adv_legend){
        if(input$adv_demog != "None"){
        p <- switch (input$adv_demog, "Median Income" = inc_pal, "Poverty Rate" = pov_pal, "High School Attainment" = hs_pal, "Bachelors Attainment" = ba_pal, "Unemployment Rate" = unemp_pal, "Home Ownership" = home_pal)
        v <- switch (input$adv_demog, "Median Income" = demog$med_income, "Poverty Rate" = demog$pov_pct, "High School Attainment" = demog$hs_pct, "Bachelors Attainment" = demog$ba_pct, "Unemployment Rate" = demog$unemploy_pct, "Home Ownership" = demog$home_own_pct)
        t <- switch (input$adv_demog, "Median Income" = "Median Income</br>(2017 Dollars)", "Poverty Rate" = "Poverty Rate %", "High School Attainment" = "High School</br>Attainment %", "Bachelors Attainment" = "Bachelors</br>Attainment %", "Unemployment Rate" = "Unemployment</br>Rate %", "Home Ownership" = "Home</br>Ownership %")
        
        leaf %>% addLegend("topleft", pal = p, values = v, opacity = .5, title = t) -> leaf
        }
        
        # draw symbol legend too
          syms <- c(); col <- c()
          if("Homicide" %in% input$adv_crime)    {syms <- c(syms, "Homicide");      col <- c(col, colorDict("mrd"))}          
          if("Rape" %in% input$adv_crime)        {syms <- c(syms, "Rape");          col <- c(col, colorDict("rap"))}          
          if("Robbery" %in% input$adv_crime)     {syms <- c(syms, "Robbery");       col <- c(col, colorDict("rob"))}          
          if("Assault" %in% input$adv_crime)     {syms <- c(syms, "Assault");       col <- c(col, colorDict("ast"))}          
          if("ATMs" %in% input$adv_env)          {syms <- c(syms, "ATM");           col <- c(col, colorDict("atm"))}          
          if("Bars" %in% input$adv_env)          {syms <- c(syms, "Bar");           col <- c(col, colorDict("bar"))}          
          if("Clubs" %in% input$adv_env)         {syms <- c(syms, "Club");          col <- c(col, colorDict("clb"))}         
          if("Liquor Stores" %in% input$adv_env) {syms <- c(syms, "Liquor Store");  col <- c(col, colorDict("liq"))}
          if("Gas Stations" %in% input$adv_env)  {syms <- c(syms, "Gas Station");   col <- c(col, colorDict("gas"))}
          if("Grocery Stores" %in% input$adv_env){syms <- c(syms, "Grocery Store"); col <- c(col, colorDict("grc"))}
          if("Bus Stops" %in% input$adv_env)     {syms <- c(syms, "Bus Stop");      col <- c(col, colorDict("bus"))}
          if("Schools" %in% input$adv_env)       {syms <- c(syms, "School");        col <- c(col, colorDict("scl"))}
          
        leaf %>% addCircleLegend(10, syms, col, "topleft") -> leaf  
        
      }
      
        
      # print map
      return(leaf)
    })
    
    observeEvent(NULL, {
      c = input$bas_map_center
      z = input$bas_map_zoom
      
      leafletProxy("adv_map") %>% setView(c$lng, c$lat, z)
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
    
    observeEvent(NULL,{
      c = input$adv_map_center
      z = input$adv_map_zoom
      
      leafletProxy("dns_map") %>% setView(c$lng, c$lat, z)
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
        leafL %<>% addCrimePoints(input$sbs_crime, crime)
        
      }
      
      #TODO add injury data
      
      # right
      leafInit(bmR, atR) -> leafR
      
      # add demographic layer
      leafR %<>% addDemographic(input$sbs_demog, demog, boundary)
      
      # add environment variables
      leafR %<>% addEnvironment(input$sbs_env, env_data)
      
      # add legends
      if(input$sbs_legend){
        if(input$sbs_demog != "None"){
          p <- switch (input$sbs_demog, "Median Income" = inc_pal, "Poverty Rate" = pov_pal, "High School Attainment" = hs_pal, "Bachelors Attainment" = ba_pal, "Unemployment Rate" = unemp_pal, "Home Ownership" = home_pal)
          v <- switch (input$sbs_demog, "Median Income" = demog$med_income, "Poverty Rate" = demog$pov_pct, "High School Attainment" = demog$hs_pct, "Bachelors Attainment" = demog$ba_pct, "Unemployment Rate" = demog$unemploy_pct, "Home Ownership" = demog$home_own_pct)
          t <- switch (input$sbs_demog, "Median Income" = "Median Income</br>(2017 Dollars)", "Poverty Rate" = "Poverty Rate %", "High School Attainment" = "High School</br>Attainment %", "Bachelors Attainment" = "Bachelors</br>Attainment %", "Unemployment Rate" = "Unemployment</br>Rate %", "Home Ownership" = "Home</br>Ownership %")
          
          leafR %>% addLegend("topleft", pal = p, values = v, opacity = .5, title = t) -> leafR
        }
        if(any(c("ATMs", "Bars", "Clubs", "Liquor Stores","Gas Stations", "Grocery Stores", "Bus Stops", "Schools") %in% input$sbs_env)){
          
          syms <- c(); col <- c()
          if("ATMs" %in% input$sbs_env)          {syms <- c(syms, "ATM");           col <- c(col, colorDict("atm"))}          
          if("Bars" %in% input$sbs_env)          {syms <- c(syms, "Bar");           col <- c(col, colorDict("bar"))}          
          if("Clubs" %in% input$sbs_env)         {syms <- c(syms, "Club");          col <- c(col, colorDict("clb"))}         
          if("Liquor Stores" %in% input$sbs_env) {syms <- c(syms, "Liquor Store");  col <- c(col, colorDict("liq"))}
          if("Gas Stations" %in% input$sbs_env)  {syms <- c(syms, "Gas Station");   col <- c(col, colorDict("gas"))}
          if("Grocery Stores" %in% input$sbs_env){syms <- c(syms, "Grocery Store"); col <- c(col, colorDict("grc"))}
          if("Bus Stops" %in% input$sbs_env)     {syms <- c(syms, "Bus Stop");      col <- c(col, colorDict("bus"))}
          if("Schools" %in% input$sbs_env)       {syms <- c(syms, "School");        col <- c(col, colorDict("scl"))}
          
          leafR %>% addCircleLegend(10, syms, col, "topleft") -> leafR
        }
        if(any(c("Homicide", "Rape", "Robbery", "Assault") %in% input$sbs_crime)){
          
          syms <- c(); col <- c()
          if("Homicide" %in% input$sbs_crime)    {syms <- c(syms, "Homicide");      col <- c(col, colorDict("mrd"))}          
          if("Rape" %in% input$sbs_crime)        {syms <- c(syms, "Rape");          col <- c(col, colorDict("rap"))}          
          if("Robbery" %in% input$sbs_crime)     {syms <- c(syms, "Robbery");       col <- c(col, colorDict("rob"))}          
          if("Aggravated Assault" %in% input$sbs_crime)     {syms <- c(syms, "Assault");       col <- c(col, colorDict("ast"))}
          
          leafL %<>% addCircleLegend(10, syms, col, "topleft")
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
      dg_f <- dygraph(vp_funding, xlab = "Year", ylab = "Total Funding ($ Thousands)", main = "Violence Prevention Funding by Year", group = "tline") %>% dyGroup("Total", "Total Funding ($)")
      dg_f[["x"]][["attrs"]][["animatedZooms"]] <- TRUE # Force animated zooms
      return(dg_f)
    })
    
    # outputs for downloads
    
    output$dlhmc  <- downloadHandler(filename = function(){
                                      "Stl_Homicides.csv"
                                      },
                                      content = function(file){
                                       write.csv(n_homicides, file = file, row.names = FALSE)
                                      }
                      )
    
    output$dlfund <- downloadHandler("Stl_ViolenceFunding.csv",
                                      content = function(file){
                                       write.csv(vp_funding, file = file, row.names = FALSE)
                                      }
                      )
    
    # generate custom reports using the crime data
    
   output$report <- downloadHandler(
     filename = function(){
        paste0("cardiff_Report_", substr(Sys.Date(),6,10),".html") # once LaTeX is available, PDF
     },
     content = function(file){
       # store in a temp dir because of dir privledges on server
       tempReport <- file.path(tempdir(), "report.Rmd")
       file.copy("report.Rmd",  tempReport, overwrite = TRUE)
       
       
       # store and send params to be rendered
       params <- list(
                  maps =  input$rep_maps,
                  tables = input$rep_table,
                  crimes = input$rep_crime,
                  month = input$rep_month,
                  yr = input$rep_year,
                  crimedata = crimes,
                  crimesf = crime_sf,
                  stlbound = boundary,
                  dist = districts,
                  hood = nbhoods
                  )
       
       rmarkdown::render(tempReport, output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
                         )
       
     }
   )
    
})
