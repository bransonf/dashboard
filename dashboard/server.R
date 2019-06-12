# Dashboard Server Logic

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dygraphs)
library(timevis)

# source custom functions
source("functions.R")

# Load data and define palettes

load("cardiff.rda")
load("crime.rda")
load("n_murder.rda")
load("tvis.rda")
load("funding.rda")

# using Jenks Natural Breaks
inc_pal   <- colorBin("viridis", domain = 0:75000, bins = c(0,22880,32609,45375,58786,74425))
pov_pal   <- colorBin("viridis", domain = 0:100, bins = c(0,14,24,35,46,62))
hs_pal    <- colorBin("viridis", domain = 0:100, bins = c(0,71,79,86,91,99))
ba_pal    <- colorBin("viridis", domain = 0:100, bins = c(0,15,29,47,61,78))
unemp_pal <- colorBin("viridis", domain = 0:100, bins = c(0,6,11,18,26,36))
home_pal  <- colorBin("viridis", domain = 0:100, bins = c(0,19,38,51,67,86))



# Define server logic
shinyServer(function(input, output) {
  
    # draw a map
    output$map <- renderLeaflet({
        # basemap and attribution case
        bm <- switch(input$base, "Terrain" = "http://tile.stamen.com/terrain/{z}/{x}/{y}.jpg", "No Labels" =  "http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png")
        at <- switch(input$base, "Terrain" = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', "No Labels" =  '<a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, © <a href="https://carto.com/attribution">CARTO</a>')
      
        leaflet() %>%
            enableTileCaching() %>%
            addFullscreenControl() %>%
            addTiles(bm, attribution = at) %>%
            setView(-90.2594, 38.6530, zoom = 11) -> leaf
            
      # add demographic layer
      if(input$demog_select == "None"){                       leaf %>% addPolygons(data = boundary, fill = NA) -> leaf}
      else if(input$demog_select == "Median Income"){         leaf %>% addPolygons(data = demog, fillColor = ~inc_pal(med_income), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
      else if(input$demog_select == "Poverty Rate"){          leaf %>% addPolygons(data = demog, fillColor = ~pov_pal(pov_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
      else if(input$demog_select == "High School Attainment"){leaf %>% addPolygons(data = demog, fillColor = ~hs_pal(hs_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
      else if(input$demog_select == "Bachelors Attainment"){  leaf %>% addPolygons(data = demog, fillColor = ~ba_pal(ba_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
      else if(input$demog_select == "Unemployment Rate"){     leaf %>% addPolygons(data = demog, fillColor = ~unemp_pal(unemploy_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
      else if(input$demog_select == "Home Ownership"){        leaf %>% addPolygons(data = demog, fillColor = ~home_pal(home_own_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
            
      # add environment variables
      if("Venues" %in% input$env_chk){        leaf %>% addPolygons(data = venues, fillColor = "blue", stroke = NA, popup = venues$name) -> leaf}
      if("Parks" %in% input$env_chk){         leaf %>% addPolygons(data = park, fillColor = "green", stroke = NA, popup = park$name) -> leaf}
      
      if("ATMs" %in% input$env_chk){          leaf %>% addCircleMarkers(data = atm, radius = 6,stroke = NA, popup = atm$name, fillColor = "palegreen") -> leaf}
      if("Bars" %in% input$env_chk){          leaf %>% addCircleMarkers(data = bar, radius = 6,stroke = NA, popup = bar$name, fillColor = "orange") -> leaf}
      if("Clubs" %in% input$env_chk){         leaf %>% addCircleMarkers(data = club, radius = 6,stroke = NA, popup = club$name, fillColor = "orange") -> leaf}
      if("Liquor Stores" %in% input$env_chk){ leaf %>% addCircleMarkers(data = liquor, radius = 6,stroke = NA, popup = liquor$name, fillColor = "orange") -> leaf}
      if("Gas Stations" %in% input$env_chk){  leaf %>% addCircleMarkers(data = gas, radius = 6,stroke = NA, popup = gas$name, fillColor = "purple") -> leaf}
      if("Grocery Stores" %in% input$env_chk){leaf %>% addCircleMarkers(data = food, radius = 6,stroke = NA, popup = food$name, fillColor = "violetred") -> leaf}
      if("Bus Stops" %in% input$env_chk){     leaf %>% addCircleMarkers(data = bus, radius = 6,stroke = NA, fillColor = "olive", fillOpacity = .1) -> leaf}
      if("Schools" %in% input$env_chk){       leaf %>% addCircleMarkers(data = school, radius = 6,stroke = NA, popup = school$name, fillColor = "deepskyblue", fillOpacity = .45) -> leaf}
      #TODO get data if("Vacancy" %in% input$env_chk){       leaf %>% addCircleMarkers(data = vacancy) -> leaf}
    
      # add crime Data
      if(any(c("Homicide", "Rape", "Robbery", "Assault") %in% input$crime_chk)){
        
          fmonth <- which(month.name == input$month)
        # add to map
          if("Homicide" %in% input$crime_chk){homicide <- homicide[which(homicide$month == fmonth),]
            leaf %>% addCircleMarkers(data = homicide, radius = 6,stroke = NA, fillColor = "red", fillOpacity = .5) -> leaf}
          if("Rape" %in% input$crime_chk)    {rape <- rape[which(rape$month == fmonth),]
            leaf %>% addCircleMarkers(data = rape, radius = 6,stroke = NA, fillColor = "red", fillOpacity = .5) -> leaf}
          if("Robbery" %in% input$crime_chk) {rob <- rob[which(rob$month == fmonth),]
            leaf %>% addCircleMarkers(data = rob, radius = 6,stroke = NA, fillColor = "red", fillOpacity = .5) -> leaf}
          if("Assault" %in% input$crime_chk) {assault <- assault[which(assault$month == fmonth),]
            leaf %>% addCircleMarkers(data = assault, radius = 6,stroke = NA, fillColor = "red", fillOpacity = .5) -> leaf}
        
      }
      #TODO add injury data
          
      # add legend
      if(input$legend){
        if(input$demog_select != "None"){
        p <- switch (input$demog_select, "Median Income" = inc_pal, "Poverty Rate" = pov_pal, "High School Attainment" = hs_pal, "Bachelors Attainment" = ba_pal, "Unemployment Rate" = unemp_pal, "Home Ownership" = home_pal)
        v <- switch (input$demog_select, "Median Income" = demog$med_income, "Poverty Rate" = demog$pov_pct, "High School Attainment" = demog$hs_pct, "Bachelors Attainment" = demog$ba_pct, "Unemployment Rate" = demog$unemploy_pct, "Home Ownership" = demog$home_own_pct)
        t <- switch (input$demog_select, "Median Income" = "Median Income</br>(2017 Dollars)", "Poverty Rate" = "Poverty Rate %", "High School Attainment" = "High School</br>Attainment %", "Bachelors Attainment" = "Bachelors</br>Attainment %", "Unemployment Rate" = "Unemployment</br>Rate %", "Home Ownership" = "Home</br>Ownership %")
        
        leaf %>% addLegend("topleft", pal = p, values = v, opacity = .5, title = t) -> leaf
        }
        
        # draw symbol legend too
        if(length(input$crime_chk) > 0 | any(input$env_chk %in% c("ATMs","Bars","Clubs","Liquor Stores", "Gas Stations", "Grocery Stores", "Bus Stops", "Schools", "Vacancy"))){
          if(length(input$crime_chk) > 0){syms <- c("Crime"); col <- c("red")}
          else{syms <- c(); col <- c()}
          # color dictionary
          if("ATMs" %in% input$env_chk)          {syms <- c(syms, "ATM");           col <- c(col, "palegreen")}          
          if("Bars" %in% input$env_chk)          {syms <- c(syms, "Bar");           col <- c(col, "orange")}          
          if("Clubs" %in% input$env_chk)         {syms <- c(syms, "Club");          col <- c(col, "orange")}         
          if("Liquor Stores" %in% input$env_chk) {syms <- c(syms, "Liquor Store");  col <- c(col, "orange")}
          if("Gas Stations" %in% input$env_chk)  {syms <- c(syms, "Gas Station");   col <- c(col, "purple")}
          if("Grocery Stores" %in% input$env_chk){syms <- c(syms, "Grocery Store"); col <- c(col, "violetred")}
          if("Bus Stops" %in% input$env_chk)     {syms <- c(syms, "Bus Stop");      col <- c(col, "olive")}
          if("Schools" %in% input$env_chk)       {syms <- c(syms, "School");        col <- c(col, "deepskyblue")}
          
        leaf %>% addCircleLegend(10, syms, col, "topleft") -> leaf  
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
    
    #  draw a plot for murders
    output$n_murders <- renderDygraph({
      dygraph(n_homicides, xlab = "Year", ylab = "Number of Homicides",main = 'Homicides by Year', group = "tline")
    })

    # and for funding
    output$funding_yr <- renderDygraph({
      dygraph(vp_funding, xlab = "Year", ylab = "Total Funding ($ Thousands)", main = "Violence Prevention Funding by Year", group = "tline") %>% dyGroup("Total", "Total Funding ($)")
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
    
    # output$report <- downloadHandler(
    #   filename = function(){
    #     
    #   }
    # )
    
    
    
    
})
