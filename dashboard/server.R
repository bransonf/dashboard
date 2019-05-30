# Dashboard Server Logic

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf)

# Load data and define palettes

load("cardiff.rda")
# using Jenks Natural Breaks
inc_pal   <- colorBin("viridis", domain = 0:75000, bins = c(0,58786,32609,22880,45375,74425))
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
      if("Venues" %in% input$env_chk){        leaf %>% addPolygons(data = venues) -> leaf}
      if("Parks" %in% input$env_chk){         leaf %>% addPolygons(data = park) -> leaf}
      
      if("ATMs" %in% input$env_chk){          leaf %>% addCircleMarkers(data = atm) -> leaf}
      if("Bars" %in% input$env_chk){          leaf %>% addCircleMarkers(data = bar) -> leaf}
      if("Clubs" %in% input$env_chk){         leaf %>% addCircleMarkers(data = club) -> leaf}
      if("Liquor Stores" %in% input$env_chk){ leaf %>% addCircleMarkers(data = liquor) -> leaf}
      if("Gas Stations" %in% input$env_chk){  leaf %>% addCircleMarkers(data = gas) -> leaf}
      if("Grocery Stores" %in% input$env_chk){leaf %>% addCircleMarkers(data = food) -> leaf}
      if("Bus Stops" %in% input$env_chk){     leaf %>% addCircleMarkers(data = bus) -> leaf}
      if("Schools" %in% input$env_chk){       leaf %>% addCircleMarkers(data = school) -> leaf}
      #TODO get data if("Vacancy" %in% input$env_chk){       leaf %>% addCircleMarkers(data = vacancy) -> leaf}
      
      #TODO Crime and Injury Data
      # add crime Data
      
      # add injury data
  
      # print map
      return(leaf)
    })


})
