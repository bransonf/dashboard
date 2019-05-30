# Dashboard Server Logic

library(shiny)
library(shinyWidgets)
library(leaflet)
library(sf)

# Load data and define palettes

load("cardiff.rda")
# using Jenks Natural Breaks
inc_pal   <- colorBin("viridis", domain = 0:75000, bins = c(0,58786,32609,22880,45375,74425))
pov_pal   <- colorBin("viridis", domain = 0:100, bins = c(0,14,24,35,46,61))
hs_pal    <- colorBin("viridis", domain = 0:100, bins = c(0,71,79,86,91,98))
ba_pal    <- colorBin("viridis", domain = 0:100, bins = c(0,15,29,47,61,77))
unemp_pal <- colorBin("viridis", domain = 0:100, bins = c(0,6,11,18,26,35))
home_pal  <- colorBin("viridis", domain = 0:100, bins = c(0,19,38,51,67,85))

# define color palettes


# Define server logic
shinyServer(function(input, output) {

    # draw a map
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.Terrain,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            setView(-90.2594, 38.6530, zoom = 11) -> leaf
            
      # add demographic layer
      if(input$demog_select == "None"){clearShapes(leaf) %>% addPolygons(data = boundary, fill = NA)}
      else if(input$demog_select == "Median Income"){         clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~inc_pal(med_income), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
      else if(input$demog_select == "Poverty Rate"){          clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~pov_pal(pov_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
      else if(input$demog_select == "High School Attainment"){clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~hs_pal(hs_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
      else if(input$demog_select == "Bachelors Attainment"){  clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~ba_pal(ba_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
      else if(input$demog_select == "Unemployment Rate"){     clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~unemp_pal(unemploy_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
      else if(input$demog_select == "Home Ownership"){        clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~home_pal(home_own_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
            
      # add Environment Variables
      #if("ATMs" %in% input$env_chk){leaf %>% addCircleMarkers(data = atm)}
  
    })


})
