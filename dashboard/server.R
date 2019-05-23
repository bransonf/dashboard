# Dashboard Server Logic

library(shiny)
library(leaflet)
library(geojsonio)

# Load data and define palette
stl <- geojsonio::geojson_read("stl_boundary.geojson", what = "sp")
demog <- geojsonio::geojson_read("stl_demog.geojson", what = "sp")
bins <- c(0, 20, 40, 60, 80, 100)
pal <- colorBin("YlOrRd", domain = 0:100, bins = bins)
inc_bins <- c(0, 15000, 25000, 45000, 60000, 100000)
inc_pal <- colorBin("YlOrRd", domain = 0:75000, bins = inc_bins)

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
      if(input$demog_select == "None"){clearShapes(leaf) %>% addPolygons(data = stl, fill = NA)}
      else if(input$demog_select == "Median Income"){clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~inc_pal(med_income), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
      else if(input$demog_select == "Poverty Rate"){clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~pal(pov_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
      else if(input$demog_select == "High School Attainment"){clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~pal(hs_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
      else if(input$demog_select == "Bachelors Attainment"){clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~pal(ba_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
      else if(input$demog_select == "Unemployment Rate"){clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~pal(unemploy_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
      else if(input$demog_select == "Home Ownership"){clearShapes(leaf) %>% addPolygons(data = demog, fillColor = ~pal(home_own_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7)}
            
  
    })


})
