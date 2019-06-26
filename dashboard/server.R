# Dashboard Server Logic

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dygraphs) # time-series line graphs
library(timevis) # timeline
library(rmarkdown) # report gen
library(dplyr) # data manipulation and summary
library(htmltools) # forced EVAl of HTML
library(leafsync) # side by side
library(leafpop) # popups, optional
library(ggplot2) # report generation and leafpop
library(shinyjs) # optional, for debugging

# source custom functions
source("functions.R")

# Load data and define palettes

load("cardiff.rda")
load("crimes.rda")
load("bounds.rda")
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

r <- 7 # define radius

# Define server logic
shinyServer(function(input, output) {
  
  ## Dynamic Month Slider based on year
    output$bas_month <- renderUI({
      # Get Current Month (Which is last month for this dashboard)
      cur_month <- month.name[as.numeric(format(Sys.Date(), "%m")) - 1]
      
      # if this year, max is month - 1
      if(input$bas_year == as.numeric(format(Sys.Date(), "%Y"))){
        sliderTextInput("bas_month", "Select a Month:", month.name[1:as.numeric(format(Sys.Date(), "%m")) - 1], cur_month)
      }
      # else all months
      else{
        sliderTextInput("bas_month", "Select a Month:", month.name, cur_month)
      }
    })
    output$adv_month <- renderUI({
      # Get Current Month (Which is last month for this dashboard)
      cur_month <- month.name[as.numeric(format(Sys.Date(), "%m")) - 1]
      
      # if this year, max is month - 1
      if(input$adv_year == as.numeric(format(Sys.Date(), "%Y"))){
        sliderTextInput("adv_month", "Select a Month:", month.name[1:as.numeric(format(Sys.Date(), "%m")) - 1], cur_month)
      }
      # else all months
      else{
        sliderTextInput("adv_month", "Select a Month:", month.name, cur_month)
      }
    })
    output$dns_month <- renderUI({
      # Get Current Month (Which is last month for this dashboard)
      cur_month <- month.name[as.numeric(format(Sys.Date(), "%m")) - 1]
      
      # if this year, max is month - 1
      if(input$dns_year == as.numeric(format(Sys.Date(), "%Y"))){
        sliderTextInput("dns_month", "Select a Month:", month.name[1:as.numeric(format(Sys.Date(), "%m")) - 1], cur_month)
      }
      # else all months
      else{
        sliderTextInput("dns_month", "Select a Month:", month.name, cur_month)
      }
    })
    output$sbs_month <- renderUI({
      # Get Current Month (Which is last month for this dashboard)
      cur_month <- month.name[as.numeric(format(Sys.Date(), "%m")) - 1]
      
      # if this year, max is month - 1
      if(input$sbs_year == as.numeric(format(Sys.Date(), "%Y"))){
        sliderTextInput("sbs_month", "Select a Month:", month.name[1:as.numeric(format(Sys.Date(), "%m")) - 1], cur_month)
      }
      # else all months
      else{
        sliderTextInput("sbs_month", "Select a Month:", month.name, cur_month)
      }
    })
    output$rep_month <- renderUI({
      # Get Current Month (Which is last month for this dashboard)
      cur_month <- month.name[as.numeric(format(Sys.Date(), "%m")) - 1]
      
      # if this year, max is month - 1
      if(input$rep_year == as.numeric(format(Sys.Date(), "%Y"))){
        sliderTextInput("rep_month", "Select a Month:", month.name[1:as.numeric(format(Sys.Date(), "%m")) - 1], cur_month)
      }
      # else all months
      else{
        sliderTextInput("rep_month", "Select a Month:", month.name, cur_month)
      }
    })
  
  
  ## Basic Map
    region_crime <- reactive({
      # filter for crimes, gun, year, date
      fmonth <- which(month.name == input$bas_month)
      fyear <- input$bas_year
      crime_sf <- crime_sf[which(crime_sf$month == fmonth & crime_sf$year == fyear),]
      crimes <- crimes[which(crimes$month == fmonth & crimes$year == fyear),]
      
      if(input$bas_gun){
        crime_sf <- crime_sf[which(crime_sf$gun),]
        crimes <- crimes[which(crimes$gun),]
      }
      
      # join to region
      if(input$bas_region == "Police Districts"){
        crimes <- group_by(crimes, district) %>%
          summarise(
            homicide = sum(homicide),
            rape = sum(rape),
            robbery = sum(robbery),
            assault = sum (assault)
          )
        dplyr::left_join(districts, crimes, by = "district") %>%
           mutate(homicide = ifelse(is.na(homicide),0,homicide),
                  rape = ifelse(is.na(rape),0,rape),
                  robbery = ifelse(is.na(robbery),0,robbery),
                  assault = ifelse(is.na(assault),0,assault))
      }
      else if(input$bas_region == "Neighborhoods"){
        crimes <- group_by(crimes, neighborhood) %>%
          summarise(
            homicide = sum(homicide),
            rape = sum(rape),
            robbery = sum(robbery),
            assault = sum (assault)
          )
        dplyr::left_join(nbhoods, crimes, by = "neighborhood") %>%
           mutate(homicide = ifelse(is.na(homicide),0,homicide),
                  rape = ifelse(is.na(rape),0,rape),
                  robbery = ifelse(is.na(robbery),0,robbery),
                  assault = ifelse(is.na(assault),0,assault))
      }

      
      
      })
    
      # define bin and pallete based on selection of crime and region (Not routinely generated, point of possible failure)
    region_bins <- reactive({
      if(input$bas_region == "Neighborhoods"){
        bins <- switch (input$bas_crime,
                        "Homicide" = c(0,1,2,5,10),
                        "Rape" = c(0,1,2),
                        "Robbery" = c(0,2,5,10,15),
                        "Assault" = c(0,5,10,15,30)
        )
      }
      else if(input$bas_region == "Police Districts"){
        bins <- switch (input$bas_crime,
                        "Homicide" = c(0,1,5,10,20),
                        "Rape" = c(0,1,2,5,10),
                        "Robbery" = c(0,10,20,30,50),
                        "Assault" = c(0,25,50,75,100)
        )
      }
      return(bins)
    })
    
    region_pal <- reactive({
      pal <- colorBin("YlGnBu", bins = region_bins(), domain = switch (input$bas_crime,
                                                 "Homicide" = region_crime()$homicide,
                                                 "Rape" = region_crime()$rape,
                                                 "Robbery" = region_crime()$robbery,
                                                 "Assault" = region_crime()$assault))
      return(pal)
    })
    
    # popg <- reactive({
    #   if(input$bas_popups){
    #     sf <- filter(crime_sf, year == input$bas_year & month == input$bas_month)
    #     
    #     gg = ggplot() +
    #       geom_sf(data = sf)
    #       
    #       
    #     return(gg)
    #   }
    # })
    
    output$bas_map <- renderLeaflet({
      # basemap and attribution case
      bm <- switch(input$bas_base, "Terrain" = "http://tile.stamen.com/terrain/{z}/{x}/{y}.jpg", "No Labels" =  "http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png")
      at <- switch(input$bas_base, "Terrain" = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', "No Labels" =  '<a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, © <a href="https://carto.com/attribution">CARTO</a>')
      
      leaflet() %>%
        enableTileCaching() %>%
        addFullscreenControl() %>%
        addTiles(bm, attribution = at) %>%
        setView(-90.2594, 38.6530, zoom = 11) -> leaf
      
      labs <- paste0("<b>Homicides: </b>", region_crime()$homicide, "</br>",
                     "<b>Rapes: </b>", region_crime()$rape, "</br>",
                     "<b>Robbery: </b>", region_crime()$robbery, "</br>",
                     "<b>Assault: </b>", region_crime()$assault) %>% lapply(htmltools::HTML)
      
      leaf %>% addPolygons(data = region_crime(), label = labs,
      fillColor = ~region_pal()(switch (input$bas_crime,
                               "Homicide" = region_crime()$homicide,
                               "Rape" = region_crime()$rape,
                               "Robbery" = region_crime()$robbery,
                               "Assault" = region_crime()$assault)),
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
        direction = "auto"),
      popup = #switch(input$bas_popups, TRUE = popupGraph(popg()), FALSE = ) # Maybe Implement Popup graphs, switch logic invalid
        region_crime()$name) -> leaf
      
      # add a legend
      if(input$bas_legend){
        leaf %>% addLegend("topleft", region_pal(),
                           title =  switch (input$bas_crime,
                                            "Homicide" = "Number of</br>Homicides",
                                            "Rape" = "Number of Rapes",
                                            "Robbery" = "Number of</br>Robberies",
                                            "Assault" = "Number of</br>Assaults"
                                            ),
                           values = switch (input$bas_crime,
                                             "Homicide" = region_crime()$homicide,
                                             "Rape" = region_crime()$rape,
                                             "Robbery" = region_crime()$robbery,
                                             "Assault" = region_crime()$assault)) -> leaf
      }
      
      return(leaf)
    })
  
  ## Advanced Map
  ## TODO ADD better event reactions so that map zoom does not change (Using observe() and leafletProxy) #https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R
    output$adv_map <- renderLeaflet({
        # basemap and attribution case
        bm <- switch(input$adv_base, "Terrain" = "http://tile.stamen.com/terrain/{z}/{x}/{y}.jpg", "No Labels" =  "http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png")
        at <- switch(input$adv_base, "Terrain" = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', "No Labels" =  '<a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, © <a href="https://carto.com/attribution">CARTO</a>')
      
        leaflet() %>%
            enableTileCaching() %>%
            addFullscreenControl() %>%
            addTiles(bm, attribution = at) %>%
            setView(-90.2594, 38.6530, zoom = 11) -> leaf
            
      # add demographic layer
      if(input$adv_demog == "None"){                       leaf %>% addPolygons(data = boundary, fill = NA) -> leaf}
      else if(input$adv_demog == "Median Income"){         leaf %>% addPolygons(data = demog, fillColor = ~inc_pal(med_income), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
      else if(input$adv_demog == "Poverty Rate"){          leaf %>% addPolygons(data = demog, fillColor = ~pov_pal(pov_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
      else if(input$adv_demog == "High School Attainment"){leaf %>% addPolygons(data = demog, fillColor = ~hs_pal(hs_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
      else if(input$adv_demog == "Bachelors Attainment"){  leaf %>% addPolygons(data = demog, fillColor = ~ba_pal(ba_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
      else if(input$adv_demog == "Unemployment Rate"){     leaf %>% addPolygons(data = demog, fillColor = ~unemp_pal(unemploy_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
      else if(input$adv_demog == "Home Ownership"){        leaf %>% addPolygons(data = demog, fillColor = ~home_pal(home_own_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leaf}
         
      # add environment variables
      if("Venues" %in% input$adv_env){        leaf %>% addPolygons(data = venues, fillColor = "blue", stroke = NA, popup = venues$name) -> leaf}
      if("Parks" %in% input$adv_env){         leaf %>% addPolygons(data = park, fillColor = "green", stroke = NA, popup = park$name) -> leaf}
      
      if("ATMs" %in% input$adv_env){          leaf %>% addCircleMarkers(data = atm, radius = r,stroke = NA, popup = atm$name, fillColor = colorDict("atm")) -> leaf}
      if("Bars" %in% input$adv_env){          leaf %>% addCircleMarkers(data = bar, radius = r,stroke = NA, popup = bar$name, fillColor = colorDict("bar")) -> leaf}
      if("Clubs" %in% input$adv_env){         leaf %>% addCircleMarkers(data = club, radius = r,stroke = NA, popup = club$name, fillColor = colorDict("clb")) -> leaf}
      if("Liquor Stores" %in% input$adv_env){ leaf %>% addCircleMarkers(data = liquor, radius = r,stroke = NA, popup = liquor$name, fillColor = colorDict("liq")) -> leaf}
      if("Gas Stations" %in% input$adv_env){  leaf %>% addCircleMarkers(data = gas, radius = r,stroke = NA, popup = gas$name, fillColor = colorDict("gas")) -> leaf}
      if("Grocery Stores" %in% input$adv_env){leaf %>% addCircleMarkers(data = food, radius = r,stroke = NA, popup = food$name, fillColor = colorDict("grc")) -> leaf}
      if("Bus Stops" %in% input$adv_env){     leaf %>% addCircleMarkers(data = bus, radius = r,stroke = NA, fillColor = colorDict("bus"), fillOpacity = .25) -> leaf}
      if("Schools" %in% input$adv_env){       leaf %>% addCircleMarkers(data = school, radius = r,stroke = NA, popup = school$name, fillColor = colorDict("scl"), fillOpacity = .45) -> leaf}
      #TODO get data if("Vacancy" %in% input$env_chk){       leaf %>% addCircleMarkers(data = vacancy) -> leaf}
    
      # add crime Data
      if(any(c("Homicide", "Rape", "Robbery", "Assault") %in% input$adv_crime)){
        # filter for month and year
          fmonth <- which(month.name == input$adv_month)
          fyear <- input$adv_year
          crime_sf <- crime_sf[which(crime_sf$month == fmonth & crime_sf$year == fyear),]
          
          if(input$adv_gun){
            crime_sf <- crime_sf[which(crime_sf$gun),]
          }
          
          homicide <- crime_sf[which(crime_sf$homicide),]
          rape     <- crime_sf[which(crime_sf$rape),]
          rob      <- crime_sf[which(crime_sf$robbery),]
          assault  <- crime_sf[which(crime_sf$assault),]
          
        
        # add points to map
          if("Homicide" %in% input$adv_crime){
            leaf %>% addCircleMarkers(data = homicide, radius = r,stroke = NA, fillColor = colorDict("mrd"), fillOpacity = .5) -> leaf}
          if("Rape" %in% input$adv_crime)    {
            leaf %>% addCircleMarkers(data = rape, radius = r,stroke = NA, fillColor = colorDict("rap"), fillOpacity = .5) -> leaf}
          if("Robbery" %in% input$adv_crime) {
            leaf %>% addCircleMarkers(data = rob, radius = r,stroke = NA, fillColor = colorDict("rob"), fillOpacity = .5) -> leaf}
          if("Assault" %in% input$adv_crime) {
            leaf %>% addCircleMarkers(data = assault, radius = r,stroke = NA, fillColor = colorDict("ast"), fillOpacity = .5) -> leaf}
        
        
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
    
  ## Density Map
    output$dns_map <- renderLeaflet({
      # basemap and attribution case
      bm <- switch(input$dns_base, "Terrain" = "http://tile.stamen.com/terrain/{z}/{x}/{y}.jpg", "No Labels" =  "http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png")
      at <- switch(input$dns_base, "Terrain" = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', "No Labels" =  '<a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, © <a href="https://carto.com/attribution">CARTO</a>')
      
      leaflet() %>%
        enableTileCaching() %>%
        addFullscreenControl() %>%
        addTiles(bm, attribution = at) %>%
        setView(-90.2594, 38.6530, zoom = 11) -> leaf
      
      # add crime Data
      if(any(c("Homicide", "Rape", "Robbery", "Assault") %in% input$dns_crime)){
        # filter for month and year
        fmonth <- which(month.name == input$dns_month)
        fyear <- input$dns_year
        crime_sf <- crime_sf[which(crime_sf$month == fmonth & crime_sf$year == fyear),]
        
        if(input$dns_gun){
          crime_sf <- crime_sf[which(crime_sf$gun),]
        }
        
        homicide <- crime_sf[which(crime_sf$homicide),]
        rape     <- crime_sf[which(crime_sf$rape),]
        rob      <- crime_sf[which(crime_sf$robbery),]
        assault  <- crime_sf[which(crime_sf$assault),]
        
        # add heatmap layer
          # init empty vector and build
          crm <- list(lon=NULL, lat=NULL)
          if("Homicide" %in% input$dns_crime){
            crm$lon <- append(crm$lon, st_coordinates(homicide)[,1])
            crm$lat <- append(crm$lat, st_coordinates(homicide)[,2])}
          if("Rape" %in% input$dns_crime)    {
            crm$lon <- append(crm$lon, st_coordinates(rape)[,1])
            crm$lat <- append(crm$lat, st_coordinates(rape)[,2])}
          if("Robbery" %in% input$dns_crime) {
            crm$lon <- append(crm$lon, st_coordinates(rob)[,1])
            crm$lat <- append(crm$lat, st_coordinates(rob)[,2])}
          if("Assault" %in% input$dns_crime) {
            crm$lon <- append(crm$lon, st_coordinates(assault)[,1])
            crm$lat <- append(crm$lat, st_coordinates(assault)[,2])}

          leaf %>% addWebGLHeatmap(data = crm,lng = ~lon, lat = ~lat, size = input$dns_size, units = "px") -> leaf
      }
        return(leaf)
    })
  ## Side by Side Map
    output$sbs_map <- renderUI({
      # basemap and attribution case
      bmL <- switch(input$sbs_baseL, "Terrain" = "http://tile.stamen.com/terrain/{z}/{x}/{y}.jpg", "No Labels" =  "http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png")
      atL <- switch(input$sbs_baseL, "Terrain" = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', "No Labels" =  '<a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, © <a href="https://carto.com/attribution">CARTO</a>')
      bmR <- switch(input$sbs_baseR, "Terrain" = "http://tile.stamen.com/terrain/{z}/{x}/{y}.jpg", "No Labels" =  "http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png")
      atR <- switch(input$sbs_baseR, "Terrain" = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', "No Labels" =  '<a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, © <a href="https://carto.com/attribution">CARTO</a>')
      
      # left
      leaflet() %>%
        enableTileCaching() %>%
        addFullscreenControl() %>%
        addTiles(bmL, attribution = atL) %>%
        setView(-90.2594, 38.6530, zoom = 11) -> leafL
      
      # add crime Data
      if(any(c("Homicide", "Rape", "Robbery", "Assault") %in% input$sbs_crime)){
        # filter for month and year
        fmonth <- which(month.name == input$sbs_month)
        fyear <- input$sbs_year
        crime_sf <- crime_sf[which(crime_sf$month == fmonth & crime_sf$year == fyear),]
        
        if(input$sbs_gun){
          crime_sf <- crime_sf[which(crime_sf$gun),]
        }
        
        homicide <- crime_sf[which(crime_sf$homicide),]
        rape     <- crime_sf[which(crime_sf$rape),]
        rob      <- crime_sf[which(crime_sf$robbery),]
        assault  <- crime_sf[which(crime_sf$assault),]
        
        
        # add points to map
        if("Homicide" %in% input$sbs_crime){
          leafL %>% addCircleMarkers(data = homicide, radius = r,stroke = NA, fillColor = colorDict("mrd"), fillOpacity = .5) -> leafL}
        if("Rape" %in% input$sbs_crime)    {
          leafL %>% addCircleMarkers(data = rape, radius = r,stroke = NA, fillColor = colorDict("rap"), fillOpacity = .5) -> leafL}
        if("Robbery" %in% input$sbs_crime) {
          leafL %>% addCircleMarkers(data = rob, radius = r,stroke = NA, fillColor = colorDict("rob"), fillOpacity = .5) -> leafL}
        if("Assault" %in% input$sbs_crime) {
          leafL %>% addCircleMarkers(data = assault, radius = r,stroke = NA, fillColor = colorDict("ast"), fillOpacity = .5) -> leafL}
        
        
      }
      
      #TODO add injury data
      
      # right
      leaflet() %>%
        enableTileCaching() %>%
        addFullscreenControl() %>%
        addTiles(bmR, attribution = atR) %>%
        setView(-90.2594, 38.6530, zoom = 11) -> leafR
      
      # add demographic layer
      if(input$sbs_demog == "None"){                       leafR %>% addPolygons(data = boundary, fill = NA) -> leafR}
      else if(input$sbs_demog == "Median Income"){         leafR %>% addPolygons(data = demog, fillColor = ~inc_pal(med_income), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leafR}
      else if(input$sbs_demog == "Poverty Rate"){          leafR %>% addPolygons(data = demog, fillColor = ~pov_pal(pov_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leafR}
      else if(input$sbs_demog == "High School Attainment"){leafR %>% addPolygons(data = demog, fillColor = ~hs_pal(hs_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leafR}
      else if(input$sbs_demog == "Bachelors Attainment"){  leafR %>% addPolygons(data = demog, fillColor = ~ba_pal(ba_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leafR}
      else if(input$sbs_demog == "Unemployment Rate"){     leafR %>% addPolygons(data = demog, fillColor = ~unemp_pal(unemploy_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leafR}
      else if(input$sbs_demog == "Home Ownership"){        leafR %>% addPolygons(data = demog, fillColor = ~home_pal(home_own_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5) -> leafR}
      
      # add environment variables
      if("Venues" %in% input$sbs_env){        leafR %>% addPolygons(data = venues, fillColor = "blue", stroke = NA, popup = venues$name) -> leafR}
      if("Parks" %in% input$sbs_env){         leafR %>% addPolygons(data = park, fillColor = "green", stroke = NA, popup = park$name) -> leafR}
      
      if("ATMs" %in% input$sbs_env){          leafR %>% addCircleMarkers(data = atm, radius = r,stroke = NA, popup = atm$name, fillColor = colorDict("atm")) -> leafR}
      if("Bars" %in% input$sbs_env){          leafR %>% addCircleMarkers(data = bar, radius = r,stroke = NA, popup = bar$name, fillColor = colorDict("bar")) -> leafR}
      if("Clubs" %in% input$sbs_env){         leafR %>% addCircleMarkers(data = club, radius = r,stroke = NA, popup = club$name, fillColor = colorDict("clb")) -> leafR}
      if("Liquor Stores" %in% input$sbs_env){ leafR %>% addCircleMarkers(data = liquor, radius = r,stroke = NA, popup = liquor$name, fillColor = colorDict("liq")) -> leafR}
      if("Gas Stations" %in% input$sbs_env){  leafR %>% addCircleMarkers(data = gas, radius = r,stroke = NA, popup = gas$name, fillColor = colorDict("gas")) -> leafR}
      if("Grocery Stores" %in% input$sbs_env){leafR %>% addCircleMarkers(data = food, radius = r,stroke = NA, popup = food$name, fillColor = colorDict("grc")) -> leafR}
      if("Bus Stops" %in% input$sbs_env){     leafR %>% addCircleMarkers(data = bus, radius = r,stroke = NA, fillColor = colorDict("bus"), fillOpacity = .25) -> leafR}
      if("Schools" %in% input$sbs_env){       leafR %>% addCircleMarkers(data = school, radius = r,stroke = NA, popup = school$name, fillColor = colorDict("scl"), fillOpacity = .45) -> leafR}
      #TODO get data if("Vacancy" %in% input$env_chk){       leaf %>% addCircleMarkers(data = vacancy) -> leaf}
      
      
      
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
          if("Assault" %in% input$sbs_crime)     {syms <- c(syms, "Assault");       col <- c(col, colorDict("ast"))}
          
          leafL %>% addCircleLegend(10, syms, col, "topleft") -> leafL
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
    
    
    # JS LOG for DEBUGGING
   observe({shinyjs::logjs(input$bas_map_shape_click)})
    
})
