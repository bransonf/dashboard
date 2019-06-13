# Dashboard Server Logic

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dygraphs)
library(timevis)
library(rmarkdown)

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
colorDict <- function(key){ # define color dictionary, using https://carto.com/carto-colors/
  return(
    switch (key,
            "atm" = "#44AA99",
            "bar" = "#882255",
            "clb" = "#DDCC77",
            "liq" = "#117733",
            "gas" = "#332288",
            "grc" = "#AA4499",
            "bus" = "#88CCEE",
            "scl" = "#999933",
            "mrd" = "#CC6677",
            "rap" = "#661100",
            "rob" = "#6699CC",
            "ast" = "#888888"
    )
  )
}



# Define server logic
shinyServer(function(input, output) {
  
    # draw a map
  ## TODO ADD better event reactions so that map zoom does not change (Using observe() and leafletProxy)
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
      
      if("ATMs" %in% input$env_chk){          leaf %>% addCircleMarkers(data = atm, radius = r,stroke = NA, popup = atm$name, fillColor = colorDict("atm")) -> leaf}
      if("Bars" %in% input$env_chk){          leaf %>% addCircleMarkers(data = bar, radius = r,stroke = NA, popup = bar$name, fillColor = colorDict("bar")) -> leaf}
      if("Clubs" %in% input$env_chk){         leaf %>% addCircleMarkers(data = club, radius = r,stroke = NA, popup = club$name, fillColor = colorDict("clb")) -> leaf}
      if("Liquor Stores" %in% input$env_chk){ leaf %>% addCircleMarkers(data = liquor, radius = r,stroke = NA, popup = liquor$name, fillColor = colorDict("liq")) -> leaf}
      if("Gas Stations" %in% input$env_chk){  leaf %>% addCircleMarkers(data = gas, radius = r,stroke = NA, popup = gas$name, fillColor = colorDict("gas")) -> leaf}
      if("Grocery Stores" %in% input$env_chk){leaf %>% addCircleMarkers(data = food, radius = r,stroke = NA, popup = food$name, fillColor = colorDict("grc")) -> leaf}
      if("Bus Stops" %in% input$env_chk){     leaf %>% addCircleMarkers(data = bus, radius = r,stroke = NA, fillColor = colorDict("bus"), fillOpacity = .1) -> leaf}
      if("Schools" %in% input$env_chk){       leaf %>% addCircleMarkers(data = school, radius = r,stroke = NA, popup = school$name, fillColor = colorDict("scl"), fillOpacity = .45) -> leaf}
      #TODO get data if("Vacancy" %in% input$env_chk){       leaf %>% addCircleMarkers(data = vacancy) -> leaf}
    
      # add crime Data
      if(any(c("Homicide", "Rape", "Robbery", "Assault") %in% input$crime_chk)){
        
          fmonth <- which(month.name == input$month)
          fyear <- input$year
        # add to map
          if("Homicide" %in% input$crime_chk){homicide <- homicide[which(homicide$month == fmonth & homicide$year == fyear),]
            leaf %>% addCircleMarkers(data = homicide, radius = r,stroke = NA, fillColor = colorDict("mrd"), fillOpacity = .5) -> leaf}
          if("Rape" %in% input$crime_chk)    {rape <- rape[which(rape$month == fmonth & rape$year == fyear),]
            leaf %>% addCircleMarkers(data = rape, radius = r,stroke = NA, fillColor = colorDict("rap"), fillOpacity = .5) -> leaf}
          if("Robbery" %in% input$crime_chk) {rob <- rob[which(rob$month == fmonth & rob$year == fyear),]
            leaf %>% addCircleMarkers(data = rob, radius = r,stroke = NA, fillColor = colorDict("rob"), fillOpacity = .5) -> leaf}
          if("Assault" %in% input$crime_chk) {assault <- assault[which(assault$month == fmonth & assault$year == fyear),]
            leaf %>% addCircleMarkers(data = assault, radius = r,stroke = NA, fillColor = colorDict("ast"), fillOpacity = .5) -> leaf}
        
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
          syms <- c(); col <- c()
          if("Homicide" %in% input$crime_chk)    {syms <- c(syms, "Homicide");      col <- c(col, colorDict("mrd"))}          
          if("Rape" %in% input$crime_chk)        {syms <- c(syms, "Rape");          col <- c(col, colorDict("rap"))}          
          if("Robbery" %in% input$crime_chk)     {syms <- c(syms, "Robbery");       col <- c(col, colorDict("rob"))}          
          if("Assault" %in% input$crime_chk)     {syms <- c(syms, "Assault");       col <- c(col, colorDict("ast"))}          
          if("ATMs" %in% input$env_chk)          {syms <- c(syms, "ATM");           col <- c(col, colorDict("atm"))}          
          if("Bars" %in% input$env_chk)          {syms <- c(syms, "Bar");           col <- c(col, colorDict("bar"))}          
          if("Clubs" %in% input$env_chk)         {syms <- c(syms, "Club");          col <- c(col, colorDict("clb"))}         
          if("Liquor Stores" %in% input$env_chk) {syms <- c(syms, "Liquor Store");  col <- c(col, colorDict("liq"))}
          if("Gas Stations" %in% input$env_chk)  {syms <- c(syms, "Gas Station");   col <- c(col, colorDict("gas"))}
          if("Grocery Stores" %in% input$env_chk){syms <- c(syms, "Grocery Store"); col <- c(col, colorDict("grc"))}
          if("Bus Stops" %in% input$env_chk)     {syms <- c(syms, "Bus Stop");      col <- c(col, colorDict("bus"))}
          if("Schools" %in% input$env_chk)       {syms <- c(syms, "School");        col <- c(col, colorDict("scl"))}
          
        leaf %>% addCircleLegend(10, syms, col, "topleft") -> leaf  
        
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
