# These should be minified custom functions for loading the web application

# add circles to legend for points
addCircleLegend <- function(map, size, text, color, position){
  if(length(text) != length(color)){stop("Color and Text arguments should be of equal length")}
  leg <- paste0()
  for (i in seq_along(text)) {
    leg <- paste0(leg, paste0('<i style="background:', color[i], '; width:10px; height:10px;opacity:0.5;margin-right: 4px;
    display: inline-block;
           vertical-align: top;
           border-radius: 50%;
           width:', size,'px;
           height:', size,'px;
           margin-top: 4px;"></i>
             <div style="display: inline-block;height: 10px;margin-top: 4px;line-height: 10px;">', text[i], '</div>
             <br>'))
  }
  leg <- shiny::HTML(paste0(leg, '</div>'))
  return(leaflet::addControl(map, html = leg, position))
}

# define color dictionary, using https://carto.com/carto-colors/
colorDict <- function(key){
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
            "ast" = "#888888",
            "undef" = "#ff5d30"
    )
}

# define pallette dictionary ## NOT CURRENTLY IN USE, Syntax may be worse than saving 6 lines in server.R
palDict <- function(key){
    switch (key,
            "inc"   = colorBin("viridis", domain = 0:75000, bins = c(0,22880,32609,45375,58786,74425)),
            "pov"   = colorBin("viridis", domain = 0:100, bins = c(0,14,24,35,46,62)),
            "hs"    = colorBin("viridis", domain = 0:100, bins = c(0,71,79,86,91,99)),
            "ba"    = colorBin("viridis", domain = 0:100, bins = c(0,15,29,47,61,78)),
            "unemp" = colorBin("viridis", domain = 0:100, bins = c(0,6,11,18,26,36)),
            "home"  = colorBin("viridis", domain = 0:100, bins = c(0,19,38,51,67,86))
    )
}
# define bins dictionary
binDict <- function(region, crime){
  if(region == "Neighborhoods"){
    bins = switch (crime,
                   "Homicide" = c(0,1,2,5,10),
                   "Rape" = c(0,1,2),
                   "Robbery" = c(0,2,5,10,15),
                   "Aggravated Assault" = c(0,5,10,15,30)
    )
  }
  else if(region == "Police Districts"){
    bins = switch (crime,
                   "Homicide" = c(0,1,5,10,20),
                   "Rape" = c(0,1,2,5,10),
                   "Robbery" = c(0,10,20,30,50),
                   "Aggravated Assault" = c(0,25,50,75,115)
    )
  }
  return(bins)
}

# define basemap url and attribution
basemap <- function(input){
  bm <- switch(input,
               "Satellite" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
               "Terrain"   = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}{r}.png",
               "No Labels" = "https://{s}.basemaps.cartocdn.com/rastertiles/light_nolabels/{z}/{x}/{y}{r}.png")
  at <- switch(input,
               "Satellite" = "Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community",
               "Terrain"   = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
               "No Labels" = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>')
  
  
  return(list(bm = bm, at = at))
}

# initialize a leaflet map to build from
leafInit <- function(bm, at){
  leaflet(options = leafletOptions(zoomSnap = 0.5)) %>%
    enableTileCaching() %>%
    addFullscreenControl() %>%
    addTiles(bm, attribution = at) %>%
    setView(-90.2594, 38.6530, zoom = 11)
}

# function to make API calls and parse the response
api_call <- function(base, endpoint){
  
 parsed <- httr::GET(utils::URLencode(paste0(base, endpoint))) %>%
 httr::content(., as = "text", encoding = "utf-8") %>%
   jsonlite::fromJSON()
 
 # return the parsed json (dataframe..)
 return(parsed)
}

# function for adding points to leaflet map
addPoints <- function(map, lon, lat, radius = 7, stroke = NA, fillColor = colorDict("undef"), fillOpacity = .5){
  
    addCircleMarkers(map, lon, lat, radius, stroke = stroke, fillColor = fillColor, fillOpacity = fillOpacity)

}

# larger batch function for adding crime points
addCrimePoints <- function(map, variables, data){
  if("Homicide" %in% variables){
    
    x <- filter(data, ucr_category == "Homicide") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("mrd"))
    }
  }
  if("Rape" %in% variables){
    
    x <- filter(data, ucr_category == "Rape")
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("rap"))
    }
  }
  if("Robbery" %in% variables){
    
    x <- filter(data, ucr_category == "Robbery") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("rob"))
    }
  }
  if("Aggravated Assault" %in% variables){
    
    x <- filter(data, ucr_category == "Aggravated Assault") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("ast"))
    }
  }
  if("Burglary" %in% variables){
    
    x <- filter(data, ucr_category == "Burglary") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Larceny" %in% variables){
    
    x <- filter(data, ucr_category == "Larceny") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Vehicle Theft" %in% variables){
    
    x <- filter(data, ucr_category == "Vehicle Theft") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Arson" %in% variables){
    
    x <- filter(data, ucr_category == "Arson") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Simple Assault" %in% variables){
    
    x <- filter(data, ucr_category == "Simple Assault") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Forgery" %in% variables){
    
    x <- filter(data, ucr_category == "Forgery") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Fraud" %in% variables){
    
    x <- filter(data, ucr_category == "Fraud") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Embezzlement" %in% variables){
    
    x <- filter(data, ucr_category == "Embezzlement") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Stolen Property" %in% variables){
    
    x <- filter(data, ucr_category == "Stolen Property") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Destruction of Property" %in% variables){
    
    x <- filter(data, ucr_category == "Destruction of Property") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Weapons Offense" %in% variables){
    
    x <- filter(data, ucr_category == "Weapons Offense") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Sex offense" %in% variables){
    
    x <- filter(data, ucr_category == "Sex offense") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("VMCSL" %in% variables){
    
    x <- filter(data, ucr_category == "VMCSL") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Offense Against Family" %in% variables){
    
    x <- filter(data, ucr_category == "Offense Against Family") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("DWI/DUI" %in% variables){
    
    x <- filter(data, ucr_category == "DWI/DUI") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Liquor Laws" %in% variables){
    
    x <- filter(data, ucr_category == "Liquor Laws") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Disorderly Conduct" %in% variables){
    
    x <- filter(data, ucr_category == "Disorderly Conduct") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Loitering/Begging" %in% variables){
    
    x <- filter(data, ucr_category == "Loitering/Begging") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }
  if("Other" %in% variables){
    
    x <- filter(data, ucr_category == "Other") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("undef"))
    }
  }

  
  return(map)
}

# add demographic layer
addDemographic <- function(map, variable, demog, boundary){
  # using Jenks Natural Breaks
  inc_pal   <- colorBin("viridis", domain = 0:75000, bins = c(0,22880,32609,45375,58786,74425))
  pov_pal   <- colorBin("viridis", domain = 0:100, bins = c(0,14,24,35,46,62))
  hs_pal    <- colorBin("viridis", domain = 0:100, bins = c(0,71,79,86,91,99))
  ba_pal    <- colorBin("viridis", domain = 0:100, bins = c(0,15,29,47,61,78))
  unemp_pal <- colorBin("viridis", domain = 0:100, bins = c(0,6,11,18,26,36))
  home_pal  <- colorBin("viridis", domain = 0:100, bins = c(0,19,38,51,67,86))
  # add demographic layer
       if(variable == "None"){                       map %<>% addPolygons(data = boundary, fill = NA)}
  else if(variable == "Median Income"){         map %<>% addPolygons(data = demog, fillColor = ~inc_pal(med_income), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  else if(variable == "Poverty Rate"){          map %<>% addPolygons(data = demog, fillColor = ~pov_pal(pov_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  else if(variable == "High School Attainment"){map %<>% addPolygons(data = demog, fillColor = ~hs_pal(hs_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  else if(variable == "Bachelors Attainment"){  map %<>% addPolygons(data = demog, fillColor = ~ba_pal(ba_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  else if(variable == "Unemployment Rate"){     map %<>% addPolygons(data = demog, fillColor = ~unemp_pal(unemploy_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  else if(variable == "Home Ownership"){        map %<>% addPolygons(data = demog, fillColor = ~home_pal(home_own_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  
  return(map)
}

# add environmental points
addEnvironment <- function(map, variables, data, r = 7){
  # add environment variables
  if("Venues" %in% variables){        map %<>% addPolygons(data = data[[1]], fillColor = "blue", stroke = NA, popup = data[[1]]$name)}
  if("Parks" %in% variables){         map %<>% addPolygons(data = data[[2]], fillColor = "green", stroke = NA, popup = data[[2]]$name)}
  if("Zones" %in% variables){         map %<>% addPolygons(data = data[[3]], color = "red", fill = NA, popup = "Hayden's Rectangle") %>% addPolygons(data = data[[4]], color = "red", fill = NA, popup = "The Wedge")}
  
  if("ATMs" %in% variables){          map %<>% addCircleMarkers(data = data[[5]], radius = r,stroke = NA, popup = data[[5]]$name, fillColor = colorDict("atm"))}
  if("Bars" %in% variables){          map %<>% addCircleMarkers(data = data[[6]], radius = r,stroke = NA, popup = data[[6]]$name, fillColor = colorDict("bar"))}
  if("Clubs" %in% variables){         map %<>% addCircleMarkers(data = data[[7]], radius = r,stroke = NA, popup = data[[7]]$name, fillColor = colorDict("clb"))}
  if("Liquor Stores" %in% variables){ map %<>% addCircleMarkers(data = data[[8]], radius = r,stroke = NA, popup = data[[8]]$name, fillColor = colorDict("liq"))}
  if("Gas Stations" %in% variables){  map %<>% addCircleMarkers(data = data[[9]], radius = r,stroke = NA, popup = data[[9]]$name, fillColor = colorDict("gas"))}
  if("Grocery Stores" %in% variables){map %<>% addCircleMarkers(data = data[[10]], radius = r,stroke = NA, popup = data[[10]]$name, fillColor = colorDict("grc"))}
  if("Bus Stops" %in% variables){     map %<>% addCircleMarkers(data = data[[11]], radius = r,stroke = NA, fillColor = colorDict("bus"), fillOpacity = .25)}
  if("Schools" %in% variables){       map %<>% addCircleMarkers(data = data[[12]], radius = r,stroke = NA, popup = data[[12]]$name, fillColor = colorDict("scl"), fillOpacity = .45)}
  #TODO get data if("Vacancy" %in% input$env_chk){       leaf %>% addCircleMarkers(data = vacancy) -> leaf}
  
  return(map)
}