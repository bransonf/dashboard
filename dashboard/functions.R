# GLOBAL FUNCTIONS FOR APP

## Constants
apiURL <- "api.stldata.org/stlcrime/"

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
colorDict <- function(key, all){
    key <- which(key == all)
  switch (key,
            "#5F4690","#1D6996","#38A6A5","#0F8554","#73AF48","#EDAD08","#E17C05","#CC503E","#94346E","#6F4070","#994E95","#666666","#88CCEE","#CC6677","#DDCC77","#117733","#332288","#AA4499","#44AA99","#999933","#882255","#661100","#6699CC","#888888","#E58606","#5D69B1","#52BCA3","#99C945","#CC61B0","#24796C","#DAA51B","#2F8AC4","#764E9F","#ED645A","#CC3A8E","#A5AA99#7F3C8D","#11A579","#3969AC","#F2B701","#E73F74","#80BA5A","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99"
    )
}

# define palette dictionary (For demographic data.. for now)
palDict <- function(key){
  switch(key,
         # Demographic uses Jenks Natural Breaks
  inc   = colorBin("viridis", domain = 0:75000, bins = c(0,22880,32609,45375,58786,74425)),
  pov   = colorBin("viridis", domain = 0:100, bins = c(0,14,24,35,46,62)),
  hs    = colorBin("viridis", domain = 0:100, bins = c(0,71,79,86,91,99)),
  ba    = colorBin("viridis", domain = 0:100, bins = c(0,15,29,47,61,78)),
  unemp = colorBin("viridis", domain = 0:100, bins = c(0,6,11,18,26,36)),
  home  = colorBin("viridis", domain = 0:100, bins = c(0,19,38,51,67,86))
  )
}

# define bins dictionary
binDict <- function(region, crime){
  if(region == "Neighborhoods"){
    bins = switch (crime,
                   "Homicide" = c(0,1,2,3,6,Inf),
                   "Rape" = c(0,1,2,3,5,Inf),
                   "Robbery" = c(0,2,5,9,27,Inf),
                   "Aggravated Assault" = c(0,4,9,17,59,Inf),
                   "Burglary" = c(0,5,13,29,83,Inf),
                   "Larceny" = c(0,12,30,65,184,Inf),
                   "Vehicle Theft" = c(0,4,9,18,72, Inf),
                   "Arson" = c(0,1,2,4,8,Inf),
                   "Simple Assault" = c(0,4,9,17,42,Inf),
                   "Forgery" = c(0,1,2,7,20,Inf),
                   "Fraud" = c(0,3,44,259,392,Inf),
                   "Embezzlement" = c(0,1,2,4,15,Inf),
                   "Stolen Property" = c(0,1,2,4,9,Inf),
                   "Destruction of Property" = c(0,4,9,17,47,Inf),
                   "Weapons Offense" = c(0,1,2,5,19,Inf),
                   "Sex Offense" = c(0,1,4,8,18,Inf),
                   "VMCSL" = c(0,2,6,12,38,Inf),
                   "Offense Against Family" = c(0,1,2,3,Inf),
                   "DWI/DUI" = c(0,1,2,4,12,Inf),
                   "Liquor Laws" = c(0,3,10,25,52,Inf),
                   "Disorderly Conduct" = c(0,2,5,10,30,Inf),
                   "Loitering/Begging" = c(0,5,17,40,93,Inf),
                   "Other" = c(0,6,16,34,81,Inf)
                   
    )
  }
  else if(region == "Police Districts"){
    bins = switch (crime,
                   "Homicide" = c(0,2,4,7,16,Inf),
                   "Rape" = c(0,1,3,5,11,Inf),
                   "Robbery" = c(0,13,24,35,64,Inf),
                   "Aggravated Assault" = c(0,19,40,65,117,Inf),
                   "Burglary" = c(0,37,66,106,188,Inf),
                   "Larceny" = c(0,60,143,219,361,Inf),
                   "Vehicle Theft" = c(0,25,50,85,152,Inf),
                   "Arson" = c(0,2,5,9,18,Inf),
                   "Simple Assault" = c(0,19,42,63,120,Inf),
                   "Forgery" = c(0,2,4,7,22,Inf),
                   "Fraud" = c(0,8,44,259,392,Inf),
                   "Embezzlement" = c(0,1,3,8,17,Inf),
                   "Stolen Property" = c(0,3,7,12,21,Inf),
                   "Destruction of Property" = c(0,24,51,80,149,Inf),
                   "Weapons Offense" = c(0,5,10,18,37,Inf),
                   "Sex Offense" = c(0,3,7,13,26,Inf),
                   "VMCSL" = c(0,12,23,35,69,Inf),
                   "Offense Against Family" = c(0,1,2,3,4,Inf),
                   "DWI/DUI" = c(0,3,6,12,24,Inf),
                   "Liquor Laws" = c(0,7,20,41,74,Inf),
                   "Disorderly Conduct" = c(0,8,14,22,42,Inf),
                   "Loitering/Begging" = c(0,11,33,62,123,Inf),
                   "Other" = c(0,34,78,130,232,Inf)
    )
  }
  return(bins)
}

# define basemap url and attribution
basemap <- function(input){
  bm <- switch(input,
               "Satellite" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
               "Terrain"   = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}{r}.png",
               "No Labels" = "https://{s}.basemaps.cartocdn.com/rastertiles/light_nolabels/{z}/{x}/{y}{r}.png",
               "Dark"      = "https://{s}.basemaps.cartocdn.com/rastertiles/dark_nolabels/{z}/{x}/{y}{r}.png")
  at <- switch(input,
               "Satellite" = "Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community",
               "Terrain"   = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
               "No Labels" = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
               "Dark"      = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>')
  
  
  return(list(bm = bm, at = at))
}

# initialize a leaflet map to build from
leafInit <- function(bm, at){
  leaflet(options = leafletOptions(zoomSnap = 0.5)) %>%
    enableTileCaching() %>%
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
addPoints <- function(map, lon, lat, radius = 7, stroke = NA, fillColor, fillOpacity = .5){
  
    addCircleMarkers(map, lon, lat, radius, stroke = stroke, fillColor = fillColor, fillOpacity = fillOpacity)

}

# larger batch function for adding crime points
addCrimePoints <- function(map, variables, data, all){
  # remove non points from the all vector
  if("Parks" %in% all){
    all <- all[-which(all == "Parks")]
  }
  if("Venues" %in% all){
    all <- all[-which(all == "Venues")]
  }
  if("Zones" %in% all){
    all <- all[-which(all == "Zones")]
  }
  
  if("Homicide" %in% variables){
    
    x <- filter(data, ucr_category == "Homicide") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Homicide", all))
    }
  }
  if("Rape" %in% variables){
    
    x <- filter(data, ucr_category == "Rape")
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Rape", all))
    }
  }
  if("Robbery" %in% variables){
    
    x <- filter(data, ucr_category == "Robbery") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Robbery", all))
    }
  }
  if("Aggravated Assault" %in% variables){
    
    x <- filter(data, ucr_category == "Aggravated Assault") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Aggravated Assault", all))
    }
  }
  if("Burglary" %in% variables){
    
    x <- filter(data, ucr_category == "Burglary") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Burglary", all))
    }
  }
  if("Larceny" %in% variables){
    
    x <- filter(data, ucr_category == "Larceny") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Larceny", all))
    }
  }
  if("Vehicle Theft" %in% variables){
    
    x <- filter(data, ucr_category == "Vehicle Theft") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Vehicle Theft", all))
    }
  }
  if("Arson" %in% variables){
    
    x <- filter(data, ucr_category == "Arson") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Arson", all))
    }
  }
  if("Simple Assault" %in% variables){
    
    x <- filter(data, ucr_category == "Simple Assault") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Simple Assault", all))
    }
  }
  if("Forgery" %in% variables){
    
    x <- filter(data, ucr_category == "Forgery") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Forgery",all))
    }
  }
  if("Fraud" %in% variables){
    
    x <- filter(data, ucr_category == "Fraud") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Fraud",all))
    }
  }
  if("Embezzlement" %in% variables){
    
    x <- filter(data, ucr_category == "Embezzlement") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Embezzlement",all))
    }
  }
  if("Stolen Property" %in% variables){
    
    x <- filter(data, ucr_category == "Stolen Property") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Stolen Property",all))
    }
  }
  if("Destruction of Property" %in% variables){
    
    x <- filter(data, ucr_category == "Destruction of Property") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Destruction of Property",all))
    }
  }
  if("Weapons Offense" %in% variables){
    
    x <- filter(data, ucr_category == "Weapons Offense") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Weapons Offense",all))
    }
  }
  if("Sex offense" %in% variables){
    
    x <- filter(data, ucr_category == "Sex offense") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Sex Offense", all))
    }
  }
  if("VMCSL" %in% variables){
    
    x <- filter(data, ucr_category == "VMCSL") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("VMCSL",all))
    }
  }
  if("Offense Against Family" %in% variables){
    
    x <- filter(data, ucr_category == "Offense Against Family") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Offense Against Family",all))
    }
  }
  if("DWI/DUI" %in% variables){
    
    x <- filter(data, ucr_category == "DWI/DUI") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("DWI/DUI",all))
    }
  }
  if("Liquor Laws" %in% variables){
    
    x <- filter(data, ucr_category == "Liquor Laws") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Liquor Laws",all))
    }
  }
  if("Disorderly Conduct" %in% variables){
    
    x <- filter(data, ucr_category == "Disorderly Conduct") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Disorderly Conduct",all))
    }
  }
  if("Loitering/Begging" %in% variables){
    
    x <- filter(data, ucr_category == "Loitering/Begging") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Loitering/Begging",all))
    }
  }
  if("Other" %in% variables){
    
    x <- filter(data, ucr_category == "Other") %>%
          filter(!is.na(wgs_x) & !is.na(wgs_y))
    
    if(length(x$wgs_x) > 0){
    map %<>% addPoints(lon = x$wgs_x, lat = x$wgs_y, fillColor = colorDict("Other",all))
    }
  }

  
  return(map)
}

# add demographic layer
addDemographic <- function(map, variable, demog, boundary){
  # add demographic layer
       if(variable == "None"){                       map %<>% addPolygons(data = boundary, fill = NA)}
  else if(variable == "Median Income"){         map %<>% addPolygons(data = demog, fillColor = ~palDict("inc")(med_income), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  else if(variable == "Poverty Rate"){          map %<>% addPolygons(data = demog, fillColor = ~palDict("pov")(pov_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  else if(variable == "High School Attainment"){map %<>% addPolygons(data = demog, fillColor = ~palDict("hs")(hs_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  else if(variable == "Bachelors Attainment"){  map %<>% addPolygons(data = demog, fillColor = ~palDict("ba")(ba_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  else if(variable == "Unemployment Rate"){     map %<>% addPolygons(data = demog, fillColor = ~palDict("unemp")(unemploy_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  else if(variable == "Home Ownership"){        map %<>% addPolygons(data = demog, fillColor = ~palDict("home")(home_own_pct), weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.5)}
  
  return(map)
}

# add environmental points
addEnvironment <- function(map, variables, data, all, r = 7){
  # remove non points from the all vector
  if("Parks" %in% all){
    all <- all[-which(all == "Parks")]
  }
  if("Venues" %in% all){
    all <- all[-which(all == "Venues")]
  }
  if("Zones" %in% all){
    all <- all[-which(all == "Zones")]
  }
  
  # add environment variables
  if("Venues" %in% variables){        map %<>% addPolygons(data = data[[1]], fillColor = "blue", stroke = NA, popup = data[[1]]$name)}
  if("Parks" %in% variables){         map %<>% addPolygons(data = data[[2]], fillColor = "green", stroke = NA, popup = data[[2]]$name)}
  if("Zones" %in% variables){         map %<>% addPolygons(data = data[[3]], color = "red", fill = NA, popup = "Hayden's Rectangle") %>% addPolygons(data = data[[4]], color = "red", fill = NA, popup = "The Wedge")}
  
  if("ATMs" %in% variables){          map %<>% addCircleMarkers(data = data[[5]],  radius = r,stroke = NA, popup = data[[5]]$name,  fillOpacity = .5, fillColor = colorDict("ATMs", all))}
  if("Bars" %in% variables){          map %<>% addCircleMarkers(data = data[[6]],  radius = r,stroke = NA, popup = data[[6]]$name,  fillOpacity = .5, fillColor = colorDict("Bars",all))}
  if("Clubs" %in% variables){         map %<>% addCircleMarkers(data = data[[7]],  radius = r,stroke = NA, popup = data[[7]]$name,  fillOpacity = .5, fillColor = colorDict("Clubs",all))}
  if("Liquor Stores" %in% variables){ map %<>% addCircleMarkers(data = data[[8]],  radius = r,stroke = NA, popup = data[[8]]$name,  fillOpacity = .5, fillColor = colorDict("Liquor Stores",all))}
  if("Gas Stations" %in% variables){  map %<>% addCircleMarkers(data = data[[9]],  radius = r,stroke = NA, popup = data[[9]]$name,  fillOpacity = .5, fillColor = colorDict("Gas Stations",all))}
  if("Hotels" %in% variables){        map %<>% addCircleMarkers(data = data[[10]], radius = r,stroke = NA, popup = data[[10]]$name, fillOpacity = .5, fillColor = colorDict("Hotels",all))}
  if("Bus Stops" %in% variables){     map %<>% addCircleMarkers(data = data[[11]], radius = r/2,stroke = NA,                        fillOpacity = .25, fillColor = colorDict("Bus Stops", all))}
  if("Schools" %in% variables){       map %<>% addCircleMarkers(data = data[[12]], radius = r,stroke = NA, popup = data[[12]]$name, fillOpacity = .5, fillColor = colorDict("Schools",all))}
  
  return(map)
}

# add crime legend
crimeLegend <- function(map, crime, all){
  # remove non points from the all vector
  if("Parks" %in% all){
    all <- all[-which(all == "Parks")]
  }
  if("Venues" %in% all){
    all <- all[-which(all == "Venues")]
  }
  if("Zones" %in% all){
    all <- all[-which(all == "Zones")]
  }
  
  cols <-  sapply(crime, colorDict, all = all, USE.NAMES = FALSE)
  map %<>% addCircleLegend(10, crime, cols, "topleft")
}

# add demographic legend
demogLegend <- function(map, demog, variable){
  p <- switch (variable, "Median Income" = palDict("inc"), "Poverty Rate" = palDict("pov"), "High School Attainment" = palDict("hs"), "Bachelors Attainment" = palDict("ba"), "Unemployment Rate" = palDict("unemp"), "Home Ownership" = palDict("home"))
  v <- switch (variable, "Median Income" = demog$med_income, "Poverty Rate" = demog$pov_pct, "High School Attainment" = demog$hs_pct, "Bachelors Attainment" = demog$ba_pct, "Unemployment Rate" = demog$unemploy_pct, "Home Ownership" = demog$home_own_pct)
  t <- switch (variable, "Median Income" = "Median Income</br>(2017 Dollars)", "Poverty Rate" = "Poverty Rate %", "High School Attainment" = "High School</br>Attainment %", "Bachelors Attainment" = "Bachelors</br>Attainment %", "Unemployment Rate" = "Unemployment</br>Rate %", "Home Ownership" = "Home</br>Ownership %")
  
 map %<>% addLegend("topleft", pal = p, values = v, opacity = .5, title = t)
}

# add environment legend
envLegend <- function(map, env, all){
  # if not any other than these, NULL
  if(all(env %in% c("Parks", "Venues", "Zones"))){
    return(map)
  }else{  
    # remove non points from the all and env vectors
    if("Parks" %in% env){
      env <- env[-which(env == "Parks")]
    }
    if("Venues" %in% env){
      env <- env[-which(env == "Venues")]
    }
    if("Zones" %in% env){
      env <- env[-which(env == "Zones")]
    }
    if("Parks" %in% all){
      all <- all[-which(all == "Parks")]
    }
    if("Venues" %in% all){
      all <- all[-which(all == "Venues")]
    }
    if("Zones" %in% all){
      all <- all[-which(all == "Zones")]
    }
    
    cols <-  sapply(env, colorDict, all = all, USE.NAMES = FALSE)
    map %<>% addCircleLegend(10, env, cols, "topleft")
  }
}

# add choropleth legend
choroLegend <- function(map, data, variable, region){
  pal <- colorBin("YlGnBu", bins = binDict(region, variable),
                  domain = data[[variable]])
  # if last character is a Y
  plural <- ifelse(substr(variable, nchar(variable), nchar(variable)) == "y", 
                   paste0(substr(variable, 1, nchar(variable) - 1), "ies"),
                   paste0(variable,"s"))
  
  title = paste0("Number of ", plural)
  map %<>% addLegend("topleft", pal, data[[variable]], title = title)
    
  return(map)
}

# generate labels
genLabs <- function(data, variable){
  value <- data[[variable]]
  
  labs <- paste0("<h4>", data$name, "</h4>",
                 "<b>", variable,": </b>", value) %>% lapply(shiny::HTML)
}

# add choropleth layer
choropleth <- function(map, data, variable, region){
  pal <- colorBin("YlGnBu", bins = binDict(region, variable),
                  domain = data[[variable]])
  
  map %<>% addPolygons(data = data, label = genLabs(data, variable),
              fillColor = ~pal(data[[variable]]),
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

# render a UI slider element with most recent month
monthSliderUI <- function(sel_year, max_month, label){
  if(sel_year == as.numeric(format(Sys.Date(), "%Y"))){
    sliderTextInput(label, "Select a Month:", month.name[1:which(month.name == max_month)], max_month)
  }
  else{
    sliderTextInput(label, "Select a Month:", month.name, max_month)
  }
}

# call and parse region crime
regionCrime <- function(region, month, year, gun, hood, dist){
  if(region == "Police Districts"){
    
    crime <- api_call(apiURL, paste0("district",
                                     "?month=", month,
                                     "&year=", year,
                                     "&gun=",  ifelse(gun, 'true', 'false'))) %>%
      dplyr::mutate(district = as.numeric(district)) %>%
      dplyr::left_join(dist, ., by = "district") %>%
      tidyr::spread("ucr_category", "Incidents")
    
    crime[is.na(crime)] <- 0
    
    return(crime)
  }
  else if(region == "Neighborhoods"){
    
    crime <- api_call(apiURL, paste0("nbhood",
                                     "?month=",month,
                                     "&year=", year,
                                     "&gun=",  ifelse(gun, 'true', 'false'))) %>%
      dplyr::mutate(neighborhood = as.numeric(neighborhood)) %>%
      dplyr::left_join(hood, ., by = "neighborhood") %>%
      tidyr::spread("ucr_category", "Incidents")
    
    crime[is.na(crime)] <- 0
    
    return(crime)
  }
}

# gun filter dynamic element
gunFiltUI <- function(input, label){
  if(any(input %in% c("Homicide", "Robbery", "Aggravated Assault"))){
    checkboxInput(label, "Filter for Gun Crimes")
  }else{
    NULL
  }
}

# custom download button
downloadImage <- function(outputId, label = "Save Image", class = NULL, ...){
  aTag <- tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", 
                                              class), href = "", target = "_blank", download = NA, 
                 icon("image"), label, ...)
}

