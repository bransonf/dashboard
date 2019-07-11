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
            "ast" = "#888888"
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
                   "Assault" = c(0,5,10,15,30)
    )
  }
  else if(region == "Police Districts"){
    bins = switch (crime,
                   "Homicide" = c(0,1,5,10,20),
                   "Rape" = c(0,1,2,5,10),
                   "Robbery" = c(0,10,20,30,50),
                   "Assault" = c(0,25,50,75,100)
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
  
 get <- httr::GET(paste0(base, endpoint))
 parsed <- httr::content(get, as = "text", encoding = "utf-8") %>%
   jsonlite::fromJSON()
 
 # return the parsed json (dataframe..)
 return(parsed)
}
