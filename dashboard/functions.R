# These should be minified custom functions for loading the web application

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