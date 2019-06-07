# custom point legend function for leaflet
# draws raw styled SVG and adds it as an HTML element
# built as a pipe-able leaflet function to add custom legend based on vector of attributes (name and color)
# size is dictated in pixels

addCircleLegend <- function(map, size, text, color, position){
  
  # error for unequal lengths
  if(length(text) != length(color)){stop("Color and Text arguments should be of equal length")}
  
  # initialize background
        # leg <- paste0('<div style="padding: 6px 8px;
        #        font: 14px/16px Arial, Helvetica, sans-serif;
        #        background: white;
        #        background: rgba(255,255,255,0.8);
        #        box-shadow: 0 0 15px rgba(0,0,0,0.2);
        #        border-radius: 5px;
        #        line-height: 18px;
        #        color:#555;">')
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
  
  # add each element
  leg <- shiny::HTML(paste0(leg, '</div>'))

  return(leaflet::addControl(map, html = leg, position))
  
}


