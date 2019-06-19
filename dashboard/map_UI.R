## Map Component UI
##TODO Add better isolation (Independent Namespaces per https://shiny.rstudio.com/articles/modules.html)
# Basic Map (Choropleth)
basMapUI <- function(cur_month){
  column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
         selectInput("bas_base", "Basemap", c("Terrain", "No Labels"), selected = "Terrain"),
         pickerInput("bas_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Assault"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE),
         checkboxInput("bas_gun", "Filter for Gun Crimes"),
         pickerInput("bas_inj", "Violent Injury",
                     choices = c("Gun Shot *", "Stabbing *", "Rape *"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE
         ),
         radioButtons("bas_year", "Select a Year:", c(2018, 2019), 2019, inline = TRUE),
         sliderTextInput("bas_month", "Select a Month:", month.name, cur_month),
         
         selectInput("bas_region", "Region",
                     choices = c("Police Districts", "Wards", "Neighborhoods"),
                     selected = "Neighborhoods"),
         checkboxInput("bas_legend", "Show Legend"),
         checkboxInput("bas_popups", "Advanced Popups")
  )
}

# Advanced Map (Point)
advMapUI <- function(cur_month){
  column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
         selectInput("adv_base", "Basemap", c("Terrain", "No Labels"), selected = "Terrain"),
         pickerInput("adv_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Assault"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE),
         checkboxInput("adv_gun", "Filter for Gun Crimes"),
         pickerInput("adv_inj", "Violent Injury",
                     choices = c("Gun Shot *", "Stabbing *", "Rape *"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE
         ),
         radioButtons("adv_year", "Select a Year:", c(2018, 2019), 2019, inline = TRUE),
         sliderTextInput("adv_month", "Select a Month:", month.name, cur_month),
         
         pickerInput("adv_env", "Environment",
                     choices = c("ATMs", "Bars", "Clubs", "Liquor Stores", "Gas Stations", "Grocery Stores", "Bus Stops", "Schools", "Vacancy *", "Venues", "Parks"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE
         ),
         selectInput("adv_demog", "Demographic",
                     choices = c("Median Income", "Poverty Rate", "High School Attainment", "Bachelors Attainment", "Unemployment Rate", "Home Ownership", "None"),
                     selected = "None"),
         checkboxInput("adv_legend", "Show Legend(s)")
         
  )
}

# Density Map
dnsMapUI <- function(cur_month){
  column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
         selectInput("dns_base", "Basemap", c("Terrain", "No Labels"), selected = "Terrain"),
         pickerInput("dns_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Assault"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE),
         checkboxInput("dns_gun", "Filter for Gun Crimes"),
         pickerInput("dns_inj", "Violent Injury",
                     choices = c("Gun Shot *", "Stabbing *", "Rape *"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE
         ),
         radioButtons("dns_year", "Select a Year:", c(2018, 2019), 2019, inline = TRUE),
         sliderTextInput("dns_month", "Select a Month:", month.name, cur_month)
  )
}

# Side by Side Map
sbsMapUI <- function(cur_month){
  column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
         selectInput("base4", "Basemap", c("Terrain", "No Labels"), selected = "Terrain"),
         pickerInput("crime_chk4", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Assault"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE),
         checkboxInput("gun4", "Filter for Gun Crimes"),
         pickerInput("inj_chk4", "Violent Injury",
                     choices = c("Gun Shot *", "Stabbing *", "Rape *"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE
         ),
         pickerInput("env_chk4", "Environment",
                     choices = c("ATMs", "Bars", "Clubs", "Liquor Stores", "Gas Stations", "Grocery Stores", "Bus Stops", "Schools", "Vacancy *", "Venues", "Parks"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE
         ),
         selectInput("demog_select4", "Demographic",
                     choices = c("Median Income", "Poverty Rate", "High School Attainment", "Bachelors Attainment", "Unemployment Rate", "Home Ownership", "None"),
                     selected = "None"),
         checkboxInput("heatmap4", "Draw Heatmap"),
         checkboxInput("legend4", "Show Legend(s)"),
         radioButtons("year4", "Select a Year:", c(2018, 2019), 2019, inline = TRUE),
         sliderTextInput("month4", "Select a Month:", month.name, cur_month, animate = TRUE),
         fluidRow(
           #column(1, submitButton("Update")),
           #column(1, offset = 3, dropdownButton(nav, icon = icon("question"), size = "sm", right = TRUE, up = TRUE))
         )
         
  )
}