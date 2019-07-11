## Map Component UI
##TODO Add better isolation (Independent Namespaces per https://shiny.rstudio.com/articles/modules.html)
# Basic Map (Choropleth)
basMapUI <- function(){
  column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
         selectInput("bas_base", "Basemap", c("Satellite" ,"Terrain", "No Labels"), selected = "Terrain"),
         pickerInput("bas_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burglary *", "Larceny *", "Vehicle Theft *", "Arson *", "Simple Assault *", "Forgery *", "Fraud *", "Embezzlement *", "Stolen Property *", "Destruction of Property *", "Weapons Offense *", "Sex Offense *", "VMCSL *", "Offense Against Family *", "DWI/DUI *", "Liquor Laws *", " Disorderly Conduct *", "Loitering/Begging *", "Other *"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = FALSE),
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
         sliderTextInput("bas_year", "Select a Year:", 2008:2019, 2019),
         uiOutput("bas_month"),
         selectInput("bas_region", "Region",
                     choices = c("Police Districts", "Neighborhoods"), # Maybe add wards later...
                     selected = "Neighborhoods"),
         checkboxInput("bas_legend", "Show Legend"),
         checkboxInput("bas_popups", "Popups (Better for Mobile)")
  )
}

# Advanced Map (Point)
advMapUI <- function(){
  column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
         selectInput("adv_base", "Basemap", c("Satellite" ,"Terrain", "No Labels"), selected = "Terrain"),
         pickerInput("adv_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burglary", "Larceny", "Vehicle Theft", "Arson", "Simple Assault", "Forgery", "Fraud", "Embezzlement", "Stolen Property", "Destruction of Property", "Weapons Offense", "Sex Offense", "VMCSL", "Offense Against Family", "DWI/DUI", "Liquor Laws", " Disorderly Conduct", "Loitering/Begging", "Other"),
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
         sliderTextInput("adv_year", "Select a Year:", 2008:2019, 2019),
         uiOutput("adv_month"),
         pickerInput("adv_env", "Environment",
                     choices = c("ATMs", "Bars", "Clubs", "Liquor Stores", "Gas Stations", "Grocery Stores", "Bus Stops", "Schools", "Vacancy *", "Venues", "Parks", "Zones"),
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
dnsMapUI <- function(){
  column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
         selectInput("dns_base", "Basemap", c("Satellite" ,"Terrain", "No Labels"), selected = "Terrain"),
         pickerInput("dns_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burglary", "Larceny", "Vehicle Theft", "Arson", "Simple Assault", "Forgery", "Fraud", "Embezzlement", "Stolen Property", "Destruction of Property", "Weapons Offense", "Sex Offense", "VMCSL", "Offense Against Family", "DWI/DUI", "Liquor Laws", " Disorderly Conduct", "Loitering/Begging", "Other"),
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
         sliderTextInput("dns_year", "Select a Year:", 2008:2019, 2019),
         uiOutput("dns_month"),
         sliderInput("dns_size", "Heat Size", 30, 120, 90, 5)
  )
}

# Side by Side Map
sbsMapUI <- function(){
  column(3,
         sliderTextInput("sbs_year", "Select a Year:", 2008:2019, 2019),
         uiOutput("sbs_month"),
         checkboxInput("sbs_legend", "Show Legends"),
         HTML("<h5 class=heading>Left Map:</h5>"),
         selectInput("sbs_baseL", "Basemap", c("Satellite" ,"Terrain", "No Labels"), selected = "Terrain"),
         pickerInput("sbs_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burglary", "Larceny", "Vehicle Theft", "Arson", "Simple Assault", "Forgery", "Fraud", "Embezzlement", "Stolen Property", "Destruction of Property", "Weapons Offense", "Sex Offense", "VMCSL", "Offense Against Family", "DWI/DUI", "Liquor Laws", " Disorderly Conduct", "Loitering/Begging", "Other"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE),
         pickerInput("sbs_inj", "Violent Injury",
                     choices = c("Gun Shot *", "Stabbing *", "Rape *"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE
         ),
         checkboxInput("sbs_gun", "Filter for Gun Crimes"),
         HTML("<h5 class=heading>Right Map:</h5>"),
         selectInput("sbs_baseR", "Basemap", c("Satellite" ,"Terrain", "No Labels"), selected = "Terrain"),
         pickerInput("sbs_env", "Environment",
                     choices = c("ATMs", "Bars", "Clubs", "Liquor Stores", "Gas Stations", "Grocery Stores", "Bus Stops", "Schools", "Vacancy *", "Venues", "Parks", "Zones"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE
         ),
         selectInput("sbs_demog", "Demographic",
                     choices = c("Median Income", "Poverty Rate", "High School Attainment", "Bachelors Attainment", "Unemployment Rate", "Home Ownership", "None"),
                     selected = "None"),
         fluidRow(
           #column(1, submitButton("Update")),
           #column(1, offset = 3, dropdownButton(nav, icon = icon("question"), size = "sm", right = TRUE, up = TRUE))
         )
         
  )
}