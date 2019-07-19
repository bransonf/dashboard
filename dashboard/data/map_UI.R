## Map Component UI
# Basic Map (Choropleth)
basMapUI <- function(){
  column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
         selectInput("bas_base", "Basemap", c("Satellite" ,"Terrain", "No Labels", "Dark"), selected = "Terrain"),
         pickerInput("bas_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burglary", "Larceny", "Vehicle Theft", "Arson", "Simple Assault", "Forgery", "Fraud", "Embezzlement", "Stolen Property", "Destruction of Property", "Weapons Offense", "Sex Offense", "VMCSL", "Offense Against Family", "DWI/DUI", "Liquor Laws", "Disorderly Conduct", "Loitering/Begging", "Other"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = FALSE),
         uiOutput("bas_gunf"),
         sliderTextInput("bas_year", "Select a Year:", 2008:2019, 2019),
         uiOutput("bas_month"),
         selectInput("bas_region", "Region",
                     choices = c("Police Districts", "Neighborhoods"),
                     selected = "Neighborhoods"),
         checkboxInput("bas_legend", "Show Legend"),
         downloadImage("bas_save")
  )
}

# Advanced Map (Point)
advMapUI <- function(){
  column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
         selectInput("adv_base", "Basemap", c("Satellite" ,"Terrain", "No Labels", "Dark"), selected = "Terrain"),
         pickerInput("adv_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burglary", "Larceny", "Vehicle Theft", "Arson", "Simple Assault", "Forgery", "Fraud", "Embezzlement", "Stolen Property", "Destruction of Property", "Weapons Offense", "Sex Offense", "VMCSL", "Offense Against Family", "DWI/DUI", "Liquor Laws", "Disorderly Conduct", "Loitering/Begging", "Other"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE),
         uiOutput("adv_gunf"),
         sliderTextInput("adv_year", "Select a Year:", 2008:2019, 2019),
         uiOutput("adv_month"),
         pickerInput("adv_env", "Environment",
                     choices = c("ATMs", "Bars", "Clubs", "Liquor Stores", "Gas Stations", "Hotels", "Bus Stops", "Schools", "Venues", "Parks", "Zones"),
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
         checkboxInput("adv_legend", "Show Legend(s)"),
         checkboxInput("adv_bunit", "Show Block Units"), # Block Units...
         downloadImage("adv_save")
  )
}

# Density Map
dnsMapUI <- function(){
  column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
         selectInput("dns_base", "Basemap", c("Satellite" ,"Terrain", "No Labels", "Dark"), selected = "Terrain"),
         pickerInput("dns_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burglary", "Larceny", "Vehicle Theft", "Arson", "Simple Assault", "Forgery", "Fraud", "Embezzlement", "Stolen Property", "Destruction of Property", "Weapons Offense", "Sex Offense", "VMCSL", "Offense Against Family", "DWI/DUI", "Liquor Laws", "Disorderly Conduct", "Loitering/Begging", "Other"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE),
         uiOutput("dns_gunf"),
         sliderTextInput("dns_year", "Select a Year:", 2008:2019, 2019),
         uiOutput("dns_month"),
         sliderInput("dns_size", "Heat Size", 30, 120, 90, 5),
         downloadImage("dns_save")
  )
}

# Side by Side Map
sbsMapUI <- function(){
  column(3,
         sliderTextInput("sbs_year", "Select a Year:", 2008:2019, 2019),
         uiOutput("sbs_month"),
         checkboxInput("sbs_legend", "Show Legends"),
         HTML("<h4 class=heading style='font-weight:bold;text-decoration:underline'>Left Map:</h4>"),
         selectInput("sbs_baseL", "Basemap", c("Satellite" ,"Terrain", "No Labels", "Dark"), selected = "Terrain"),
         pickerInput("sbs_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burglary", "Larceny", "Vehicle Theft", "Arson", "Simple Assault", "Forgery", "Fraud", "Embezzlement", "Stolen Property", "Destruction of Property", "Weapons Offense", "Sex Offense", "VMCSL", "Offense Against Family", "DWI/DUI", "Liquor Laws", "Disorderly Conduct", "Loitering/Begging", "Other"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE),
         uiOutput("sbs_gunf"),
         HTML("<h4 class=heading style='font-weight:bold;text-decoration:underline'>Right Map:</h4>"),
         selectInput("sbs_baseR", "Basemap", c("Satellite" ,"Terrain", "No Labels", "Dark"), selected = "Terrain"),
         pickerInput("sbs_env", "Environment",
                     choices = c("ATMs", "Bars", "Clubs", "Liquor Stores", "Gas Stations", "Hotels", "Bus Stops", "Schools", "Venues", "Parks", "Zones"),
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