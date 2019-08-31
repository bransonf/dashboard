## Map Component UI
# Basic Map (Choropleth)
basMapUI <- function(){
  column(12,
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
         airMonthpickerInput("bas_date", "Month", latest_data + 1,
                              minDate = "2008-01-02",
                              maxDate = latest_data + 1),
         selectInput("bas_region", "Region",
                     choices = c("Police Districts", "Neighborhoods"),
                     selected = "Neighborhoods"),
         checkboxInput("bas_legend", "Show Legend"),
         downloadImage("bas_save")
  )
}

# Advanced Map (Point)
advMapUI <- function(){
  column(12,
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
         airDatepickerInput("adv_date", "Time Period", selectDays(latest_data),
                              range = TRUE,
                              minDate = "2008-01-02",
                              maxDate = latest_data + 1),
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
  column(12,
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
         airDatepickerInput("dns_date", "Time Period", selectDays(latest_data),
                            range = TRUE,
                            minDate = "2008-01-02",
                            maxDate = latest_data + 1),
         sliderInput("dns_size", "Heat Radius", 5, 30, 20, 5),
         sliderInput("dns_blur", "Heat Blur", 5, 50, 15, 1),
         downloadImage("dns_save")
  )
}

# Side by Side Map
sbsMapUI <- function(){
  fluidRow(
    column(4, offset = 1,
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
           airDatepickerInput("sbs_date", "Time Period", selectDays(latest_data),
                              range = TRUE,
                              minDate = "2008-01-02",
                              maxDate = latest_data + 1),
           checkboxInput("sbs_legend", "Show Legends")
    ),
    column(4, offset = 1,
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
                       selected = "None")
    )
  )
}
## Mobile Specific User Interface

mobMapUI <- function(){
  column(12,
         selectInput("mob_base", "Basemap", c("Satellite" ,"Terrain", "No Labels", "Dark"), selected = "Terrain"),
         pickerInput("mob_crime", "Crime",
                     choices = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burglary", "Larceny", "Vehicle Theft", "Arson", "Simple Assault", "Forgery", "Fraud", "Embezzlement", "Stolen Property", "Destruction of Property", "Weapons Offense", "Sex Offense", "VMCSL", "Offense Against Family", "DWI/DUI", "Liquor Laws", "Disorderly Conduct", "Loitering/Begging", "Other"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE),
         uiOutput("mob_gunf"),
         airDatepickerInput("mob_date", "Time Period", selectDays(latest_data),
                            range = TRUE,
                            minDate = "2008-01-02",
                            maxDate = latest_data + 1),
         pickerInput("mob_env", "Environment",
                     choices = c("ATMs", "Bars", "Clubs", "Liquor Stores", "Gas Stations", "Hotels", "Bus Stops", "Schools", "Venues", "Parks", "Zones"),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 10,
                       `selected-text-format` = "count > 3"
                     ), 
                     multiple = TRUE
         ),
         selectInput("mob_demog", "Demographic",
                     choices = c("Median Income", "Poverty Rate", "High School Attainment", "Bachelors Attainment", "Unemployment Rate", "Home Ownership", "None"),
                     selected = "None"),
         checkboxInput("mob_legend", "Show Legend(s)")
  )
}
