# Dashboard User Interface

library(shiny)
library(leaflet)

# Set Text Here
cardiff <- HTML("<h3 class=heading>The Cardiff Model</h3>", "<p class=sans> The U.S. Department of Justice found that more than half of violent crime in the United States goes unreported to law enforcement. This is a clear limitation to understanding where violence occurs. The Cardiff Model combines police and hospital data on violence. As a result, key stakeholders from policy makers to public health experts can more accurately assess violence in the community. <a href='https://www.cdc.gov/violenceprevention/publichealthissue/fundedprograms/cardiffmodel/whatis.html'>Read More at the CDC</a></p>")

shinyUI(fluidPage(
    
    # This links to the CSS stylesheet
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    # Application title
    headerPanel(HTML("<h1 class=title>Cardiff Dashboard</h1>")),
    # Add a Row
    fluidRow(
        # Add Columns Within Row
        column(9, leafletOutput("map", height = "500px"), cardiff),
        column(3, HTML("<h5 class=heading>Select Data to Map:</h5>"),
               checkboxGroupInput("crime_chk", "Crime",
                                    choices = c("Homicide", "Rape", "Robbery", "Assault")),
               checkboxGroupInput("inj_chk", "Violent Injury",
                                    choices = c("Gun Shot", "Stabbing", "Rape")),
               selectInput("demog_select", "Demographic",
                                    choices = c("Median Income", "Poverty Rate", "High School Attainment", "Bachelors Attainment", "Unemployment Rate", "Home Ownership", "None"),
                            selected = "None"),
               checkboxInput("legend", "Show Legend"),
               dateRangeInput("date_rng", "Select a Date Range:"),
               sliderInput("age_slide", "Select an Age Range:", 13, 100, value = c(13, 100), step = 1),
               submitButton("Submit")
        )
    )
))
