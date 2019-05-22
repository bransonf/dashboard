# Dashboard User Interface

library(shiny)
library(leaflet)

# Set Text Here
cardiff <- HTML("<h3>The Cardiff Model</h3>", "The Department of ... found that over half of violent crimes go unreported to police. By combining police and hospital data, key stakeholders from policy makers to public health experts can more accurately assess violence in the region.")


shinyUI(fluidPage(
    
    # Application title
    headerPanel("Cardiff Dashboard"),
    # Add a Row
    fluidRow(
        # Add Columns Within Row
        column(9, leafletOutput("map")),
        column(3, HTML("<h5>Select Data to Map:</h5></br>"),
               checkboxGroupInput("crime_chk", "Crime",
                                    choices = c("Homicide", "Rape", "Robbery", "Assault")),
               checkboxGroupInput("inj_chk", "Violent Injury",
                                    choices = c("Gun Shot", "Stabbing", "Rape")),
               selectInput("demog_chk", "Demographic",
                                    choices = c("Income", "Poverty", "High School or Above", "Bachelors or Above", "None"),
                            selected = "None"),
               submitButton("Submit")
        )
    ),
    fluidRow(
        column(6, cardiff),
        column(6,
            dateRangeInput("date_rng", "Select a Date Range"),
            sliderInput("age_slide", "Select an Age Range:", 13, 100, value = c(13, 100), step = 1)
        )
    )
))
