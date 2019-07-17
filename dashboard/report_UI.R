# report_UI

reportUI <- function(rep_info){
  column(12, align = 'center',
         HTML("<h4 class=sans>Select Options Below</h4>"),
         fluidRow(align = "center",
                  pickerInput("rep_table", "Table(s)",
                              choices = c("Summary Table","District Table", "Neighborhood Table"),
                              options = list(
                                `actions-box` = TRUE, 
                                size = 10,
                                `selected-text-format` = "count > 3"
                              ),multiple = TRUE
                  ),
                  pickerInput("rep_maps", "Map(s)",
                              choices = c("District Map", "Neighborhood Map", "Point Map", "Heat Map"),
                              options = list(
                                `actions-box` = TRUE, 
                                size = 10,
                                `selected-text-format` = "count > 3"
                              ),multiple = TRUE
                  ),
                  pickerInput("rep_crime", "Crime(s)",
                              choices = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burglary", "Larceny", "Vehicle Theft", "Arson", "Simple Assault", "Forgery", "Fraud", "Embezzlement", "Stolen Property", "Destruction of Property", "Weapons Offense", "Sex Offense", "VMCSL", "Offense Against Family", "DWI/DUI", "Liquor Laws", "Disorderly Conduct", "Loitering/Begging", "Other"),
                              options = list(
                                `actions-box` = TRUE,
                                size = 10,
                                `selected-text-format` = "count > 3"
                              ),multiple = TRUE
                  ),
                  sliderTextInput("rep_year", "Select a Year:", 2008:2019, 2019),
                  uiOutput("rep_month"),
                  HTML("<h4 class=sans>Generate Report</h4>"),
                  downloadButton('report', "Download")
         ),
         fluidRow(rep_info)
  )
}