# Run this to rebuild all of the dependencies (Copy, timedata)

source("scripts/copy.R")
source("scripts/crime.R")
source("scripts/time_data.R")
source("scripts/crime_clean.R")

# Need to minify custom functions and create file in /dashboard/

# Need to copy and push to seperate minimum repo and push the changes to the shiny server
# IE Semi-continuous integration, or one click rebuilds...