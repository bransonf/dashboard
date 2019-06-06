# NOW DEPRECATED IN FAVOR OF RDATA FILE FORMAT
#
# get demographic data and save to json for dashboard

library(tidycensus)
library(sf)
library(dplyr)

vars <- load_variables(2017, "acs5")

vars <- c(
  "B01003_001", # Total Population
  ## Race
  "B02001_001", # Race Total
  "B02001_002", # White Alone
  "B02001_003", # Black Alone
  ## Education
  "B15001_001", # Total, Educational Attainment for 18+
  "B15001_006", # Male 18-24 HS
  "B15001_007", # Some College
  "B15001_008", # Associates
  "B15001_009", # Bachelors
  "B15001_010", # Graduate +
  "B15001_014", # Male 25-34 HS
  "B15001_015", # Some College
  "B15001_016", # Associates
  "B15001_017", # Bachelors
  "B15001_018", # Graduate +
  "B15001_022", # Male 35-44 HS
  "B15001_023", # Some College
  "B15001_024", # Associates
  "B15001_025", # Bachelors
  "B15001_026", # Graduate +
  "B15001_030", # Male 45-64 HS
  "B15001_031", # Some College
  "B15001_032", # Associates
  "B15001_033", # Bachelors
  "B15001_034", # Graduate +
  "B15001_038", # Male 65 + HS
  "B15001_039", # Some College
  "B15001_040", # Associates
  "B15001_041", # Bachelors
  "B15001_042", # Graduate +
  "B15001_047", # Female 18-24 HS
  "B15001_048", # Some College
  "B15001_049", # Associates
  "B15001_050", # Bachelors
  "B15001_051", # Graduate +
  "B15001_055", # Female 25-34 HS
  "B15001_056", # Some College
  "B15001_057", # Associates
  "B15001_058", # Bachelors
  "B15001_059", # Graduate +
  "B15001_063", # Female 35-44 HS
  "B15001_064", # Some College
  "B15001_065", # Associates
  "B15001_066", # Bachelors
  "B15001_067", # Graduate +
  "B15001_071", # Female 45-64 HS
  "B15001_072", # Some College
  "B15001_073", # Associates
  "B15001_074", # Bachelors
  "B15001_075", # Graduate +
  "B15001_079", # Female 65 + HS
  "B15001_080", # Some College
  "B15001_081", # Associates
  "B15001_082", # Bachelors
  "B15001_083", # Graduate +
  ## Poverty
  "B17001_001", # Total, Poverty Status
  "B17001_002", # Below the Poverty Line
  ## Income
  "B19013_001", # Median Household Income
  ## Employment
  "B23025_002", # Total In Labor Force
  "B23025_005", # Unemployed in Labor Force
  ## Home Ownership
  "B25003_001", # Total Housing Units (Tenure)
  "B25003_002" # Owner Occupied
)


census <- get_acs("tract", vars, year = 2017, output = "wide", state = 29, county = 510, geometry = TRUE) %>%
  transmute(pop = B01003_001E,
            blk_pct = B02001_003E/B02001_001E * 100,
            wht_pct = B02001_002E/B02001_001E * 100,
            hs_pct = (B15001_006E + B15001_007E + B15001_008E +
              B15001_009E + B15001_010E + B15001_014E + B15001_015E + B15001_016E + B15001_017E + B15001_018E + B15001_022E +
              B15001_023E + B15001_024E + B15001_025E + B15001_026E + B15001_030E + B15001_031E + B15001_032E + B15001_033E +
              B15001_034E + B15001_038E + B15001_039E + B15001_040E + B15001_041E + B15001_042E + B15001_047E + B15001_048E +
              B15001_049E + B15001_050E + B15001_051E + B15001_055E + B15001_056E + B15001_057E + B15001_058E + B15001_059E +
              B15001_063E + B15001_064E + B15001_065E + B15001_066E + B15001_067E + B15001_071E + B15001_072E + B15001_073E +
              B15001_074E + B15001_075E + B15001_079E + B15001_080E + B15001_081E + B15001_082E + B15001_083E)/B15001_001E * 100,
            ba_pct = (B15001_008E +
              B15001_009E + B15001_010E + B15001_016E + B15001_017E + B15001_018E +
              B15001_024E + B15001_025E + B15001_026E + B15001_032E + B15001_033E +
              B15001_034E + B15001_040E + B15001_041E + B15001_042E +
              B15001_049E + B15001_050E + B15001_051E + B15001_057E + B15001_058E + B15001_059E +
              B15001_065E + B15001_066E + B15001_067E + B15001_073E +
              B15001_074E + B15001_075E + B15001_081E + B15001_082E + B15001_083E)/B15001_001E * 100,
            pov_pct = B17001_002E/B17001_001E * 100,
            med_income = B19013_001E,
            unemploy_pct = B23025_005E/B23025_002E * 100,
            home_own_pct = B25003_002E/B25003_001E * 100
            )

st_write(census, "../dashboard/stl_demog.geojson")