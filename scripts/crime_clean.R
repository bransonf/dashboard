# Creating a Crime Database from 2018-May2019 data

library(readr)
library(dplyr)

crimes <- vector("list", 17)

crimes[[1]] <- read_csv("../raw_data/crime_data/January2018.CSV")
crimes[[2]] <- read_csv("../raw_data/crime_data/February2018.CSV")
crimes[[3]] <- read_csv("../raw_data/crime_data/March2018.CSV")
crimes[[4]] <- read_csv("../raw_data/crime_data/April2018.CSV")
crimes[[5]] <- read_csv("../raw_data/crime_data/May2018.CSV")
crimes[[6]] <- read_csv("../raw_data/crime_data/June2018.CSV")
crimes[[7]] <- read_csv("../raw_data/crime_data/July2018.CSV")
crimes[[8]] <- read_csv("../raw_data/crime_data/August2018.CSV")
crimes[[9]] <- read_csv("../raw_data/crime_data/September2018.CSV")
crimes[[10]] <- read_csv("../raw_data/crime_data/October2018.CSV")
crimes[[11]] <- read_csv("../raw_data/crime_data/November2018.CSV")
crimes[[12]] <- read_csv("../raw_data/crime_data/December2018.CSV")
crimes[[13]] <- read_csv("../raw_data/crime_data/January2019.CSV")
crimes[[14]] <- read_csv("../raw_data/crime_data/February2019.CSV")
crimes[[15]] <- read_csv("../raw_data/crime_data/March2019.CSV")
crimes[[16]] <- read_csv("../raw_data/crime_data/April2019.CSV")
crimes[[17]] <- read_csv("../raw_data/crime_data/May2019.CSV")


crimes <- bind_rows(crimes)

crimes <- transmute(crimes,
                    year = lubridate::year(lubridate::mdy_hm(DateOccur)),
                    month = lubridate::month(lubridate::mdy_hm(DateOccur)),
                    neighborhood = Neighborhood,
                    district = District,
                    crime_code = Crime,
                    lon = XCoord,
                    lat = YCoord
) %>%
  filter(year %in% c(2018, 2019))

library(sf)

# make an sf object and reproject to latitude and longitude coordinates

crime_sf <- st_as_sf(crimes, coords = c("lon", "lat"), crs = 102696) %>%
  st_transform(4326)
crime_sf <- mutate(crime_sf,
                   lon = st_coordinates(crime_sf)[,1],
                   lat = st_coordinates(crime_sf)[,2]) %>%
            filter(lon > -90.31919 & lon < -90.17727 & lat > 38.53155 & lat < 38.77524) %>%
            select(- lon, - lat)

# need to create a neighborhood lookup table and a UCR categorization (Ignoring Description var)

districts <- st_read("../raw_data/STL-Police-Districts-2014-2/STL POLICE DISTRICTS/GIS.STL.POLICE_DISTRICTS_2014.shp", crs = 102696) %>%
  st_transform(4326) %>%
  transmute(district = as.numeric(DISTNO))
nbhoods <- st_read("../raw_data/nbrhds_wards/nbrhds_wards/BND_Nhd88_cw.shp", crs = 102696) %>%
  st_transform(4326) %>%
  transmute(neighborhood = NHD_NUM,
            name = as.character(NHD_NAME))

rape     <- filter(crime_sf, crime_code >= 20000 & crime_code < 30000)
homicide <- filter(crime_sf, crime_code >= 10000 & crime_code < 20000)
rob      <- filter(crime_sf, crime_code >= 30000 & crime_code < 40000)
assault  <- filter(crime_sf, crime_code >= 40000 & crime_code < 50000)



# point and heat map will use sf, hood/district will use crime obj

save(districts, nbhoods, file = "../dashboard/bounds.rda")
save(crimes, crime_sf, rape, homicide, rob, assault, file = "../dashboard/crimes.rda")
