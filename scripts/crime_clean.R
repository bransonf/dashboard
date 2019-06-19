# Creating a Crime Database from 2018-May2019 data

library(readr)
library(dplyr)

# reproducible way
# wd = getwd()
# setwd("../raw_data/crime_data")
# all_files <- Reduce(rbind, lapply(list.files(), read_csv))
# setwd(wd)

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
                    lat = YCoord,
                    homicide = ifelse(Crime >= 10000 & Crime < 20000, TRUE, FALSE),
                    rape = ifelse(Crime >= 20000 & Crime < 30000, TRUE, FALSE),
                    robbery = ifelse(Crime >= 30000 & Crime < 40000, TRUE, FALSE),
                    assault = ifelse(Crime >= 40000 & Crime < 50000, TRUE, FALSE),
                    gun = ifelse(Crime == 10000 | (Crime > 41000 & Crime < 42000) | Crime %in% c(31111, 31112,32111,32112,33111,34111,35111,35112,36112,37111,37112,38111,38112), TRUE, FALSE)
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
wards <- st_read("../raw_data/nbrhds_wards/nbrhds_wards/POL_WRD_2010_Prec.shp", crs = 102696) %>%
  st_transform(4326) %>%
  transmute(ward = WARD10,
            precinct = PREC10)



# point and heat map will use sf, hood/district will use crime obj

save(districts, nbhoods, wards, file = "../dashboard/bounds.rda")
save(crimes, crime_sf, file = "../dashboard/crimes.rda")


# 01 Homicides
# 02 Rape
# 03 Robbery
# 04 Aggravated Assault

