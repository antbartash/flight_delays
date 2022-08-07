library(tidyverse)

setwd("C:\\Users\\olial\\OneDrive\\Desktop\\Desktop\\Flights\\data\\FAA Registration Database")



##### FLIGHTS, AIRCRAFTS, MODELS #####
### some flight delays may be correlated with technical issues with aircrafts
### --> need to use info about aircrafts


# read the database of currently registered aircrafts 
# and select only essential columns
MASTER <- read_csv("MASTER.txt")
n_distinct(MASTER$`N-NUMBER`)   # all N-NUMBERs are unique
MASTER <- MASTER %>% select(
  `N-NUMBER`, `MFR MDL CODE`,`SERIAL NUMBER`, `ENG MFR MDL`, `YEAR MFR`, 
  `LAST ACTION DATE`, `CERT ISSUE DATE`, `AIR WORTH DATE`,
  `EXPIRATION DATE`
)
master_colnames <- c(
  "n_number", "mfr_mdl_code", "serial_number", "eng_mfr_code",
  "year_mfr", "last_act_date", "cert_issue_date",
  "air_worth_date", "exp_date"
)
colnames(MASTER) <- master_colnames


# read the database of deregistered aircrafts 
# and select only essential columns
DEREG <- read_csv("DEREG.txt")
DEREG <- DEREG %>% select(
  `N-NUMBER`, `SERIAL-NUMBER`, `MFR-MDL-CODE`, `ENG-MFR-MDL`, `YEAR-MFR`, 
  `AIR-WORTH-DATE`, `CANCEL-DATE`, `LAST-ACT-DATE`, `CERT-ISSUE-DATE`
)
dereg_colnames <- c("n_number", "serial_number", "mfr_mdl_code", "eng_mfr_mdl",
                    "year_mfr", "air_worth_date", "cancel_date", 
                    "last_act_date", "cert_issue_date")
colnames(DEREG) <- dereg_colnames


# read the data about aircraft models 
# and select only essential columns
ACFTREF <- read_csv("ACFTREF.txt")
n_distinct(ACFTREF$CODE) # brak powtórzeń
ACFTREF <- ACFTREF %>% select(
  -`BUILD-CERT-IND`, -`TC-DATA-SHEET`, -`TC-DATA-HOLDER`, -`...14`
)
acftref_colnames <- c("mfr_mdl_code", "mfr", "model", 'type_acft', "type_eng", 
                      "ac_cat", "n_eng", "n_seats", "ac_weigth", "speed")
colnames(ACFTREF) <- acftref_colnames


# merge currently registered aircrafts with deregistered ones
colnames(DEREG)[7] <- 'exp_date'
colnames(MASTER)[4] <- 'eng_mfr_mdl'
planes <- bind_rows(MASTER, DEREG)


# read the database of 2000000 US domestic flights
# and select only essential columns
flights <- read_csv("C:\\Users\\olial\\OneDrive\\Desktop\\Desktop\\Flights\\data\\airline_2m.csv")
flights <- flights %>% select(
  Year, FlightDate, Reporting_Airline, Tail_Number,
  Flight_Number_Reporting_Airline,
  OriginAirportID, Origin, OriginCityMarketID, OriginCityName,
  DestAirportID, Dest, DestCityMarketID, DestCityName,
  OriginState, DestState,
  DepTime, DepDelay, DepDel15, DepartureDelayGroups,
  TaxiOut, WheelsOff, WheelsOn, TaxiIn,
  ArrTime, ArrDelay, ArrDel15, ArrivalDelayGroups,
  Cancelled, CancellationCode, Diverted,
  ActualElapsedTime, AirTime, Distance, DistanceGroup,
  CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay,
  DivReachedDest)


# format planes' tail number as in flights df
planes$n_number <- paste0("N", planes$n_number)


# change the type of features containing dates from numeric to date
library(lubridate)
planes$last_act_date <- ymd(planes$last_act_date)
planes$cert_issue_date <- ymd(planes$cert_issue_date)
planes$air_worth_date <- ymd(planes$air_worth_date)
planes$exp_date <- ymd(planes$exp_date)


# merge flights data with aircrafts data
colnames(flights)[4] <- 'n_number'
flights <- left_join(flights, planes, by = "n_number")


# same n-number for different aircrafts in planes df 
# --> 400K duplicates in flights df
# remove duplicates (flights with the same date, time and flight number)
flights <- flights[(!duplicated(flights[c("FlightDate", "DepTime", "Flight_Number_Reporting_Airline")])), ]


# merge flights with plane's model info
flights <- left_join(flights, ACFTREF)




##### AIRPORT COORDINATES #####
### since some flight delays may be correlated with weather conditions,
### need to use weather data --> 
### need to use airport and weather station coords to locate 
### nearest weather station and download weather data
setwd("C:\\Users\\olial\\OneDrive\\Desktop\\Desktop\\Flights\\data")

### AIRPORT COORDINATES

# load the data of airports coordinates and select only essential features
airports <- read_csv("weather/us-airports.csv")
airports <- airports[2:nrow(airports),] # remove the 1st row (comments)
airports <- airports %>% 
  select(iata_code, latitude_deg, longitude_deg)


# add coordinates of a destination airport for each flight
airports <- rename(airports, 
                   dest_iata_code = iata_code,
                   dest_latitude_deg = latitude_deg,
                   dest_longitude_deg = longitude_deg)
flights_w_airports <- left_join(flights, airports, 
                                by=c("Dest" = "dest_iata_code"))

# check missing values level - about 0.005
sum(is.na(flights_w_airports$dest_latitude_deg))/nrow(flights_w_airports)


# add coordinates of origin airport for each flight
airports <- rename(airports, 
                   origin_iata_code = dest_iata_code,
                   origin_latitude_deg = dest_latitude_deg,
                   origin_longitude_deg = dest_longitude_deg)
flights_w_airports <- left_join(flights_w_airports, airports, 
                                by=c("Origin" = "origin_iata_code"))


# drop unnecessary dfs
rm(ACFTREF, DEREG, MASTER, airports, planes, flights)


# create a df with unique origin airport coordinates
airport_coords <- 
  unique(flights_w_airports[, c('origin_latitude_deg', 'origin_longitude_deg')])
airport_coords <- airport_coords %>% 
  rename(.,
         'latitude' = 'origin_latitude_deg',
         'longitude' = 'origin_longitude_deg')

airport_coords <- na.omit(airport_coords) # remove 1 NA obs.

# change coordinates dtype from str to num
airport_coords$latitude <- as.numeric(airport_coords$latitude)
airport_coords$longitude <- as.numeric(airport_coords$longitude)



##### WEATHER STATION COORDINATES #####

# package to download weather data
library(riem)

# select only the US weather station networks
networks <- riem_networks()
states_codes <- c("AL", "AK", "AS", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", 
                  "FL", "GA", "GU", "HI", "ID", "IL", "IN", "IA", "KS", "KY",
                  "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MN", "NE",
                  "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "MP", "OH", "OK",
                  "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT",
                  "VA", "VI", "WA", "WV", "WI", "WY")
states_codes <- paste0(states_codes, "_ASOS")
networks <- networks[networks$code %in% states_codes, ]
states_codes <- networks$code

# find all stations for every US network
stations <- data.frame()
for (i in 1:length(states_codes)) {
  network <- states_codes[i]
  stations <- bind_rows(stations, riem_stations(network))
  cat(i, '\n')
}
stations <- stations %>% select(id, archive_begin, longitude, latitude)

unique(stations[, c('longitude', 'latitude')]) # all stations in the stations df are unique



### FINDING CLOSEST WEATHER STATION

# for each airport find weather station with the same coordinates
airport_stations <- left_join(airport_coords, stations)


# find weather station which is located close to the airport
# (gradually lowering the precision of coordinates)

airport_stations$latitude_full <- airport_stations$latitude
airport_stations$longitude_full <- airport_stations$longitude
airport_stations_filled <- filter(airport_stations, !is.na(id))


for (i in c(5, 4, 3, 2, 1, 0)) {
  
  # lower the precision of coordinates
  airport_stations$latitude <- round(airport_stations$latitude, i)
  airport_stations$longitude <- round(airport_stations$longitude, i)
  stations$latitude <- round(stations$latitude, i)
  stations$longitude <- round(stations$longitude, i)
  
  # find a weather station
  airport_stations <- left_join(airport_stations, stations,
                                by = c("latitude", "longitude"))
  airport_stations <- airport_stations %>% 
    select(-`id.x`, -`archive_begin.x`) %>% 
    rename('id' = 'id.y',
           'archive_begin' = 'archive_begin.y')
  
  # add weather station info to another df
  airport_stations_filled <- bind_rows(airport_stations_filled,
                                       filter(airport_stations, !is.na(id)))
  
  # drop observations where weather station info has been found on that level of precision
  airport_stations <- filter(airport_stations, is.na(id))
  
  cat(i, '\n')
}

# remove duplicates, select and rename necessary columns
airport_stations_filled <- airport_stations_filled[
  (!duplicated(airport_stations_filled[c('latitude_full', 'longitude_full')])), ]
airport_stations_filled <-  airport_stations_filled %>% 
  select(id, latitude_full, longitude_full, archive_begin) %>%
  rename('latitude' = 'latitude_full',
         'longitude' = 'longitude_full')



### ADD WEATHER STATION INFO TO EACH FLIGHT

# change coordinates dtype from str to num in flights_w_airports df
flights_w_airports$dest_latitude_deg <- as.numeric(flights_w_airports$dest_latitude_deg)
flights_w_airports$dest_longitude_deg <- as.numeric(flights_w_airports$dest_longitude_deg)
flights_w_airports$origin_latitude_deg <- as.numeric(flights_w_airports$origin_latitude_deg)
flights_w_airports$origin_longitude_deg <- as.numeric(flights_w_airports$origin_longitude_deg)


# add weather station info for a destination airport
flights_w_weather <- 
  left_join(flights_w_airports, airport_stations_filled,
            by = c("dest_latitude_deg" = "latitude",
                   "dest_longitude_deg" = "longitude")) %>% 
  rename('dest_weather_id' = 'id', 
         'dest_weather_begin' = 'archive_begin')

# add weather station info for an origin airport
flights_w_weather <- 
  left_join(flights_w_weather, airport_stations_filled,
            by = c("origin_latitude_deg" = "latitude",
                   "origin_longitude_deg" = "longitude")) %>% 
  rename('origin_weather_id' = 'id', 
         'origin_weather_begin' = 'archive_begin')


# check missing values percentage -- about 0.008
sum(is.na(flights_w_weather$origin_weather_id)) / nrow(flights_w_weather)
sum(is.na(flights_w_weather$dest_weather_id)) / nrow(flights_w_weather)


# write_csv(flights_w_weather, 'flights_with_weather.csv')



##### DOWNLOAD WEATHER DATA #####

# create a new df with weather data
flights_new <- flights_w_weather

# select only necessary columns and flights since 2004
flights_new <- flights_new %>% select(
  Year, FlightDate, Reporting_Airline, Origin, OriginCityName,
  DepTime, DepDel15, Distance, year_mfr, cert_issue_date, 
  mfr, model, type_acft, type_eng, n_eng, n_seats, origin_weather_id
)
flights_new <- flights_new %>% filter(Year >= 2004)

# select first 110K flights (observations are not ordered --> no need to shuffle)
flights_new <- flights_new[1:110000,]


# check missing values percentage in flight dates
sum(is.na(flights_new$FlightDate)) / nrow(flights_new)  # 0 NA
sum(is.na(flights_new$DepTime)) / nrow(flights_new)     # 0.017 NA
sum(is.na(flights_new$origin_weather_id)) / nrow(flights_new) # 0 NA

# cannot find weather data withount time and date --> filter out obs with NAs
flights_new <- flights_new %>% 
  filter(!is.na(DepTime),
         !is.na(origin_weather_id))
flights_new <- as.data.frame(flights_new)


### DF FOR WEATHER DATA
#weather <- data.frame(matrix(NA, ncol = 7, nrow = nrow(flights_new)))  # new df
weather <- read.csv('weather.csv') # backup df


for (i in 1:nrow(flights_new)){
  
  # download weather data for needed date and weather station
  station = flights_new$origin_weather_id[i]
  date =  flights_new$FlightDate[i]
  measures =  riem_measures(station, date_start = date, date_end = date)
  
  # skip the observation if weather data if not available
  if (is.null(measures)){
    next
  }
  
  # select only needed measures
  measures = measures %>% 
    select(valid, tmpf, dwpf, relh, sknt, gust, vsby) %>% 
    mutate(minutes = minute(valid),
           hour = hour(valid))
  
  # round flight and weather measure hour
  flight_hour = as.numeric(substr(flights_new$DepTime[i], 1, 2))
  flight_minutes = as.numeric(substr(flights_new$DepTime[i], 3, 4))
  flight_hour = ifelse(flight_minutes > 30, flight_hour + 1, flight_hour)
  measures$hour <- ifelse(measures$minutes > 30, measures$hour + 1, measures$hour)
  
  # select weather observation for needed hour
  measures <- measures %>% 
    select(-valid, -minutes) %>% 
    filter(hour == flight_hour)
  
  # check if hour is available
  if (length(measures$hour) == 0){
    next
  }
  
  # if multiple observation for needed hour -> select the first one
  if (nrow(measures) > 1) {
    measures <- measures[1,]
  }
  
  # add weather info to the weather df
  weather[i,] <-  measures
  
  # checkpoint every 1000 observations
  if (i %% 1000 == 0) {write_csv(weather, 'weather.csv')}
  cat(i, "\n")
}

# save downloaded weather data
# write_csv(weather, 'weather.csv')

# select and rename needed columns
weather <- select(weather, -X7)
colnames(weather) <- c('origin_tmpf', 'origin_dwpf', 'origin_relh', 
                       'origin_sknt', 'origin_gust', 'origin_vsby')



# add weather info to flights info
flights_new <- bind_cols(flights_new, weather)
flights_new <- flights_new %>% select(-origin_weather_id, -origin_gust)



##### HOLIDAYS #####
### some flight delays may be correlated with holidays

holidays <- read_csv('US Holiday Dates (2004-2021).csv')
holidays <- holidays %>% select(Date, Holiday)

flights_new <- left_join(flights_new, holidays, by=c("FlightDate"='Date'))



# remove observations without DepDel15 (cannot be used for delay analysis)
flights_new <- filter(flights_new, !is.na(DepDel15))

# save the df with all data
write_csv(flights_new, 'flights_full.csv')
