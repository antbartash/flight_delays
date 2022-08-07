# Load the data and libraries

library(tidyverse)
library(lubridate)
library(zoo)
library(InformationValue)
library(smbinning)

setwd("C:\\Users\\olial\\OneDrive\\Desktop\\Desktop\\Flights\\data")

flights <- read_csv('flights_full.csv')



##### REPORTING_AIRLINE #####
### change dtype to a factor and calculate Information Value for the feature
flights$Reporting_Airline <- factor(flights$Reporting_Airline)
IV(factor(flights$Reporting_Airline), flights$DepDel15)



##### ORIGIN #####
### change dtype to a factor and calculate Information Value for the feature
### IV may be overestimated because of the high number of categories
flights$Origin <- factor(flights$Origin)
IV(flights$Origin, flights$DepDel15) 


### group origin airports with the number of observations at or below 250
### into one category ("Others") to decrease the risk of overfitting

# df for the number of observations for each origin airport
origin_n_flights <- flights %>% 
  select(Origin) %>% 
  group_by(Origin) %>% 
  summarise(origin_grouped = n()) %>% 
  arrange(desc(origin_grouped))
flights <- left_join(flights, origin_n_flights)

# change origin to category to 'Others' if count <= 250
flights$origin_grouped <- ifelse(flights$origin_grouped > 250,
                                 as.character(flights$Origin),
                                 'Others')
flights$origin_grouped <- factor(flights$origin_grouped)
IV(flights$origin_grouped, flights$DepDel15)



##### ORIGINCITYNAME #####
### variable may be useful if in some cities there are several airports
### and flight delays depends on a city rather than on airports
flights$OriginCityName <- factor(flights$OriginCityName)
IV(flights$OriginCityName, flights$DepDel15) # may be high because of the # of categories
### IV for OriginCityName is lower than for Origin



##### DISTANCE #####
ggplot(flights, aes(Distance)) + 
  geom_histogram(bins=50, fill='#4169e1') + 
  theme_classic()

# group to reduce the number of rare values and outliers
flights$Distance_grouped <- cut(flights$Distance, 
                                breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 10000))
flights$Distance_grouped <- factor(flights$Distance_grouped,
                                   labels = c('0-500', '500-1000', '1000-1500',
                                              '1500-2000', '2000-2500',
                                              '2500-3000', '>3000'))
IV(flights$Distance_grouped, flights$DepDel15)
smbinning(as.data.frame(flights), 'DepDel15', 'Distance')
# variable has low predictive power;
# smbinning cannot find any significant splits to group feature values


##### YEAR_MFR and AIRCRAFT_AGE  #####

### older planes may be more likely to brake
### use aircraft age in order to try to predict technical problems 

### calculate aircraft age based on flight year and year the plane was produced
### and handle missing values

table(flights$year_mfr)
flights$year_mfr <- ifelse(flights$year_mfr == 0, NA, flights$year_mfr)
table(flights$year_mfr)
flights$aircraft_age <- flights$Year - flights$year_mfr
table(flights$aircraft_age)
flights$aircraft_age <- ifelse(flights$aircraft_age < 0, NA, flights$aircraft_age)
table(flights$aircraft_age)
flights$aircraft_age <- round(flights$aircraft_age)

### check missing values percentage -- about 18%
sum(is.na(flights$aircraft_age)) / nrow(flights)

### fill missing aircraft age with a mean value for the airline planes
avg_age <- flights %>% 
  select(Reporting_Airline, aircraft_age) %>% 
  group_by(Reporting_Airline) %>% 
  summarise(mean_age_for_airline = mean(aircraft_age, na.rm=TRUE))
flights <- left_join(flights, avg_age)
flights$aircraft_age <- ifelse(is.na(flights$aircraft_age),
                               flights$mean_age_for_airline,
                               flights$aircraft_age)

IV(as.factor(flights$aircraft_age), flights$DepDel15)
smbinning(flights, 'DepDel15', 'aircraft_age')
### low Information value and no significant splits


##### MFR #####
IV(factor(flights$mfr), flights$DepDel15)

# list of the largest planes manufacturers
mfrs <- flights %>% 
  select(mfr) %>% 
  group_by(mfr) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
mfrs

n_distinct(flights$mfr)
### 417 categories, but some of them denote the same manufacturer
### --> group the same manufacturers and rare categories

mfrs %>% filter(substr(mfr, 1, 9) == 'CANADAIR')
flights$mfr_grouped <- ifelse(flights$mfr == 'CANADAIR', 'CANADAIR', 'Others')

mfrs %>% filter(substr(mfr, 1, 6) == 'BOEING')
flights$mfr_grouped <- ifelse(flights$mfr %in% c('BOEING', 'BOEING OF CANADA/DEHAV DIV'), 'BOEING', flights$mfr_grouped)

mfrs %>% filter(substr(mfr, 1, 10) == 'BOMBARDIER')
flights$mfr_grouped <- ifelse(flights$mfr == 'BOMBARDIER INC', 'BOMBARDIER', flights$mfr_grouped)

mfrs %>% filter(substr(mfr, 1, 7) == 'EMBRAER')
flights$mfr_grouped <- ifelse(flights$mfr %in% c('EMBRAER', 'EMBRAER S A', 'EMBRAER-EMPRESA BRASILEIRA DE'),
                              'EMBRAER', flights$mfr_grouped)

mfrs %>% filter(substr(mfr, 1, 6) == 'AIRBUS')
flights$mfr_grouped <- ifelse(flights$mfr %in% c('AIRBUS INDUSTRIE', 'AIRBUS',
                                                 'AIRBUS HELICOPTERS INC',
                                                 'AIRBUS CANADA LTD PTNRSP',
                                                 'AIRBUS HELICOPTERS',
                                                 'AIRBUS SAS'),
                              'AIRBUS', flights$mfr_grouped)

mfrs %>% filter(substr(mfr, 1, 9) == 'MCDONNELL')
flights$mfr_grouped <- ifelse(flights$mfr %in% c('MCDONNELL DOUGLAS', 
                                                 'MCDONNELL DOUGLAS AIRCRAFT CO',
                                                 'MCDONNELL DOUGLAS CORPORATION',
                                                 'MCDONNELL-DOUGLAS'),
                              'MCDONNELL DOUGLAS', flights$mfr_grouped)

mfrs %>% filter(substr(mfr, 1, 9) == 'CESSNA')
flights$mfr_grouped <- ifelse(flights$mfr == 'CESSNA', 'CESSNA', flights$mfr_grouped)

table(flights$mfr_grouped)
flights$mfr_grouped <- factor(flights$mfr_grouped)
IV(flights$mfr_grouped, flights$DepDel15)
### final variable has 8 categories, but low predictive power



##### MODEL #####
IV(factor(flights$model), flights$DepDel15)

# the most popular models
models <- flights %>% select(model) %>% 
  group_by(model) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n), model)

n_distinct(flights$model)
### 881 categories, but some of them denote variations of the same model
### group models and rare values

flights$model_grouped <- flights$model
flights$model_grouped <- ifelse(substr(flights$model, 1, 5) %in% c('737-7', '737-8', '737-9'),
                                '737NG', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 5) %in% c('737-3', '737-4', '737-5'),
                                '737Classic', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 5) %in% c('737-1', '737-2'),
                                '737', flights$model_grouped)

flights$model_grouped <- ifelse(substr(flights$model, 1, 4) == 'A320',
                                'A320', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 4) == 'A321',
                                'A321', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 4) == 'A319',
                                'A319', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 4) == 'A318',
                                'A318', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 4) == 'A330',
                                'A330', flights$model_grouped)

flights$model_grouped <- ifelse(substr(flights$model, 1, 6) == 'CL-600',
                                'CL-600', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 3) == '717',
                                '717', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 3) == '727',
                                '727', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 3) == '757',
                                '757', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 3) == '767',
                                '767', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 3) == '777',
                                '777', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 7) == 'EMB-145',
                                'EMB-145', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 7) == 'EMB-120',
                                'EMB-120', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 7) == 'EMB-135',
                                'EMB-135', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 5) == 'MD-88',
                                'MD-88', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 7) == 'ERJ 170',
                                'ERJ 170', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 7) == 'ERJ 190',
                                'ERJ 190', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 5) == 'MD-90',
                                'MD-90', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 4) == 'DC-9',
                                'DC-9', flights$model_grouped)
flights$model_grouped <- ifelse(flights$model %in% c('FALCON XP', 'FALCON-XP'),
                                'FALCON-XP', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 4) == 'SAAB',
                                'SAAB', flights$model_grouped)
flights$model_grouped <- ifelse(substr(flights$model, 1, 3) == '172',
                                'C172', flights$model_grouped)
flights$model_grouped <- ifelse(is.na(flights$model), 'NA', flights$model_grouped)

### group rare models into "Others" category

rare_models <- flights %>%
  select(model_grouped) %>%
  group_by(model_grouped) %>% 
  summarise(n = n()) %>% 
  filter(n > 140) %>% 
  select(model_grouped) %>% 
  as.vector()

flights$model_grouped <- ifelse(
  (flights$model_grouped %in% rare_models$model_grouped) | (is.na(flights$model_grouped)),
  flights$model_grouped, 'Others')
flights$model_grouped <- factor(flights$model_grouped)
IV(flights$model_grouped, flights$DepDel15)
### final variable has 26 categories, but low predictive power and about


sum(is.na(flights$model_grouped)) / nrow(flights) # about 10% NAs
### replace missing values using distribution of models of the airline
set.seed(0)

for (i in 1: nrow(flights)){
  
  # allows to track the progress
  if (i %% 1000 == 0){cat(i, '\n')}  
  
  # check if the model_grouped is missing
  if((flights$model_grouped)[i] != 'NA'){
    next
  }
  
  # create a sample df
  airline <- flights$Reporting_Airline[i]
  flights_airline <- flights %>% 
    select(Reporting_Airline, model_grouped) %>% 
    filter(Reporting_Airline == airline,
           model_grouped != 'NA')
  sample_df <- flights_airline[sample(nrow(flights_airline), 1), ]
  
  # check if the sample df is not empty
  if(nrow(sample_df) == 0) next
  
  # fill missing values
  flights$model_grouped[i] <- sample_df$model_grouped
}


flights %>% filter(model_grouped == 'NA')
view(flights %>% filter(model_grouped == 'NA'))
### missing values are still present (flights of Allegant Air (G4 category))
### but occur only in 218 observations and can be left as 'NA' string value


### drop useless features
flights <- flights %>% 
  select(-type_eng, -type_acft, -n_eng)



##### N_SEATS #####
### the more passengers on a flights, the higher may be risk of delay
### use number of seats in a plane as an 'estimator' of the number of passengers

# change variable dtype from character to numeric
flights$n_seats <- as.numeric(flights$n_seats)

# group feature values with smbinning (maximises IV)
smbinning(flights, 'DepDel15', 'n_seats')
flights$n_seats_grouped <- flights$n_seats
flights$n_seats_grouped <- ifelse(flights$n_seats > 80, ">80", flights$n_seats_grouped)
flights$n_seats_grouped <- ifelse(flights$n_seats <= 80, "<=80", flights$n_seats_grouped)
flights$n_seats_grouped <- ifelse(is.na(flights$n_seats), 'NA', flights$n_seats_grouped)
table(flights$n_seats_grouped)
flights$n_seats_grouped <- factor(flights$n_seats_grouped)
IV(flights$n_seats_grouped, flights$DepDel15)
# variable has low predictive power

table(flights$n_seats)
### missing values can be represented as 0, 999, NA
### fill missing values using distribution of the variable

flights$n_seats <- ifelse(((flights$n_seats == 999) | (flights$n_seats == 0) | (is.na(flights$n_seats))), 
                          flights[sample(nrow(flights), 1), which(colnames(flights) == 'n_seats')],
                          flights$n_seats)
IV(flights$n_seats, flights$DepDel15)
# variable has low predictive power 


##### MISSING WEATHER DATA #####

# missing values percentages
sum(is.na(flights$origin_tmpf)) / nrow(flights)
sum(is.na(flights$origin_dwpf)) / nrow(flights)
sum(is.na(flights$origin_relh)) / nrow(flights)
sum(is.na(flights$origin_sknt)) / nrow(flights)
sum(is.na(flights$origin_vsby)) / nrow(flights)

### fill missing values with mean
flights <- flights %>% 
  mutate(origin_tmpf = replace_na(origin_tmpf, mean(origin_tmpf, na.rm = TRUE)),
         origin_dwpf = replace_na(origin_dwpf, mean(origin_dwpf, na.rm = TRUE)),
         origin_relh = replace_na(origin_relh, mean(origin_relh, na.rm = TRUE)),
         origin_sknt = replace_na(origin_sknt, mean(origin_sknt, na.rm = TRUE)),
         origin_vsby = replace_na(origin_vsby, mean(origin_vsby, na.rm = TRUE)))



##### ORIGIN_TMPF #####
### grouping is based on the Power BI plot
flights$origin_tmpf_grouped <- factor(ifelse(flights$origin_tmpf <= 82, "<=82", '>82'))
table(flights$origin_tmpf_grouped)
IV(flights$origin_tmpf_grouped, flights$DepDel15)
# variable has low predictive power


##### ORIGIN_DWPF #####
### grouping is based on the Power BI plot
flights$origin_dwpf_grouped <- ifelse(flights$origin_dwpf <= 18, '<=18', flights$origin_dwpf)
flights$origin_dwpf_grouped <- ifelse(((flights$origin_dwpf > 18) & (flights$origin_dwpf <= 48)),
                                      "18-48", flights$origin_dwpf_grouped)
flights$origin_dwpf_grouped <- ifelse((flights$origin_dwpf > 48),
                                      ">48", flights$origin_dwpf_grouped)
flights$origin_dwpf_grouped <- factor(flights$origin_dwpf_grouped,
                                      levels = c('<=18', '18-48', '>48'),
                                      ordered = TRUE)
table(flights$origin_dwpf_grouped)
IV(flights$origin_dwpf_grouped, flights$DepDel15)
# variable has low predictive power


##### ORIGIN_RELH #####
summary(flights$origin_relh)
smbinning(as.data.frame(flights), 'DepDel15', 'origin_relh')

### grouping is based on smbinning results (maximizing IV)
flights$origin_relh_grouped <- ifelse(flights$origin_relh <= 30, "<=30", flights$origin_relh)
flights$origin_relh_grouped <- ifelse((flights$origin_relh > 30) & (flights$origin_relh <= 57),
                                      '30-57', flights$origin_relh_grouped)
flights$origin_relh_grouped <- ifelse(flights$origin_relh > 57, ">57", flights$origin_relh_grouped)
table(flights$origin_relh_grouped)
flights$origin_relh_grouped <- factor(flights$origin_relh_grouped, 
                                      levels = c('<=30', '30-57', '>57'),
                                      ordered = TRUE)
IV(flights$origin_relh_grouped, flights$DepDel15)
# variable has low predictive power


##### ORIGIN_SKNT #####
smbinning(as.data.frame(flights), 'DepDel15', 'origin_sknt')

### grouping is based on smbinning results (maximizing IV)
flights$origin_sknt_grouped <- ifelse(flights$origin_sknt <= 1, "<=1", flights$origin_sknt)
flights$origin_sknt_grouped <- ifelse((flights$origin_sknt > 1) & (flights$origin_sknt <= 4.82),
                                      '1-4.82', flights$origin_sknt_grouped)
flights$origin_sknt_grouped <- ifelse((flights$origin_sknt > 4.82) & (flights$origin_sknt <= 6.7),
                                      '4.82-6.7', flights$origin_sknt_grouped)
flights$origin_sknt_grouped <- ifelse((flights$origin_sknt > 6.7) & (flights$origin_sknt <= 10.79),
                                      '6.7-10.79', flights$origin_sknt_grouped)
flights$origin_sknt_grouped <- ifelse((flights$origin_sknt > 10.79) & (flights$origin_sknt <= 14.36),
                                      '10.79-14.36', flights$origin_sknt_grouped)
flights$origin_sknt_grouped <- ifelse(flights$origin_sknt > 14.36, ">14.36", flights$origin_sknt_grouped)
flights$origin_sknt_grouped <- factor(flights$origin_sknt_grouped,
                                      levels = c('<=1', '1-4.82', '4.82-6.7',
                                                 '6.7-10.79', '10.79-14.36',
                                                 '>14.36'),
                                      ordered = TRUE)
table(flights$origin_sknt_grouped)



##### ORIGIN_VSBY #####
smbinning(as.data.frame(flights), 'DepDel15', 'origin_vsby') # No significant splits
IV(as.factor(flights$origin_vsby), flights$DepDel15)

### grouping is based on the plot in Power BI
flights$origin_vsby_grouped <- ifelse(flights$origin_vsby >= 10, '>=10', flights$origin_vsby)
flights$origin_vsby_grouped <- ifelse(((flights$origin_vsby < 10) & (flights$origin_vsby >= 6)),
                                      '[6-10)', flights$origin_vsby_grouped)
flights$origin_vsby_grouped <- ifelse(((flights$origin_vsby < 6) & (flights$origin_vsby >= 5)),
                                      '[5-6)', flights$origin_vsby_grouped)
flights$origin_vsby_grouped <- ifelse(((flights$origin_vsby < 5) & (flights$origin_vsby >= 4)),
                                      '[4-5)', flights$origin_vsby_grouped)
flights$origin_vsby_grouped <- ifelse(((flights$origin_vsby < 4) & (flights$origin_vsby >= 3)),
                                      '[3-4)', flights$origin_vsby_grouped)
flights$origin_vsby_grouped <- ifelse(flights$origin_vsby < 3, '<3', flights$origin_vsby_grouped)
table(flights$origin_vsby_grouped)
flights$origin_vsby_grouped <- factor(flights$origin_vsby_grouped, 
                                      levels = c('<3', '[3-4)', '[4-5)', '[5-6)', 
                                                 '[6-10)', '>=10'),
                                      ordered = TRUE)
IV(flights$origin_vsby_grouped, flights$DepDel15)



##### MONTH #####
### flight delays may be correlated with month, day of week or hour of a flight
flights$month <- factor(flights$month)
IV(flights$month, flights$DepDel15)

ggplot(flights, aes(month)) + 
  geom_histogram(stat='count', fill='#4169e1') + 
  theme_classic()



##### WEEKDAY #####
flights$weekday <- factor(flights$weekday)
IV(flights$weekday, flights$DepDel15)

ggplot(flights, aes(weekday)) + 
  geom_histogram(stat='count', fill='#4169e1') + 
  theme_classic()



##### HOUR #####
flights$hour <- factor(flights$hour)
IV(flights$hour, flights$DepDel15)
# variable has very high predictive power

ggplot(flights, aes(hour)) + 
  geom_histogram(stat='count', fill='#4169e1') + 
  theme_classic()

table(flights$hour)

### group rare values to avoid overfitting
flights$hour_grouped <- as.character(flights$hour)
flights$hour_grouped <- ifelse(flights$hour_grouped %in% c('00', '01', '02', '03', '04'),
                               '0-4', flights$hour_grouped)
flights$hour_grouped <- factor(flights$hour_grouped,
                               levels = c('0-4', '05', '06', '07', '08', '09', '10',
                                          '11', '12', '13', '14', '15', '16', 
                                          '17', '18', '19', '20', '21', '22', 
                                          '23'),
                               ordered = TRUE)
table(flights$hour_grouped)
IV(flights$hour_grouped, flights$DepDel15)
# after grouping variable predictive power is still very high


##### HOLIDAY #####
flights$Holiday <- factor(flights$Holiday)
IV(flights$Holiday, flights$DepDel15)

flights$near_holidays <- factor(flights$near_holidays)
IV(flights$near_holidays, flights$DepDel15)

### both features are not predictive
### create a new feature representing Christmas period and Juneteenth
### (holidays with the largest probability of flight delay)
### (based on Power BI plot)

# create an array with Christmas dates since 2004
xmas_period <- as.Date.character('2004-12-14'):as.Date.character('2005-01-07')
xmas_period <- as.Date(xmas_period)
flights$FlightDate %>% year() %>% unique() %>% sort()
for (i in 1:16){
  xmas_period <- c(xmas_period, xmas_period %m+% years(1))
}
xmas_period %>% year() %>% unique()
xmas_period <- unique(xmas_period)

# create an array with Juneteenth dates since 2004
juneteenth <- as.Date.character('2004-06-19')
juneteenth <- as.Date(juneteenth)
for (i in 1:16){
  juneteenth <- c(juneteenth, juneteenth %m+% years(1))
}
juneteenth <- unique(juneteenth)

# create a new feature for Chrismas period, Juneteenth or None
flights$Holiday2 <- ifelse(flights$FlightDate %in% xmas_period, 
                           'Christmas period',
                           NA)
flights$Holiday2 <- ifelse(flights$FlightDate %in% juneteenth, 
                           'Juneteenth',
                           flights$Holiday2)
flights$Holiday2 <- ifelse(is.na(flights$Holiday2), 'None', flights$Holiday2)
table(flights$Holiday2)
flights$Holiday2 <- as.factor(flights$Holiday2)
IV(flights$Holiday2, flights$DepDel15)

# check flight delay probability for each category
flights %>% select(DepDel15, Holiday2) %>% 
  group_by(Holiday2) %>% 
  summarise(mean = mean(DepDel15))

### new feature is better in terms of predicting delays, than 2 previous ones


##### FINAL INFORMATION VALUE AND SAVING DATA #####

### select only final features
flights_final_data <- flights %>% 
  select(Reporting_Airline, origin_grouped, DepDel15, Distance_grouped, 
         mfr_grouped, model_grouped, n_seats_grouped, origin_relh, 
         origin_tmpf_grouped, origin_dwpf_grouped, origin_relh_grouped, 
         origin_vsby_grouped, month, weekday, hour_grouped, Holiday2)
flights_final_data <- as.data.frame(flights_final_data)

### check IV for each feature
IV(flights_final_data$Reporting_Airline, flights_final_data$DepDel15)
IV(flights_final_data$origin_grouped, flights_final_data$DepDel15)
IV(flights_final_data$Distance_grouped, flights_final_data$DepDel15)
IV(flights_final_data$mfr_grouped, flights_final_data$DepDel15)
IV(flights_final_data$model_grouped, flights_final_data$DepDel15)
IV(flights_final_data$n_seats_grouped, flights_final_data$DepDel15)
IV(flights_final_data$origin_relh, flights_final_data$DepDel15)
IV(flights_final_data$origin_tmpf_grouped, flights_final_data$DepDel15)
IV(flights_final_data$origin_dwpf_grouped, flights_final_data$DepDel15)
IV(flights_final_data$origin_relh_grouped, flights_final_data$DepDel15)
IV(flights_final_data$origin_vsby_grouped, flights_final_data$DepDel15)
IV(flights_final_data$month, flights_final_data$DepDel15)
IV(flights_final_data$weekday, flights_final_data$DepDel15)
IV(flights_final_data$hour_grouped, flights_final_data$DepDel15)
IV(flights_final_data$Holiday2, flights_final_data$DepDel15)
### most of the variables are weak in terms of predicting flight delays


# write_csv(flights_final_data, 'data_final.csv')
