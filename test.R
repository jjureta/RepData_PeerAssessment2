library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

## hin to loade date in other format than Y-m-d
## http://stackoverflow.com/questions/13022299/specify-date-format-for-colclasses-argument-in-read-table-read-csv/13022441#13022441
setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%Y %H:%M:%S") )

## data are loaded with read.csv and not fread because fread is not able to cope with som
## double quoted strings
#data <- data.table(read.csv("data/repdata_data_StormData.csv", 
#                            colClasses=c('numeric','myDate', rep(NA, 35)) 
#                            )
#                   )

## keep only data important for analysis
data <- data[, c('EVTYPE','FATALITIES', 'INJURIES', 
         'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP', 'BGN_DATE'), with = FALSE]

## some eventypes are in uppercase and other in lowercase.
## in order to consider the evantypes same in case where the letter case 
## is not the same, the eventype is uppercased
data[,EVTYPE := toupper(EVTYPE)]

##still too many event types
length(unique(data$EVTYPE))

## download the event types from http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf
## (see 2.1.1)
## in some cases multiple event types are specified as an event type, ex. SNOW/HIGH WINDS.
## in such cases we take only one of them which seems more appropriate.
eventtypes <- fread("data/eventtypes.csv")
eventtypes[,EVTYPE := toupper(EVTYPE)]
eventtypes[,EVTYPEVALID := TRUE]

data[EVTYPE %like% '^THUND', EVTYPE := 'THUNDERSTORM WIND']
data[EVTYPE %like% '^TSTM', EVTYPE := 'THUNDERSTORM WIND']
data[EVTYPE %like% '^TORN', EVTYPE := 'TORNADO']

data[EVTYPE %like% '^HURRICANE', EVTYPE := 'HURRICANE (TYPHOON)']

data[EVTYPE %like% '^HIGH WIND', EVTYPE := 'HIGH WIND']

data[EVTYPE == 'STRONG WINDS', EVTYPE := 'STRONG WIND']

data[EVTYPE %like% '^HAIL', EVTYPE := 'HAIL']

data[EVTYPE %like% '^MARINE TSTM WIND', EVTYPE := 'MARINE THUNDERSTORM WIND']

data[EVTYPE %like% '^HEAVY RAIN', EVTYPE := 'HEAVY RAIN']

data[EVTYPE %like% '^HEAVY SNOW', EVTYPE := 'HEAVY SNOW']

data[EVTYPE %like% '^FLASH FLOOD', EVTYPE := 'FLASH FLOOD']

data[EVTYPE %like% '^URBAN/SML STREAM FLD', EVTYPE := 'FLOOD']
data[EVTYPE %like% '^URBAN/SMALL STREAM FLOODING', EVTYPE := 'FLOOD']
data[EVTYPE %like% '^URBAN/SMALL STREAM  FLOOD', EVTYPE := 'FLOOD']
data[EVTYPE %like% '^URBAN FLOODS', EVTYPE := 'FLOOD']
data[EVTYPE %like% '^URBAN FLOODING', EVTYPE := 'FLOOD']
data[EVTYPE %like% '^URBAN FLOOD', EVTYPE := 'FLOOD']
data[EVTYPE %like% '^HIGHWAY FLOODING', EVTYPE := 'FLOOD']

data[EVTYPE %like% '^BLIZZARD', EVTYPE := 'BLIZZARD']

data[EVTYPE %like% '^VOLCANIC', EVTYPE := 'VOLCANIC ASH']

data[EVTYPE %like% '^WINTER WEATHER', EVTYPE := 'WINTER WEATHER']

data[EVTYPE %like% '^WINTER STORM', EVTYPE := 'WINTER STORM']

data[EVTYPE %like% '^EXTREME COLD', EVTYPE := 'EXTREME COLD/WIND CHILL']

data[EVTYPE %like% '^EXTREME HEAT', EVTYPE := 'EXCESSIVE HEAT']
data[EVTYPE %like% '^HEAT WAVE', EVTYPE := 'EXCESSIVE HEAT']
data[EVTYPE %like% '^RECORD HEAT', EVTYPE := 'EXCESSIVE HEAT']

data[EVTYPE %like% '^FOG', EVTYPE := 'DENSE FOG']

data[EVTYPE %like% '^WILD FIRES', EVTYPE := 'WILDFIRE']
data[EVTYPE %like% '^WILD/FOREST FIRE', EVTYPE := 'WILDFIRE']

data[EVTYPE %like% '^GLAZE', EVTYPE := 'WINTER WEATHER']
data[EVTYPE %like% '^BLACK ICE', EVTYPE := 'WINTER WEATHER']

data[EVTYPE %like% '^WATERSPOUT', EVTYPE := 'WATERSPOUT']

data[EVTYPE %like% '^COASTAL', EVTYPE := 'COASTAL FLOOD']

data[EVTYPE %like% '^FREEZING', EVTYPE := 'FROST/FREEZE']

data[EVTYPE %like% '^HEAVY SURF', EVTYPE := 'HIGH SURF']

data[EVTYPE == 'COLD', EVTYPE := 'COLD/WIND CHILL']
data[EVTYPE == 'EXTREME WINDCHILL', EVTYPE := 'COLD/WIND CHILL']

data[EVTYPE %in% c('ICE', 'ICE ROADS', 'ICE STORM/FLASH FLOOD', 'ICY ROADS'), 
     EVTYPE := 'HIGH SURF']

data[EVTYPE == 'RIP CURRENTS', EVTYPE := 'RIP CURRENT']

data[EVTYPE == 'SMALL HAIL', EVTYPE := 'HAIL']

data[EVTYPE == 'HIGH SURF ADVISORY', EVTYPE := 'HIGH SURF']

## PROPDMGEXP doesn't look to be normalized.
unique(data[, PROPDMGEXP])

## in order to fix it for all PROPDMG equal to 0, I put 0 in PROPDMGEXP, 
## because 0 * x = 0; i.e. exponent doesn't change damage value.
data[PROPDMG == 0, PROPDMGEXP := '0']

data <- merge(data, eventtypes, by = "EVTYPE", all.x = T)

economic_damage <- data[, c('EVTYPE', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 
                            'CROPDMGEXP', 'EVTYPEVALID', 'BGN_DATE'), 
                        with = FALSE]

## some exponents are not compatible with expected data
table(economic_damage$PROPDMGEXP)

## I choose to remove all empty, -, ? and + exponents.
## transform textual exponents into numbers
economic_damage[PROPDMGEXP == 'B', PROPDMGTOTAL := PROPDMG * 1E9]
economic_damage[PROPDMGEXP == 'h', PROPDMGTOTAL := PROPDMG * 1E2]
economic_damage[PROPDMGEXP == 'H', PROPDMGTOTAL := PROPDMG * 1E2]
economic_damage[PROPDMGEXP == 'K', PROPDMGTOTAL := PROPDMG * 1E3]
economic_damage[PROPDMGEXP == 'k', PROPDMGTOTAL := PROPDMG * 1E3]
economic_damage[PROPDMGEXP == 'M', PROPDMGTOTAL := PROPDMG * 1E6]
economic_damage[PROPDMGEXP == 'm', PROPDMGTOTAL := PROPDMG * 1E6]

economic_damage[PROPDMGEXP %in% c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9'), 
                PROPDMGTOTAL := PROPDMG * 10^as.numeric(PROPDMGEXP)]

economic_damage[CROPDMGEXP == 'B', CROPDMGTOTAL := CROPDMG * 1E9]
economic_damage[CROPDMGEXP == 'h', CROPDMGTOTAL := CROPDMG * 1E2]
economic_damage[CROPDMGEXP == 'H', CROPDMGTOTAL := CROPDMG * 1E2]
economic_damage[CROPDMGEXP == 'K', CROPDMGTOTAL := CROPDMG * 1E3]
economic_damage[CROPDMGEXP == 'k', CROPDMGTOTAL := CROPDMG * 1E3]
economic_damage[CROPDMGEXP == 'M', CROPDMGTOTAL := CROPDMG * 1E6]
economic_damage[CROPDMGEXP == 'm', CROPDMGTOTAL := CROPDMG * 1E6]

economic_damage[CROPDMGEXP %in% c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9'), 
                CROPDMGTOTAL := CROPDMG * 10^as.numeric(CROPDMGEXP)]

economic_damage[is.na(PROPDMGTOTAL), PROPDMGTOTAL := 0]
economic_damage[is.na(CROPDMGTOTAL), CROPDMGTOTAL := 0]

economic_damage[, DMGTOTAL := PROPDMGTOTAL + CROPDMGTOTAL]

economic_damage[, YEAR := year(BGN_DATE)]

## adjust inflation
## see http://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package
monthly_cpi <-
  read.csv("http://research.stlouisfed.org/fred2/data/CPIAUCSL.csv", header = TRUE)
monthly_cpi$YEAR <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(YEAR) %>% summarize(cpi = mean(VALUE))
yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$YEAR == 2014]
yearly_cpi <- data.table(yearly_cpi)

economic_damage <- merge(economic_damage, yearly_cpi, by = "YEAR", all.x = T)

economic_damage <- economic_damage[, c('EVTYPE', 'YEAR', 'EVTYPEVALID', 'DMGTOTAL', 'adj_factor'), 
                        with = FALSE]
