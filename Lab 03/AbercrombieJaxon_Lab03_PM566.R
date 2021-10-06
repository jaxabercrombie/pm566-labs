#Load packages
library(tidyverse)
library(R.utils)
library(leaflet)

#1 - Read in the data
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "met_all.gz", method="libcurl", timeout = 60)
met <- data.table::fread("met_all.gz")


#2 - Checking dimensions, headers, and footers
dim(met)
head(met)
tail(met)


#3 - Looking at all the variables
str(met)


#4 - Closer look at the key variables
table(met$year)
table(met$day)
table(met$hour)

summary(met$temp)
summary(met$elev)
summary(met$wind.sp)

met[met$elev==9999.0] <- NA
summary(met$elev)

met <- met[temp>-40]
met2 <- met[order(temp)]
head(met2)


#5 - Check data against external source
met <- met[temp>-15]
met2 <- met[order(temp)]
head(met2)


#6 - Calculate summary statistics
elev <- met[elev==max(elev)]
summary(elev)

cor(elev$temp, elev$wind.sp, use="complete")
cor(elev$temp, elev$hour, use="complete")
cor(elev$wind.sp, elev$day, use="complete")
cor(elev$wind.sp, elev$hour, use="complete")
cor(elev$temp, elev$day, use="complete")


#7 - Exploratory graphs
hist(met$elev, breaks=100)
hist(met$temp)
hist(met$wind.sp)

leaflet(elev) %>%
  addProviderTiles('OpenStreetMap') %>%
  addCircles(lat=~lat,lng=~lon, opacity=1, fillOpacity=1, radius=100)

library(lubridate)
elev$date <- with(elev, ymd_h(paste(year, month, day, hour, sep= ' ')))
summary(elev$date)

elev <- elev[order(date)]
head(elev)

plot(elev$date, elev$temp, type='l')
# Temperature rises and lowers over the course of a single day; day vs. night
# Values stay within the range of about 3 and 15
# There maybe is a relationship between wind and temperature, for
# days that are colder tend to have more wind

plot(elev$date, elev$wind.sp, type='l')
# Wind is highest in mid- and late August for this particular data set
# After wind reaches high speeds, the following days often have low speeds
# Wind never reaches higher than 20mph, and it hits as low as 0mph




