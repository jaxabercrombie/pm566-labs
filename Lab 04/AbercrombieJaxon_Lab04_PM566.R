# Libraries
library(tidyverse)
library(leaflet)
library(ggplot2)
library(data.table)


#1 - Read in the data
if (!file.exists("met_all.gz"))
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "met_all.gz",
    method   = "libcurl",
    timeout  = 60)
met <- data.table::fread("met_all.gz")


#2 - Prepare the data
#Remove temperatures that are less than 17 degrees Celsius
met <- met[temp >= -17]

# Ensure there's no missing data in key variables that are coded as 9999, 999
met[, range(temp)]
met[, range(rh, na.rm = T)]
met[, range(wind.sp, na.rm = T)]
met[, range(vis.dist, na.rm = T)]
met[, range(dew.point, na.rm = T)]
met[, range(lat, na.rm = T)]
met[, range(lon, na.rm = T)]
met[, range(elev, na.rm = T)]

# Because elev had range including 9999:
met[elev == 9999, elev := NA]

# Generate date variable using functions as.Date()
met[, ymd := as.Date(paste(year, month, day, sep = "-"))]

# Keep observations of the first week of the month
met[, table(week(ymd))]
met <- met[ week(ymd) == 31]

#Compute mean by station of many variables
met_avg <- met[, .(
  temp = mean(temp, na.rm = T),
  rh = mean(rh, na.rm = T),
  wind.sp = mean(wind.sp, na.rm = T),
  vis.dist = mean(vis.dist, na.rm = T),
  dew.point = mean(dew.point, na.rm = T),
  lat = mean(lat, na.rm = T),
  lon = mean(lon, na.rm = T),
  elev = mean(elev, na.rm = T), USAFID
), by = "USAFID"]

# Create a region variable for NW, SW, NE, SE based on longitude and latitude
met_avg[lat >= 39.71 & lon <= -98, region := "Northwest"]
met_avg[lat < 39.71 & lon <= -98, region := "Southwest"]
met_avg[lat >= 39.71 & lon > -98, region := "Northeast"]
met_avg[lat < 39.71 & lon > -98, region := "Southeast"]

met_avg[, table(region, useNA = "always")]

met_avg[, region2 := fifelse(lat >= 39.71 & lon <= -98, "Northwest",
                             fifelse(lat < 39.71  & lon <= -98, "Southwest",
                                     fifelse(lat >= 39.71 & lon > -98, "Northeast",
                                             fifelse(lat < 39.71  & lon > -98, "Southeast", NA_character_))))]

# Create categorical variable for elevation as in the lecture slide deck
met_avg[, elev_cat := fifelse(elev > 252, "high", "low")]

#Kept running into unique data issue later on throughout code:
colnames(met_avg) <- make.unique(names(met_avg))


#3 - geom_violin
ggplot(met_avg, mapping = aes(y = wind.sp, x = 1)) +
  geom_violin() +
  facet_grid(~region)

ggplot(met_avg, mapping = aes(y = dew.point, x = 1)) +
  geom_violin() +
  facet_grid(~region)

# Based on the graphs, it appears the northeast has the smallest wind speed values and the southwest has the greatest
# Additionally, for dew point, the southeast has the greatest and northwest the least.


#4 - geom_jitter with stat_smooth
met_avg[!is.na(wind.sp) & !is.na(dew.point)] %>%
  ggplot(mapping = aes(x = dew.point, y = wind.sp, linetype = region, color = region)) +
  geom_jitter() +
  stat_smooth(formula = y~x, method = lm, se = F, col = "black")

# Based on these graphs, it is evident that datapoints from each region are fairly clustered with other points.
# Wind speed generally stays between the values of 0 and about 3.75, and dew point between 10 and 25.
# The southeast has the greatest dew points, while the northeast generally has the least.
# For wind speed, the southwest appears to have the greatest values while southeast the least.

#5 - geom_bar
met_avg[!is.na(elev_cat)] %>%
  ggplot() +
  geom_bar(mapping = aes(x = elev_cat, fill = region), position = "dodge") +
  labs(x = "Elevation Categories" , y = "Frequency", title = "Elevation by Region") +
  scale_fill_brewer(palette = "Spectral")

# Based on this plot, the southeast has the lowest elevation, while the northeast has the highest.


#6 - stat_summary
ggplot(
  met_avg[!is.na(wind.sp) & !is.na(dew.point)],
  mapping = aes(x=region, y = wind.sp)) +
  stat_summary(fun.data="mean_sdl") +
  stat_summary(fun.data="mean_sdl", geom="errorbar")

ggplot(
  met_avg[!is.na(wind.sp) & !is.na(wind.sp)],
  mapping = aes(x=region, y = dew.point)) +
  stat_summary(fun.data="mean_sdl") +
  stat_summary(fun.data="mean_sdl", geom="errorbar")

# Based on these graphs, we can see that there is sizable error across each
# region for wind speed, while the error for dew point varies more.
# Southern regions had the least error for wind speed and northern the most.
# The southeast region had the least amount of error for dew.point
# Dew point temperature is largest in the southeast and smallest in the northwest
# Wind speed is largest in the southwest and smallest in the northeast


#7 - spatial trend map
library(leaflet)

temp.pal <- colorNumeric(c('darkgreen','goldenrod','brown'),
domain=met_avg$rh)

leaflet(met_avg) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(
    lat = ~lat, lng=~lon,
    # HERE IS OUR PAL!
    label = ~paste0(round(rh,2), ' rh'), color = ~ temp.pal(rh),
    opacity = 1, fillOpacity = 1, radius = 500) %>%
  # And a pretty legend
  addLegend('bottomleft', pal=temp.pal, values=met_avg$rh,
            title='Temperature, C', opacity=1)

# The trend in rh across the United States is that the southeast has the
# highest values, while moving east decreases the values.



#8 - ggplot extension
library(ggplot2)
library(patchwork)

p1 <- ggplot(met_avg) + geom_point(aes(elev, temp))
p2 <- ggplot(met_avg) + geom_boxplot(aes(elev, temp, group = region))
p3 <- ggplot(met_avg) + geom_smooth(aes(elev, temp))

(p1 | p2 / p3)

