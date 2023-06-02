#-------------------------------------------------------------------------------
# The input data have to be in serie format without missing values. The first 
# column is the Datetime, each datetime value have this kind of format:
#(2013-01-01 00:00:00+01:00). Second column is "value" which corresponds to the 
# pollutant concentration value in ug/m^3. Next columns are Longitude and Latitude
# which corresponds to the longitude, latitude of the station.
#-------------------------------------------------------------------------------
#NO2 EDA

library("dplyr")
library("tidyr")
library("STRbook")
library("sp")
library("spacetime")
library(lubridate)


setwd("C:/EPFL/Master/Semester_project/Pollution_data_EU")

serie_data_no_na <- read.csv("plot_time_serie.csv")

serie_data_no_na <- serie_data_no_na %>% 
  mutate(id = group_indices(., Latitude))
serie_data_no_na <- filter(serie_data_no_na, id < 31)

serie_data_no_na$year <- year(serie_data_no_na$Datetime)
serie_data_no_na$month <- month(serie_data_no_na$Datetime)
serie_data_no_na$day <- day(serie_data_no_na$Datetime)
serie_data_no_na$hour <- hour(serie_data_no_na$Datetime)


serie_data_no_na$date <- with(serie_data_no_na,
                              paste(year, month, day, hour, sep = "-"))
serie_data_no_na$date <- as.Date(serie_data_no_na$date)
class(serie_data_no_na$date)

# EDA
library("CCA")
library("dplyr")
library("ggplot2")
library("gstat")
library("sp")
library("spacetime")
library("STRbook")
library("tidyr")
set.seed(1)
Tmax_serie <- filter(serie_data_no_na, # subset the data
                       month %in% 5:9 & # May to September
                       year == 2015) # year of 1993
Tmax_serie <- Tmax_serie %>% 
  group_by(id) %>% 
  mutate(t = row_number())

# Spatial Means NO2
spat_av <- group_by(serie_data_no_na, Latitude, Longitude) %>% # group by lon-lat
  summarise(mu_emp = mean(value)) # mean for each lon-lat
lat_means <- ggplot(spat_av) +
  geom_point(aes(Latitude, mu_emp)) +
  xlab("Latitude (deg)") +
  ylab("NO2 Concentration") + theme_bw()
lon_means <- ggplot(spat_av) +
  geom_point(aes(Longitude, mu_emp)) +
  xlab("Longitude (deg)") +
  ylab("NO2 Concentration") + theme_bw()
print(lat_means)
print(lon_means)

# Temporal Mean pollutant
Tmax_av <- group_by(serie_data_no_na, date) %>%
  summarise(meanTmax = mean(value))
p <-
  ggplot() +
  geom_line(data = serie_data_no_na,aes(x = date, y = value, group = id),
            colour = "blue", alpha = 0.04) +
  geom_line(data = Tmax_av, aes(x = date, y = meanTmax)) +
  xlab("Time") + ylab("NO2 concentration in ug/m^3") +
  theme_bw() +
  ggtitle("NO2 concentration over time")+
  scale_y_log10(limits = c(1, NA))+
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.text.x = element_text(size = 25),    
        axis.text.y = element_text(size = 25))
  
print(p)
ggsave(filename = "images/no2_over_time.png", plot = p, width = 10, height = 10)

library(dplyr)
serie_data_no_na <- serie_data
nrow(serie_data)
serie_data_no_na <- serie_data[complete.cases(serie_data$value), ]
nrow(serie_data_no_na)

library(lubridate)

serie_data_no_na <- serie_data_no_na %>% 
  mutate(id = group_indices(., Latitude))

serie_data_no_na$year <- year(serie_data_no_na$Datetime)
serie_data_no_na$month <- month(serie_data_no_na$Datetime)
serie_data_no_na$day <- day(serie_data_no_na$Datetime)
serie_data_no_na$hour <- hour(serie_data_no_na$Datetime)


library(dplyr)
library("sp")
library("spacetime")
serie_data_no_na$date <- with(serie_data_no_na,
                              paste(year, month, day, hour, sep = "-"))
serie_data_no_na$date <- as.Date(serie_data_no_na$date)
class(serie_data_no_na$date)

# Aggregate the data by day of the week and calculate the average NO2 concentration for each day
serie_data_agg <- serie_data_no_na %>%
  group_by(hour) %>%
  summarise(mean_NO2 = mean(value))

# Plot the average NO2 concentration for each day of the week
gNO2_weekday <- ggplot(serie_data_agg, aes(x = hour, y = mean_NO2)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  xlab("Hour of the day") + ylab("Average NO2 Concentration [ug/m^3]") +
  ggtitle("Mean NO2 concentration by hour of the day") +
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.text.x = element_text(size = 25),    
        axis.text.y = element_text(size = 25))
print(gNO2_weekday)
ggsave(filename = "images/mean_no2_day_hours.png", plot = gNO2_weekday, width = 10, height = 10)


# Aggregate the data by day of the week and calculate the average NO2 concentration for each day
serie_data_agg <- serie_data_no_na %>%
  group_by(year) %>%
  summarise(mean_NO2 = mean(value))

# Plot the average NO2 concentration for each day of the week
gNO2_weekday <- ggplot(serie_data_agg, aes(x = year, y = mean_NO2)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  xlab("Years") + ylab("Average NO2 Concentration [ug/m^3]") +
  ggtitle("Mean NO2 concentration by year") +
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.text.x = element_text(size = 25),    
        axis.text.y = element_text(size = 25))

print(gNO2_weekday)
ggsave(filename = "images/mean_no2_years.png", plot = gNO2_weekday, width = 10, height = 10)


# Aggregate the data by month of the year and calculate the average NO2 concentration for each month
serie_data_agg <- serie_data_no_na %>%
  group_by(month = format(date, "%m")) %>%
  summarise(mean_NO2 = mean(value))

#Convert the month column to a factor with the month names as levels
serie_data_agg$month <- factor(serie_data_agg$month, levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), labels = month.abb)

# Plot the average NO2 concentration for each month of the year with month names in the x-axis
gNO2_month <- ggplot(serie_data_agg, aes(x = month, y = mean_NO2)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  xlab("Month of the year") + ylab("Average NO2 Concentration [ug/m^3]") +
  ggtitle("Mean NO2 concentration by month of the year") +
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.text.x = element_text(size = 25),    
        axis.text.y = element_text(size = 25))
print(gNO2_month)
ggsave(filename = "images/mean_no2_months.png", plot = gNO2_month, width = 10, height = 10)

# Define the order of the levels for the day of the week
day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Aggregate the data by day of the week and calculate the average NO2 concentration for each day
serie_data_agg <- serie_data_no_na %>%
  group_by(day = format(date, "%A")) %>%
  summarise(mean_NO2 = mean(value))

# Convert the day column to a factor with the levels in the correct order
serie_data_agg$day <- factor(serie_data_agg$day, levels = day_order)

# Plot the average NO2 concentration for each day of the week with the x-axis sorted by day of the week
gNO2_weekday <- ggplot(serie_data_agg, aes(x = day, y = mean_NO2)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  xlab("Day of the Week") + ylab("Average NO2 Concentration [ug/m^3]") +
  ggtitle("Mean NO2 concentration by day of the week") +
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.text.x = element_text(size = 18),    
        axis.text.y = element_text(size = 25))
print(gNO2_weekday)
ggsave(filename = "images/mean_no2_day_weeks.png", plot = gNO2_weekday, width = 10, height = 10)


#------------------------------------------------------------------------------
#PM2.5 EDA

library("dplyr")
library("tidyr")
library("STRbook")

setwd("C:/EPFL/Master/Semester_project/Pollution_data_EU")

serie_data <- read.csv("plot_time_serie_PM25.csv")

serie_data_no_na <- serie_data
nrow(serie_data)
serie_data_no_na <- serie_data[complete.cases(serie_data$value), ]
nrow(serie_data_no_na)

library(lubridate)

serie_data_no_na <- serie_data_no_na %>% 
  mutate(id = group_indices(., Latitude))
serie_data_no_na <- filter(serie_data_no_na, id < 31)


serie_data_no_na$year <- year(serie_data_no_na$Datetime)
serie_data_no_na$month <- month(serie_data_no_na$Datetime)
serie_data_no_na$day <- day(serie_data_no_na$Datetime)
serie_data_no_na$hour <- hour(serie_data_no_na$Datetime)


library(dplyr)
library("sp")
library("spacetime")
serie_data_no_na$date <- with(serie_data_no_na,
                              paste(year, month, day, hour, sep = "-"))
serie_data_no_na$date <- as.Date(serie_data_no_na$date)
class(serie_data_no_na$date)

# EDA
library("CCA")
library("dplyr")
library("ggplot2")
library("gstat")
library("sp")
library("spacetime")
library("STRbook")
library("tidyr")
set.seed(1)
Tmax_serie <- filter(serie_data_no_na, # subset the data
                     month %in% 5:9 & # May to September
                       year == 2015) # year of 1993
Tmax_serie <- Tmax_serie %>% 
  group_by(id) %>% 
  mutate(t = row_number())

# Spatial Means NO2
spat_av <- group_by(serie_data_no_na, Latitude, Longitude) %>% # group by lon-lat
  summarise(mu_emp = mean(value)) # mean for each lon-lat
lat_means <- ggplot(spat_av) +
  geom_point(aes(Latitude, mu_emp)) +
  xlab("Latitude (deg)") +
  ylab("NO2 Concentration") + theme_bw()
lon_means <- ggplot(spat_av) +
  geom_point(aes(Longitude, mu_emp)) +
  xlab("Longitude (deg)") +
  ylab("NO2 Concentration") + theme_bw()
print(lat_means)
print(lon_means)

# Temporal Mean pollutant
Tmax_av <- group_by(serie_data_no_na, date) %>%
  summarise(meanTmax = mean(value))
p <-
  ggplot() +
  geom_line(data = serie_data_no_na,aes(x = date, y = value, group = id),
            colour = "blue", alpha = 0.04) +
  geom_line(data = Tmax_av, aes(x = date, y = meanTmax)) +
  xlab("Time") + ylab("PM2.5 concentration in ug/m^3") +
  theme_bw() +
  ggtitle("PM2.5 concentration over time")+
  scale_y_log10(limits = c(1, NA))+
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.text.x = element_text(size = 25),    
        axis.text.y = element_text(size = 25))

print(p)
ggsave(filename = "images/PM25_over_time.png", plot = p, width = 10, height = 10)

library(dplyr)
serie_data_no_na <- serie_data
nrow(serie_data)
serie_data_no_na <- serie_data[complete.cases(serie_data$value), ]
nrow(serie_data_no_na)

library(lubridate)

serie_data_no_na <- serie_data_no_na %>% 
  mutate(id = group_indices(., Latitude))

serie_data_no_na$year <- year(serie_data_no_na$Datetime)
serie_data_no_na$month <- month(serie_data_no_na$Datetime)
serie_data_no_na$day <- day(serie_data_no_na$Datetime)
serie_data_no_na$hour <- hour(serie_data_no_na$Datetime)


library(dplyr)
library("sp")
library("spacetime")
serie_data_no_na$date <- with(serie_data_no_na,
                              paste(year, month, day, hour, sep = "-"))
serie_data_no_na$date <- as.Date(serie_data_no_na$date)
class(serie_data_no_na$date)

# Aggregate the data by day of the week and calculate the average NO2 concentration for each day
serie_data_agg <- serie_data_no_na %>%
  group_by(hour) %>%
  summarise(mean_NO2 = mean(value))

# Plot the average NO2 concentration for each day of the week
gNO2_weekday <- ggplot(serie_data_agg, aes(x = hour, y = mean_NO2)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  xlab("Hour of the day") + ylab("Average PM2.5 Concentration [ug/m^3]") +
  ggtitle("Mean PM2.5 concentration by hour of the day") +
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.text.x = element_text(size = 25),    
        axis.text.y = element_text(size = 25))
print(gNO2_weekday)
ggsave(filename = "images/mean_PM25_day_hours.png", plot = gNO2_weekday, width = 10, height = 10)



# Aggregate the data by day of the week and calculate the average NO2 concentration for each day
serie_data_agg <- serie_data_no_na %>%
  group_by(year) %>%
  summarise(mean_NO2 = mean(value))

# Plot the average NO2 concentration for each day of the week
gNO2_weekday <- ggplot(serie_data_agg, aes(x = year, y = mean_NO2)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  xlab("Years") + ylab("Average PM2.5 Concentration [ug/m^3]") +
  ggtitle("Mean PM2.5 concentration by year") +
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.text.x = element_text(size = 25),    
        axis.text.y = element_text(size = 25))

print(gNO2_weekday)
ggsave(filename = "images/mean_PM25_years.png", plot = gNO2_weekday, width = 10, height = 10)


# Aggregate the data by month of the year and calculate the average NO2 concentration for each month
serie_data_agg <- serie_data_no_na %>%
  group_by(month = format(date, "%m")) %>%
  summarise(mean_NO2 = mean(value))

#Convert the month column to a factor with the month names as levels
serie_data_agg$month <- factor(serie_data_agg$month, levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), labels = month.abb)

# Plot the average NO2 concentration for each month of the year with month names in the x-axis
gNO2_month <- ggplot(serie_data_agg, aes(x = month, y = mean_NO2)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  xlab("Month of the year") + ylab("Average PM2.5 Concentration [ug/m^3]") +
  ggtitle("Mean PM2.5 concentration by month of the year") +
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.text.x = element_text(size = 25),    
        axis.text.y = element_text(size = 25))
print(gNO2_month)
ggsave(filename = "images/mean_PM25_months.png", plot = gNO2_month, width = 10, height = 10)



# Define the order of the levels for the day of the week
day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Aggregate the data by day of the week and calculate the average NO2 concentration for each day
serie_data_agg <- serie_data_no_na %>%
  group_by(day = format(date, "%A")) %>%
  summarise(mean_NO2 = mean(value))

# Convert the day column to a factor with the levels in the correct order
serie_data_agg$day <- factor(serie_data_agg$day, levels = day_order)

# Plot the average NO2 concentration for each day of the week with the x-axis sorted by day of the week
gNO2_weekday <- ggplot(serie_data_agg, aes(x = day, y = mean_NO2)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  xlab("Day of the Week") + ylab("Average PM2.5 Concentration [ug/m^3]") +
  ggtitle("Mean PM2.5 concentration by day of the week") +
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.text.x = element_text(size = 18),    
        axis.text.y = element_text(size = 25))
print(gNO2_weekday)
ggsave(filename = "images/mean_PM25_day_weeks.png", plot = gNO2_weekday, width = 10, height = 10)


