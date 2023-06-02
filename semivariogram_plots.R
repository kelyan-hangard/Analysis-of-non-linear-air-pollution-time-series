#-------------------------------------------------------------------------------
#The input data (NO2 SEP, NO2 FIM, PM2.5 SEP and PM2.5 FIM) have to be in
# serie format without missing values. The first column is the index, the second
# column is the Datetime column, each datetime value have this kind of format:
#(2013-01-01 00:00:00+01:00). Third column is "value" which corresponds to the 
# pollutant concentration value in ug/m^3. Next columns are Longitude, Latitude
# and ID which corresponds to the longitude, latitude and station ID of the station
# where the pollutant were measured
#-------------------------------------------------------------------------------

library("tidyr")
library("STRbook")
library("animation")
library("dplyr")
library("ggplot2")
library("gstat")
library("maps")
library("sp")
library("spacetime")
library(lubridate)
library("dplyr")

setwd("C:/EPFL/Master/Semester_project/Pollution_data_EU")
serie_data_NO2 <- read.csv("sample_data1008.csv")
nrow(serie_data_NO2)
serie_data_no_na_NO2 <- serie_data_NO2[complete.cases(serie_data_NO2$value), ]
nrow(serie_data_no_na_NO2)

serie_data_no_na_NO2$year <- year(serie_data_no_na_NO2$Datetime)
serie_data_no_na_NO2$month <- month(serie_data_no_na_NO2$Datetime)
serie_data_no_na_NO2$day <- day(serie_data_no_na_NO2$Datetime)
serie_data_no_na_NO2$hour <- hour(serie_data_no_na_NO2$Datetime)
serie_data_no_na_NO2$minute <- minute(serie_data_no_na_NO2$Datetime)
serie_data_no_na_NO2$second <- second(serie_data_no_na_NO2$Datetime)

# Filter rows where 'minute' ans "second" is 0
serie_data_no_na_NO2 <- serie_data_no_na_NO2 %>% filter(minute == 0)
serie_data_no_na_NO2 <- serie_data_no_na_NO2 %>% filter(second == 0)

serie_data_no_na_NO2 <- serie_data_no_na_NO2 %>% 
  mutate(id = group_indices(., Latitude))
serie_data_no_na_NO2$date <- with(serie_data_no_na_NO2,
                              paste(year, month, day, hour, sep = "-"))
serie_data_no_na_NO2$date <- as.Date(serie_data_no_na_NO2$date)

# Hourly precision
grouped_data_NO2 <- serie_data_no_na_NO2

Tmax_serie_NO2 <- grouped_data_NO2 %>%
  mutate(julian = row_number() + 726833)
Times_serie_NO2 <- Tmax_serie_NO2 %>%
  dplyr::select(year, month, day, hour) %>%
  distinct() %>%
  arrange(year, month, day, hour)

Times_serie_NO2 <- Times_serie_NO2 %>%
  mutate(julian = row_number() + 726833)
loc_serie_NO2<- Tmax_serie_NO2 %>%
  dplyr::select(Longitude, Latitude) %>%
  distinct()
spat_part_serie_NO2 <- SpatialPoints(coords = loc_serie_NO2[, c("Longitude", "Latitude")])
temp_part_serie_NO2 <- with(Times_serie_NO2, paste(year, month, day, hour, sep = "-"))
temp_part_serie_NO2 <- as.POSIXct(temp_part_serie_NO2, format = "%Y-%m-%d-%H")

Tmax_long3_serie_NO2 <- Tmax_serie_NO2 %>%
  dplyr::select(julian, year, month, day, hour, id, value)
Tmax_long3_serie_NO2$id <- as.integer(Tmax_long3_serie_NO2$id)

STObj3_serie_NO2 <- STFDF(sp = spat_part_serie_NO2,
                      time = temp_part_serie_NO2,
                      data = Tmax_long3_serie_NO2)
class(STObj3_serie_NO2)


# Variogram
vv_serie_NO2_6hrs = variogram(value~1, STObj3_serie_NO2, width=5, cutoff = 3000, tlags=0:6)
vv_serie_NO2_12hrs_10 = variogram(value~1, STObj3_serie_NO2, width=10, cutoff = 3000, tlags=0:12)
vv_serie_NO2_12hrs_15 = variogram(value~1, STObj3_serie_NO2, width=15, cutoff = 3000, tlags=0:12)
vv_serie_NO2_24hrs_10 = variogram(value~1, STObj3_serie_NO2, width=10, cutoff = 3000, tlags=0:24)
vv_serie_NO2_24hrs_15 = variogram(value~1, STObj3_serie_NO2, width=15, cutoff = 3000, tlags=0:24)

#--------------------------------------------------

serie_data_PM25 <- read.csv("sample_data_PM25_2017.csv")
nrow(serie_data_PM25)
serie_data_no_na_PM25 <- serie_data_PM25[complete.cases(serie_data_PM25$value), ]
nrow(serie_data_no_na_PM25)

serie_data_no_na_PM25$year <- year(serie_data_no_na_PM25$Datetime)
serie_data_no_na_PM25$month <- month(serie_data_no_na_PM25$Datetime)
serie_data_no_na_PM25$day <- day(serie_data_no_na_PM25$Datetime)
serie_data_no_na_PM25$hour <- hour(serie_data_no_na_PM25$Datetime)
serie_data_no_na_PM25$minute <- minute(serie_data_no_na_PM25$Datetime)
serie_data_no_na_PM25$second <- second(serie_data_no_na_PM25$Datetime)

# Filter rows where 'minute' ans "second" is 0
serie_data_no_na_PM25 <- serie_data_no_na_PM25 %>% filter(minute == 0)
serie_data_no_na_PM25 <- serie_data_no_na_PM25 %>% filter(second == 0)

serie_data_no_na_PM25 <- serie_data_no_na_PM25 %>% 
  mutate(id = group_indices(., Latitude))
serie_data_no_na_PM25$date <- with(serie_data_no_na_PM25,
                              paste(year, month, day, hour, sep = "-"))
serie_data_no_na_PM25$date <- as.Date(serie_data_no_na_PM25$date)

test <- serie_data_no_na_PM25 %>% 
  filter(date >= ymd("2014-04-01") & date <= ymd("2015-03-25"))
nrow(test)

# Hourly precision
grouped_data_PM25 <- test

Tmax_serie_PM25 <- grouped_data_PM25 %>%
  mutate(julian = row_number() + 726833)
Times_serie_PM25 <- Tmax_serie_PM25 %>%
  dplyr::select(year, month, day, hour) %>%
  distinct() %>%
  arrange(year, month, day, hour)

Times_serie_PM25 <- Times_serie_PM25 %>%
  mutate(julian = row_number() + 726833)
loc_serie_PM25 <- Tmax_serie_PM25 %>%
  dplyr::select(Longitude, Latitude) %>%
  distinct()
spat_part_serie_PM25 <- SpatialPoints(coords = loc_serie_PM25[, c("Longitude", "Latitude")])
temp_part_serie_PM25 <- with(Times_serie_PM25, paste(year, month, day, hour, sep = "-"))
temp_part_serie_PM25 <- as.POSIXct(temp_part_serie_PM25, format = "%Y-%m-%d-%H")

Tmax_long3_serie_PM25 <- Tmax_serie_PM25 %>%
  dplyr::select(julian, year, month, day, hour, id, value)
Tmax_long3_serie_PM25$id <- as.integer(Tmax_long3_serie_PM25$id)

STObj3_serie_PM25 <- STFDF(sp = spat_part_serie_PM25,
                      time = temp_part_serie_PM25,
                      data = Tmax_long3_serie_PM25)
class(STObj3_serie_PM25)


# Variogram
vv_serie_PM25_6hrs = variogram(value~1, STObj3_serie_PM25, width=5, cutoff = 3000, tlags=0:6)
vv_serie_PM25_12hrs_10 = variogram(value~1, STObj3_serie_PM25, width=10, cutoff = 3000, tlags=0:12)
vv_serie_PM25_12hrs_15 = variogram(value~1, STObj3_serie_PM25, width=15, cutoff = 3000, tlags=0:12)
vv_serie_PM25_24hrs_10 = variogram(value~1, STObj3_serie_PM25, width=10, cutoff = 3000, tlags=0:24)
vv_serie_PM25_24hrs_15 = variogram(value~1, STObj3_serie_PM25, width=15, cutoff = 3000, tlags=0:24)

#------------------------------------------------------------------------------
vv_serie_clean <- vv_serie_NO2_6hrs[!is.na(vv_serie_NO2_6hrs$gamma), ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$timelag != 0, ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$spacelag != 0, ]
plot(vv_serie_clean)
png(filename = "images/semivariogram_6hrs_NO2.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, main = "6 hours NO2 semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()

library(lattice)
metricVgm_serie <- vgmST("metric",joint=vgm(50,"Exp",100,0), stAni=50)
metricVgm_serie <- fit.StVariogram(vv_serie_clean, metricVgm_serie)
sepVgm_serie <- vgmST("separable",space=vgm(0.9,"Exp", 123, 0.1),time =vgm(0.9,"Exp", 2.9, 0.1),sill=100)
sepVgm_serie <- fit.StVariogram(vv_serie_clean, sepVgm_serie, method = "L-BFGS-B",lower = c(10,0,0.01,0,1),upper = c(500,1,20,1,200))
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)))
png(filename = "images/semivariogram_6hrs_NO2_3D.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)), main = "6 hours NO2 3D Semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()

vv_serie_clean <- vv_serie_NO2_12hrs_10[!is.na(vv_serie_NO2_12hrs_10$gamma), ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$timelag != 0, ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$spacelag != 0, ]
plot(vv_serie_clean)
png(filename = "images/semivariogram_12hrs_NO2.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, main = "12 hours NO2 semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()

library(lattice)
metricVgm_serie <- vgmST("metric",joint=vgm(50,"Exp",100,0), stAni=50)
metricVgm_serie <- fit.StVariogram(vv_serie_clean, metricVgm_serie)
sepVgm_serie <- vgmST("separable",space=vgm(0.9,"Exp", 123, 0.1),time =vgm(0.9,"Exp", 2.9, 0.1),sill=100)
sepVgm_serie <- fit.StVariogram(vv_serie_clean, sepVgm_serie, method = "L-BFGS-B",lower = c(10,0,0.01,0,1),upper = c(500,1,20,1,200))
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)))
png(filename = "images/semivariogram_12hrs_NO2_3D.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)), main = "12 hours NO2 3D Semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()


vv_serie_clean <- vv_serie_NO2_24hrs_15[!is.na(vv_serie_NO2_24hrs_15$gamma), ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$timelag != 0, ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$spacelag != 0, ]
plot(vv_serie_clean)
png(filename = "images/semivariogram_24hrs_NO2.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, main = "24 hours NO2 semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()

library(lattice)
metricVgm_serie <- vgmST("metric",joint=vgm(50,"Exp",100,0), stAni=50)
metricVgm_serie <- fit.StVariogram(vv_serie_clean, metricVgm_serie)
sepVgm_serie <- vgmST("separable",space=vgm(0.9,"Exp", 123, 0.1),time =vgm(0.9,"Exp", 2.9, 0.1),sill=100)
sepVgm_serie <- fit.StVariogram(vv_serie_clean, sepVgm_serie, method = "L-BFGS-B",lower = c(10,0,0.01,0,1),upper = c(500,1,20,1,200))
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)))
png(filename = "images/semivariogram_24hrs_NO2_3D.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)), main = "24 hours NO2 3D Semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()
# -----------------------------------------------------------------------------

vv_serie_clean <- vv_serie_PM25_6hrs[!is.na(vv_serie_PM25_6hrs$gamma), ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$timelag != 0, ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$spacelag != 0, ]
plot(vv_serie_clean)
png(filename = "images/semivariogram_6hrs_PM25.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, main = "6 hours PM2.5 semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()

library(lattice)
metricVgm_serie <- vgmST("metric",joint=vgm(50,"Exp",100,0), stAni=50)
metricVgm_serie <- fit.StVariogram(vv_serie_clean, metricVgm_serie)
sepVgm_serie <- vgmST("separable",space=vgm(0.9,"Exp", 123, 0.1),time =vgm(0.9,"Exp", 2.9, 0.1),sill=100)
sepVgm_serie <- fit.StVariogram(vv_serie_clean, sepVgm_serie, method = "L-BFGS-B",lower = c(10,0,0.01,0,1),upper = c(500,1,20,1,200))
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)))
png(filename = "images/semivariogram_6hrs_PM25_3D.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)), main = "6 hours PM2.5 3D Semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()

vv_serie_clean <- vv_serie_PM25_12hrs_10[!is.na(vv_serie_PM25_12hrs_10$gamma), ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$timelag != 0, ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$spacelag != 0, ]
plot(vv_serie_clean)
png(filename = "images/semivariogram_12hrs_PM25.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, main = "12 hours PM2.5 semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()

library(lattice)
metricVgm_serie <- vgmST("metric",joint=vgm(50,"Exp",100,0), stAni=50)
metricVgm_serie <- fit.StVariogram(vv_serie_clean, metricVgm_serie)
sepVgm_serie <- vgmST("separable",space=vgm(0.9,"Exp", 123, 0.1),time =vgm(0.9,"Exp", 2.9, 0.1),sill=100)
sepVgm_serie <- fit.StVariogram(vv_serie_clean, sepVgm_serie, method = "L-BFGS-B",lower = c(10,0,0.01,0,1),upper = c(500,1,20,1,200))
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)))
png(filename = "images/semivariogram_12hrs_PM25_3D.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)), main = "12 hours PM2.5 3D Semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()


vv_serie_clean <- vv_serie_PM25_24hrs_10[!is.na(vv_serie_PM25_24hrs_10$gamma), ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$timelag != 0, ]
vv_serie_clean <- vv_serie_clean[vv_serie_clean$spacelag != 0, ]
plot(vv_serie_clean)
png(filename = "images/semivariogram_24hrs_PM25.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, main = "24 hours PM2.5 semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()

library(lattice)
metricVgm_serie <- vgmST("metric",joint=vgm(50,"Exp",100,0), stAni=50)
metricVgm_serie <- fit.StVariogram(vv_serie_clean, metricVgm_serie)
sepVgm_serie <- vgmST("separable",space=vgm(0.9,"Exp", 123, 0.1),time =vgm(0.9,"Exp", 2.9, 0.1),sill=100)
sepVgm_serie <- fit.StVariogram(vv_serie_clean, sepVgm_serie, method = "L-BFGS-B",lower = c(10,0,0.01,0,1),upper = c(500,1,20,1,200))
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)))
png(filename = "images/semivariogram_24hrs_PM25_3D.png", width = 600, height = 600, res= 150)
plot(vv_serie_clean, all=T, wireframe=T, zlim=c(min(vv_serie_clean$gamma),max(vv_serie_clean$gamma)),zlab=NULL,xlab=list("distance (km)", rot=30), ylab=list("time lag (days)", rot=-35),scales=list(arrows=F, z = list(distance = 5)), main = "24 hours PM2.5 3D Semivariogram", cex =2, cex.lab =2, cex.main = 2, cex.axis = 2)
dev.off()
