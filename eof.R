#-------------------------------------------------------------------------------
#All the input data (NO2 SEP, NO2 FIM, PM2.5 SEP and PM2.5 FIM) have to be in
# serie format without missing values. The first column is the Datetime column
# each datetime value have this kind of format (2013-01-01 00:00:00+01:00).
# Each other column have the name of the pollutant station on the first row and 
# the values corresponding to the pollutant concentrations in each row of the column
#-------------------------------------------------------------------------------
# Compute EOF NO2 SEP
setwd("C:/EPFL/Master/Semester_project/Pollution_data_EU")
SEP <- read.csv("SEP_NO2_nona.csv")
metadata <- read.csv("metadata_NO2_nona.csv")

SEP <- SEP[,-1]
SEP <- t(SEP)

Z_SEP <- t(SEP)
dim(Z_SEP)

## First find the matrix we need to subtract:
spat_mean_SEP <- apply(SEP, 1, mean)
nT_SEP <- ncol(SEP)

## Then subtract and standardize:
Zspat_detrend_SEP <- Z_SEP - outer(rep(1, nT_SEP), spat_mean_SEP)
Zt_SEP <- 1/sqrt(nT_SEP - 1)*Zspat_detrend_SEP

E_SEP <- svd(Zt_SEP)

V_SEP <- E_SEP$v
colnames(E_SEP$v) <- paste0("EOF", 1:ncol(SEP)) # label columns
EOFs_SEP <- cbind(metadata, E_SEP$v)
head(EOFs_SEP[, 1:6])

library(tidyr)
library(ggplot2)
library(dplyr)

TS_SEP <- data.frame(E_SEP$u) %>% # convert U to data frame
  mutate(t = 1:nrow(E_SEP$u)) %>% # add a time field
  gather(EOF_SEP, PC, -t) # put columns (except time)


TS_SEP$nPC <- TS_SEP$PC * sqrt(nT_SEP-1)



## First component 
filtered_data <- TS_SEP %>% filter(EOF_SEP == "X1")

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

library(lubridate)

# Set the starting date
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2017-12-26")

# Calculate the total number of days in the data period
total_days <- as.numeric(difftime(end_date, start_date, units = "days"))

# Calculate the time step size in days
time_step_days <- total_days / (261 - 1)

# Create a new column with the corresponding dates
filtered_data$date <- start_date + (filtered_data$t - 1) * time_step_days

# Create the plot
# Find the date for the maximum point
max_point <- filtered_data[which.max(filtered_data$nPC), "date"]

p <- ggplot(data = filtered_data, aes(x = date, y = nPC)) +
  geom_line(size = 2) +   # Increase the line size
  theme_bw() +
  xlab("Time") +
  ylab("Principal component 1 value") +
  ggtitle("First PC time serie of SEP based on NO2") +
  scale_x_date(limits = c(as.Date("2013-01-01"), as.Date("2017-12-26"))) +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25)     # Increase size of y-axis ticks
  ) +
  geom_vline(xintercept = max_point, linetype = "dashed", size = 1) +  # Add a dashed line at the max point
  annotate("text", x = max_point, y = min(filtered_data$nPC), label = as.character(max_point), hjust = 1, vjust = 0, size = 8)

print(p)
ggsave(filename = "images/eof_NO2_SEP_PCA_1.png", plot = p, width = 10, height = 10)

library(viridis)

# Install and load the necessary packages
if (!requireNamespace("ggmap", quietly = TRUE)) {
  install.packages("ggmap")
}

library(ggmap)


# Define the bounding box for Europe
#bbox <- c(left = -25, bottom = 35, right = 40, top = 65)
bbox <- c(left = -20, bottom = 25, right = 35, top = 65)

# Get the map
map <- get_stamenmap(bbox, zoom = 5, maptype = "toner-lite")

# Create the plot
# Define the breaks
p <- ggmap(map) +
  geom_point(data = EOFs_SEP, aes(x = Longitude, y = Latitude, color = log(EOF1)), size = 3, alpha = 0.5) +
  scale_color_viridis_c(name = "(log) EOF1") +
  theme_bw() +
  xlab("Longitude (deg)") +
  ylab("Latitude (deg)") +
  ggtitle("First EOF of SEP based on NO2") +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25),     # Increase size of y-axis ticks
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )
print(p)
ggsave(filename = "images/eof_NO2_SEP_PCA_1_map.png", plot = p, width = 10, height = 10)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

## Second component 
filtered_data <- TS_SEP %>% filter(EOF_SEP == "X2")

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

library(lubridate)

# Set the starting date
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2017-12-26")

# Calculate the total number of days in the data period
total_days <- as.numeric(difftime(end_date, start_date, units = "days"))

# Calculate the time step size in days
time_step_days <- total_days / (261 - 1)

# Create a new column with the corresponding dates
filtered_data$date <- start_date + (filtered_data$t - 1) * time_step_days

# Create the plot
# Find the date for the maximum point
# Find the date for the maximum point
max_point <- filtered_data[which.max(filtered_data$nPC), "date"]

p <- ggplot(data = filtered_data, aes(x = date, y = nPC)) +
  geom_line(size = 2) +   # Increase the line size
  theme_bw() +
  xlab("Time") +
  ylab("Principal component 2 value") +
  ggtitle("Second PC time serie of SEP based on NO2") +
  scale_x_date(limits = c(as.Date("2013-01-01"), as.Date("2017-12-26"))) +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25)     # Increase size of y-axis ticks
  ) +
  geom_vline(xintercept = max_point, linetype = "dashed", size = 1) +  # Add a dashed line at the max point
  annotate("text", x = max_point, y = min(filtered_data$nPC), label = as.character(max_point), hjust = 1, vjust = 0, size = 8)

print(p)
ggsave(filename = "images/eof_NO2_SEP_PCA_2.png", plot = p, width = 10, height = 10)

library(viridis)

# Install and load the necessary packages
if (!requireNamespace("ggmap", quietly = TRUE)) {
  install.packages("ggmap")
}

library(ggmap)


# Define the bounding box for Europe
#bbox <- c(left = -25, bottom = 35, right = 40, top = 65)
bbox <- c(left = -20, bottom = 25, right = 35, top = 65)


# Get the map
map <- get_stamenmap(bbox, zoom = 5, maptype = "toner-lite")

# Create the plot
p <- ggmap(map) +
  geom_point(data = EOFs_SEP, aes(x = Longitude, y = Latitude, color = log(EOF2)), size = 3, alpha = 0.5) +
  scale_color_viridis_c(name = "(log) EOF2") +
  theme_bw() +
  xlab("Longitude (deg)") +
  ylab("Latitude (deg)") +
  ggtitle("Second EOF of SEP based on NO2") +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25),     # Increase size of y-axis ticks
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )
print(p)
ggsave(filename = "images/eof_NO2_SEP_PCA_2_map.png", plot = p, width = 10, height = 10)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

# NO2 FIM
setwd("C:/EPFL/Master/Semester_project/Pollution_data_EU")
SEP <- read.csv("FIM_NO2_nona.csv")
metadata <- read.csv("metadata_NO2_nona.csv")


SEP <- SEP[,-1]
SEP <- t(SEP)

Z_SEP <- t(SEP)
dim(Z_SEP)

## First find the matrix we need to subtract:
spat_mean_SEP <- apply(SEP, 1, mean)
nT_SEP <- ncol(SEP)

## Then subtract and standardize:
Zspat_detrend_SEP <- Z_SEP - outer(rep(1, nT_SEP), spat_mean_SEP)
Zt_SEP <- 1/sqrt(nT_SEP - 1)*Zspat_detrend_SEP

E_SEP <- svd(Zt_SEP)

V_SEP <- E_SEP$v
colnames(E_SEP$v) <- paste0("EOF", 1:ncol(SEP)) # label columns
EOFs_SEP <- cbind(metadata, E_SEP$v)
head(EOFs_SEP[, 1:6])


library(tidyr)
library(ggplot2)
library(dplyr)

TS_SEP <- data.frame(E_SEP$u) %>% # convert U to data frame
  mutate(t = 1:nrow(E_SEP$u)) %>% # add a time field
  gather(EOF_SEP, PC, -t) # put columns (except time)


TS_SEP$nPC <- TS_SEP$PC * sqrt(nT_SEP-1)

## First component 
filtered_data <- TS_SEP %>% filter(EOF_SEP == "X1")

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

library(lubridate)

# Set the starting date
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2017-12-26")

# Calculate the total number of days in the data period
total_days <- as.numeric(difftime(end_date, start_date, units = "days"))

# Calculate the time step size in days
time_step_days <- total_days / (261 - 1)

# Create a new column with the corresponding dates
filtered_data$date <- start_date + (filtered_data$t - 1) * time_step_days

# Create the plot
# Find the date for the maximum point
# Find the date for the maximum point
max_point <- filtered_data[which.max(filtered_data$nPC), "date"]

p <- ggplot(data = filtered_data, aes(x = date, y = nPC)) +
  geom_line(size = 2) +   # Increase the line size
  theme_bw() +
  xlab("Time") +
  ylab("Principal component 1 value") +
  ggtitle("First PC time serie of FIM based on NO2") +
  scale_x_date(limits = c(as.Date("2013-01-01"), as.Date("2017-12-26"))) +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25)     # Increase size of y-axis ticks
  ) +
  geom_vline(xintercept = max_point, linetype = "dashed", size = 1) +  # Add a dashed line at the max point
  annotate("text", x = max_point, y = min(filtered_data$nPC), label = as.character(max_point), hjust = 1, vjust = 0, size = 8)

print(p)
ggsave(filename = "images/eof_NO2_FIM_PCA_1.png", plot = p, width = 10, height = 10)

library(viridis)

# Install and load the necessary packages
if (!requireNamespace("ggmap", quietly = TRUE)) {
  install.packages("ggmap")
}

library(ggmap)


# Define the bounding box for Europe
#bbox <- c(left = -25, bottom = 35, right = 40, top = 65)
bbox <- c(left = -20, bottom = 25, right = 35, top = 65)


# Get the map
map <- get_stamenmap(bbox, zoom = 5, maptype = "toner-lite")

# Create the plot
p <- ggmap(map) +
  geom_point(data = EOFs_SEP, aes(x = Longitude, y = Latitude, color = log(EOF1)), size = 3, alpha = 0.5) +
  scale_color_viridis_c(name = "(log) EOF1") +
  theme_bw() +
  xlab("Longitude (deg)") +
  ylab("Latitude (deg)") +
  ggtitle("First EOF of FIM based on NO2") +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25),     # Increase size of y-axis ticks
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )
print(p)
ggsave(filename = "images/eof_NO2_FIM_PCA_1_map.png", plot = p, width = 10, height = 10)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

## Second component 
filtered_data <- TS_SEP %>% filter(EOF_SEP == "X2")

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

library(lubridate)

# Set the starting date
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2017-12-26")

# Calculate the total number of days in the data period
total_days <- as.numeric(difftime(end_date, start_date, units = "days"))

# Calculate the time step size in days
time_step_days <- total_days / (261 - 1)

# Create a new column with the corresponding dates
filtered_data$date <- start_date + (filtered_data$t - 1) * time_step_days

# Create the plot
# Find the date for the maximum point
# Find the date for the maximum point
max_point <- filtered_data[which.max(filtered_data$nPC), "date"]

p <- ggplot(data = filtered_data, aes(x = date, y = nPC)) +
  geom_line(size = 2) +   # Increase the line size
  theme_bw() +
  xlab("Time") +
  ylab("Principal component 2 value") +
  ggtitle("Second PC time serie of FIM based on NO2") +
  scale_x_date(limits = c(as.Date("2013-01-01"), as.Date("2017-12-26"))) +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25)     # Increase size of y-axis ticks
  ) +
  geom_vline(xintercept = max_point, linetype = "dashed", size = 1) +  # Add a dashed line at the max point
  annotate("text", x = max_point, y = min(filtered_data$nPC), label = as.character(max_point), hjust = 1, vjust = 0, size = 8)

print(p)
ggsave(filename = "images/eof_NO2_FIM_PCA_2.png", plot = p, width = 10, height = 10)

library(viridis)

# Install and load the necessary packages
if (!requireNamespace("ggmap", quietly = TRUE)) {
  install.packages("ggmap")
}

library(ggmap)


# Define the bounding box for Europe
#bbox <- c(left = -25, bottom = 35, right = 40, top = 65)
bbox <- c(left = -20, bottom = 25, right = 35, top = 65)


# Get the map
map <- get_stamenmap(bbox, zoom = 5, maptype = "toner-lite")

# Create the plot
p <- ggmap(map) +
  geom_point(data = EOFs_SEP, aes(x = Longitude, y = Latitude, color = log(EOF2)), size = 3, alpha = 0.5) +
  scale_color_viridis_c(name = "(log) EOF2") +
  theme_bw() +
  xlab("Longitude (deg)") +
  ylab("Latitude (deg)") +
  ggtitle("Second EOF of FIM based on NO2") +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25),     # Increase size of y-axis ticks
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )
print(p)
ggsave(filename = "images/eof_NO2_FIM_PCA_2_map.png", plot = p, width = 10, height = 10)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#--------------------------------------------------------------------------------------

# PM25 SEP
setwd("C:/EPFL/Master/Semester_project/Pollution_data_EU")
SEP <- read.csv("SEP_PM25_nona.csv")
metadata <- read.csv("metadata_PM25_nona.csv")


SEP <- SEP[,-1]
SEP <- t(SEP)

Z_SEP <- t(SEP)
dim(Z_SEP)

## First find the matrix we need to subtract:
spat_mean_SEP <- apply(SEP, 1, mean)
nT_SEP <- ncol(SEP)

## Then subtract and standardize:
Zspat_detrend_SEP <- Z_SEP - outer(rep(1, nT_SEP), spat_mean_SEP)
Zt_SEP <- 1/sqrt(nT_SEP - 1)*Zspat_detrend_SEP

E_SEP <- svd(Zt_SEP)

V_SEP <- E_SEP$v
colnames(E_SEP$v) <- paste0("EOF", 1:ncol(SEP)) # label columns
EOFs_SEP <- cbind(metadata, E_SEP$v)
head(EOFs_SEP[, 1:6])



library(tidyr)
library(ggplot2)
library(dplyr)

TS_SEP <- data.frame(E_SEP$u) %>% # convert U to data frame
  mutate(t = 1:nrow(E_SEP$u)) %>% # add a time field
  gather(EOF_SEP, PC, -t) # put columns (except time)


TS_SEP$nPC <- TS_SEP$PC * sqrt(nT_SEP-1)



## First component 
filtered_data <- TS_SEP %>% filter(EOF_SEP == "X1")

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

library(lubridate)

# Set the starting date
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2017-12-26")

# Calculate the total number of days in the data period
total_days <- as.numeric(difftime(end_date, start_date, units = "days"))

# Calculate the time step size in days
time_step_days <- total_days / (261 - 1)

# Create a new column with the corresponding dates
filtered_data$date <- start_date + (filtered_data$t - 1) * time_step_days

# Create the plot
# Find the date for the maximum point
# Find the date for the maximum point
max_point <- filtered_data[which.max(filtered_data$nPC), "date"]

p <- ggplot(data = filtered_data, aes(x = date, y = nPC)) +
  geom_line(size = 2) +   # Increase the line size
  theme_bw() +
  xlab("Time") +
  ylab("Principal component 1 value") +
  ggtitle("First PC time serie of SEP based on PM2.5") +
  scale_x_date(limits = c(as.Date("2013-01-01"), as.Date("2017-12-26"))) +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25)     # Increase size of y-axis ticks
  ) +
  geom_vline(xintercept = max_point, linetype = "dashed", size = 1) +  # Add a dashed line at the max point
  annotate("text", x = max_point, y = min(filtered_data$nPC), label = as.character(max_point), hjust = 1, vjust = 0, size = 8)

print(p)
ggsave(filename = "images/eof_PM25_SEP_PCA_1.png", plot = p, width = 10, height = 10)

library(viridis)

# Install and load the necessary packages
if (!requireNamespace("ggmap", quietly = TRUE)) {
  install.packages("ggmap")
}

library(ggmap)


# Define the bounding box for Europe
#bbox <- c(left = -25, bottom = 35, right = 40, top = 65)
bbox <- c(left = -20, bottom = 25, right = 35, top = 65)


# Get the map
map <- get_stamenmap(bbox, zoom = 5, maptype = "toner-lite")

# Create the plot
p <- ggmap(map) +
  geom_point(data = EOFs_SEP, aes(x = Longitude, y = Latitude, color = log(EOF1)), size = 3, alpha = 0.5) +
  scale_color_viridis_c(name = "(log) EOF1") +
  theme_bw() +
  xlab("Longitude (deg)") +
  ylab("Latitude (deg)") +
  ggtitle("First EOF of SEP based on PM2.5") +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25),     # Increase size of y-axis ticks
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )
print(p)
ggsave(filename = "images/eof_PM25_SEP_PCA_1_map.png", plot = p, width = 10, height = 10)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

## Second component 
filtered_data <- TS_SEP %>% filter(EOF_SEP == "X2")

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

library(lubridate)

# Set the starting date
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2017-12-26")

# Calculate the total number of days in the data period
total_days <- as.numeric(difftime(end_date, start_date, units = "days"))

# Calculate the time step size in days
time_step_days <- total_days / (261 - 1)

# Create a new column with the corresponding dates
filtered_data$date <- start_date + (filtered_data$t - 1) * time_step_days

# Create the plot
# Find the date for the maximum point
# Find the date for the maximum point
max_point <- filtered_data[which.max(filtered_data$nPC), "date"]

p <- ggplot(data = filtered_data, aes(x = date, y = nPC)) +
  geom_line(size = 2) +   # Increase the line size
  theme_bw() +
  xlab("Time") +
  ylab("Principal component 2 value") +
  ggtitle("Second PC time serie of SEP based on PM2.5") +
  scale_x_date(limits = c(as.Date("2013-01-01"), as.Date("2017-12-26"))) +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25)     # Increase size of y-axis ticks
  ) +
  geom_vline(xintercept = max_point, linetype = "dashed", size = 1) +  # Add a dashed line at the max point
  annotate("text", x = max_point, y = min(filtered_data$nPC), label = as.character(max_point), hjust = 1, vjust = 0, size = 8)

print(p)
ggsave(filename = "images/eof_PM25_SEP_PCA_2.png", plot = p, width = 10, height = 10)

library(viridis)

# Install and load the necessary packages
if (!requireNamespace("ggmap", quietly = TRUE)) {
  install.packages("ggmap")
}

library(ggmap)


# Define the bounding box for Europe
#bbox <- c(left = -25, bottom = 35, right = 40, top = 65)
bbox <- c(left = -20, bottom = 25, right = 35, top = 65)


# Get the map
map <- get_stamenmap(bbox, zoom = 5, maptype = "toner-lite")

# Create the plot
p <- ggmap(map) +
  geom_point(data = EOFs_SEP, aes(x = Longitude, y = Latitude, color = log(EOF2)), size = 3, alpha = 0.5) +
  scale_color_viridis_c(name = "(log) EOF2") +
  theme_bw() +
  xlab("Longitude (deg)") +
  ylab("Latitude (deg)") +
  ggtitle("Second EOF of SEP based on PM2.5") +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25),     # Increase size of y-axis ticks
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )
print(p)
ggsave(filename = "images/eof_PM25_SEP_PCA_2_map.png", plot = p, width = 10, height = 10)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

# PM25 FIM
setwd("C:/EPFL/Master/Semester_project/Pollution_data_EU")
SEP <- read.csv("FIM_PM25_nona.csv")
metadata <- read.csv("metadata_PM25_nona.csv")


SEP <- SEP[,-1]
SEP <- t(SEP)

Z_SEP <- t(SEP)
dim(Z_SEP)

## First find the matrix we need to subtract:
spat_mean_SEP <- apply(SEP, 1, mean)
nT_SEP <- ncol(SEP)

## Then subtract and standardize:
Zspat_detrend_SEP <- Z_SEP - outer(rep(1, nT_SEP), spat_mean_SEP)
Zt_SEP <- 1/sqrt(nT_SEP - 1)*Zspat_detrend_SEP

E_SEP <- svd(Zt_SEP)

V_SEP <- E_SEP$v
colnames(E_SEP$v) <- paste0("EOF", 1:ncol(SEP)) # label columns
EOFs_SEP <- cbind(metadata, E_SEP$v)
head(EOFs_SEP[, 1:6])

library(tidyr)
library(ggplot2)
library(dplyr)

TS_SEP <- data.frame(E_SEP$u) %>% # convert U to data frame
  mutate(t = 1:nrow(E_SEP$u)) %>% # add a time field
  gather(EOF_SEP, PC, -t) # put columns (except time)


TS_SEP$nPC <- TS_SEP$PC * sqrt(nT_SEP-1)

## First component 
filtered_data <- TS_SEP %>% filter(EOF_SEP == "X1")

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

library(lubridate)

# Set the starting date
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2017-12-26")

# Calculate the total number of days in the data period
total_days <- as.numeric(difftime(end_date, start_date, units = "days"))

# Calculate the time step size in days
time_step_days <- total_days / (261 - 1)

# Create a new column with the corresponding dates
filtered_data$date <- start_date + (filtered_data$t - 1) * time_step_days

# Create the plot
# Find the date for the maximum point
# Find the date for the maximum point
max_point <- filtered_data[which.max(filtered_data$nPC), "date"]

p <- ggplot(data = filtered_data, aes(x = date, y = nPC)) +
  geom_line(size = 2) +   # Increase the line size
  theme_bw() +
  xlab("Time") +
  ylab("Principal component 1 value") +
  ggtitle("First PC time serie of FIM based on PM2.5") +
  scale_x_date(limits = c(as.Date("2013-01-01"), as.Date("2017-12-26"))) +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25)     # Increase size of y-axis ticks
  ) +
  geom_vline(xintercept = max_point, linetype = "dashed", size = 1) +  # Add a dashed line at the max point
  annotate("text", x = max_point, y = min(filtered_data$nPC), label = as.character(max_point), hjust = 1, vjust = 0, size = 8)

print(p)
ggsave(filename = "images/eof_PM25_FIM_PCA_1.png", plot = p, width = 10, height = 10)

library(viridis)

# Install and load the necessary packages
if (!requireNamespace("ggmap", quietly = TRUE)) {
  install.packages("ggmap")
}

library(ggmap)


# Define the bounding box for Europe
bbox <- c(left = -20, bottom = 25, right = 35, top = 65)


# Get the map
map <- get_stamenmap(bbox, zoom = 5, maptype = "toner-lite")
dev.off()

# Create the plot
p <- ggmap(map) +
  geom_point(data = EOFs_SEP, aes(x = Longitude, y = Latitude, color = log(EOF1)), size = 3, alpha = 0.5) +
  scale_color_viridis_c(name = "(log) EOF1") +
  theme_bw() +
  xlab("Longitude (deg)") +
  ylab("Latitude (deg)") +
  ggtitle("First EOF of FIM based on PM2.5") +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25),     # Increase size of y-axis ticks
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )
print(p)
ggsave(filename = "images/eof_PM25_FIM_PCA_1_map.png", plot = p, width = 10, height = 10)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

## Second component 
filtered_data <- TS_SEP %>% filter(EOF_SEP == "X2")

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

library(lubridate)

# Set the starting date
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2017-12-26")

# Calculate the total number of days in the data period
total_days <- as.numeric(difftime(end_date, start_date, units = "days"))

# Calculate the time step size in days
time_step_days <- total_days / (261 - 1)

# Create a new column with the corresponding dates
filtered_data$date <- start_date + (filtered_data$t - 1) * time_step_days

# Create the plot
# Find the date for the maximum point
# Find the date for the maximum point
max_point <- filtered_data[which.max(filtered_data$nPC), "date"]

p <- ggplot(data = filtered_data, aes(x = date, y = nPC)) +
  geom_line(size = 2) +   # Increase the line size
  theme_bw() +
  xlab("Time") +
  ylab("Principal component 2 value") +
  ggtitle("Second PC time serie of FIM based on PM2.5") +
  scale_x_date(limits = c(as.Date("2013-01-01"), as.Date("2017-12-26"))) +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25)     # Increase size of y-axis ticks
  ) +
  geom_vline(xintercept = max_point, linetype = "dashed", size = 1) +  # Add a dashed line at the max point
  annotate("text", x = max_point, y = min(filtered_data$nPC), label = as.character(max_point), hjust = 1, vjust = 0, size = 8)

print(p)
ggsave(filename = "images/eof_PM25_FIM_PCA_2.png", plot = p, width = 10, height = 10)

library(viridis)

# Install and load the necessary packages
if (!requireNamespace("ggmap", quietly = TRUE)) {
  install.packages("ggmap")
}

library(ggmap)


# Define the bounding box for Europe
bbox <- c(left = -20, bottom = 25, right = 35, top = 65)


# Get the map
map <- get_stamenmap(bbox, zoom = 5, maptype = "toner-lite")

# Create the plot
p <- ggmap(map) +
  geom_point(data = EOFs_SEP, aes(x = Longitude, y = Latitude, color = log(EOF2)), size = 3, alpha = 0.5) +
  scale_color_viridis_c(name = "(log) EOF2") +
  theme_bw() +
  xlab("Longitude (deg)") +
  ylab("Latitude (deg)") +
  ggtitle("Second EOF of FIM based on PM2.5") +
  theme(
    plot.title = element_text(size = 25),     # Increase size of title
    axis.title.x = element_text(size = 25),   # Increase size of x-axis label
    axis.title.y = element_text(size = 25),   # Increase size of y-axis label
    axis.text.x = element_text(size = 25),    # Increase size of x-axis ticks
    axis.text.y = element_text(size = 25),     # Increase size of y-axis ticks
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25)
  )
print(p)
ggsave(filename = "images/eof_PM25_FIM_PCA_2_map.png", plot = p, width = 10, height = 10)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-