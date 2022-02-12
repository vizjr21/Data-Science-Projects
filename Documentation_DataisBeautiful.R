
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(maps)
library(raster)
install.packages("rnaturalearth")
library("rnaturalearth")
install.packages("rnaturalearthdata")
library("rnaturalearthdata")
install.packages("wk")
install.packages("openxlsx")
library("openxlsx")
install.packages("grDevices")
library("grDevices")
library(animation)

# Creating sf object for world map 
world <- ne_countries(scale = "medium", returnclass = "sf")

# Creating data set  from NHC repository: Loading data 
hurricanes <- read.csv("atlantic.csv")

# Creating data set  from NHC repository: Selecting needed variables
hurricanes <- subset(hurricanes, 
                     select = c(Name, Date, Time, Maximum.Wind, Minimum.Pressure, Latitude, Longitude))

# Creating data set  from NHC repository: Renaming columns 
colnames(hurricanes) <- c("Name", "Date", "Time", "Wind", "Pressure", "Latitude","Longitude")

# Creating data set  from NHC repository: Dropping NAs
hurricanes <- hurricanes[hurricanes$Pressure != -999,]

# Creating data set  from NHC repository: removing letters from lat/lon
hurricanes$Latitude <- as.numeric(gsub("N","",hurricanes$Latitude, fixed = TRUE))
hurricanes$Longitude <- as.numeric(gsub("W","", hurricanes$Longitude, fixed = TRUE))

# Creating data set  from NHC repository: transforming longitude variable 
hurricanes$Longitude <- -hurricanes$Longitude

# Creating data set from NHC repository: Dropping incorrect value 
hurricanes[which(hurricanes$Wind == 32, arr.ind = TRUE), 4] = 30

# Transform dates from string of numeric with month and day to numeric of year
hurricanes$Date <- as.character(hurricanes$Date)
hurricanes$Date <- as.numeric(substr(hurricanes$Date,1,4))


# Loop to create GIF
i <- c(1998:2012)
saveGIF(for (i in 1998:2012){
  print(ggplot(data = world) + 
          geom_sf() + 
          coord_sf(xlim = c(0, -109.3), ylim = c(7.20, 70.70), expand = FALSE) + 
          geom_density2d(mapping = aes(x = Longitude, y = Latitude), color = "black", 
                         data = subset(hurricanes, Date == i), bins = 8) + 
          stat_density2d(data = subset(hurricanes, Date == i), aes(x = Longitude, y = Latitude,
                                                                   fill = ..level.., alpha = ..level..),geom = "polygon") + 
          scale_fill_gradient(low = "blue", high = "pink", name = "Observed Activity", 
                              labels = c("none", "rare", "periodic", "frequent", "maximum"),
                              breaks = c(75,150,225,300,375)) + 
          scale_alpha(range = c(0.07,0.38), guide = "none") + 
          facet_wrap(~ Intensity) +
          labs(title = paste("Hurricane Heatmap | Year: ", i, sep = ""),
               x = "Longitude",
               y = "Latitude"))
}, interval = 0.5)
