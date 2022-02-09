# How-To: A Fun Exercise in Data Visualisation
# Hurricanes Project Essentials
# Packages
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
library(gganimate)

# May need these packages
# library(RColorBrewer)
# install.packages("ggExtra")
# library(ggExtra)
# install.packages("ggpointdensity")
# library(ggpointdensity)


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

# Creating data set from NHC repository: Plot incorrect value
ggplot(hurricanes, aes(x = Pressure, y = Wind, color = Latitude)) +
  geom_point() + 
  labs(title = "Wind-Pressure Relationship",
       y = "Wind",
       x = "Pressure")

# Creating data set from NHC repository: Dropping incorrect value 
hurricanes[which(hurricanes$Wind == 32, arr.ind = TRUE), 4] = 30

# Creating data set  from NHC repository: Replot previous value
ggplot(hurricanes, aes(x = Pressure, y = Wind, color = Latitude)) +
  geom_point() + 
  labs(title = "Wind-Pressure Relationship",
       y = "Wind",
       x = "Pressure")

# Transform dates from string of numeric with month and day to numeric of year
hurricanes$Date <- as.character(hurricanes$Date)
hurricanes$Date <- as.numeric(substr(hurricanes$Date,1,4))

# Changes date column to as.date -- probably not necessary
# hurricanes <- transform(hurricanes, Date = as.Date(as.character(Date), "%Y%m%d"))

# Creating data set  from NHC repository: Categorizing intensities
hurricanes <- hurricanes %>% 
  mutate(Intensity = case_when(Wind < 40 ~ "tropical depression",
                               Wind >= 39 & Wind < 60 ~ "tropical storm",
                               Wind >= 60 & Wind < 100 ~ "hurricane",
                               Wind >= 100 ~ "major hurricane"))

# Saving Excel File 
hurricanes_cleaned <- write.xlsx(hurricanes,"Hurricanes_Github.xlsx")

# Heat map by frequency
library(raster)
ggplot(data = world) + 
  geom_sf() + 
  coord_sf(xlim = c(0, -109.3), ylim = c(7.20, 70.70), expand = FALSE) + 
  geom_density2d(mapping = aes(x = Longitude, y = Latitude), color = "black", data = hurricanes) + 
  stat_density2d(data = hurricanes, aes(x = Longitude, y = Latitude,
                                          fill = ..level.., alpha = ..level..),
                 bins = 25, geom = "polygon") + 
  scale_fill_gradient(low = "blue", high = "pink", name = "Observed Activity", labels = c("none", "rare", "periodic", "frequent", "maximum")) + 
  scale_alpha(range = c(0.02,0.2), guide = "none") +
  labs(title = "Storm Observation Heatmap",
       x = "Longitude",
       y = "Latitude") 


# Creating plots for all seasons in the satellite era 

i <- c(1966:2015)
for (i in 1966:2015){
  ggsave(ggplot(data = world) + 
           geom_sf() + 
           coord_sf(xlim = c(0, -109.3), ylim = c(7.20, 70.70), expand = FALSE) + 
           geom_density2d(mapping = aes(x = Longitude, y = Latitude), color = "black", 
                          data = subset(hurricanes, Date == i), bins = 7) + 
           stat_density2d(data = subset(hurricanes, Date == i), aes(x = Longitude, y = Latitude,
                                                                    fill = ..level.., alpha = ..level..),geom = "polygon") + 
           scale_fill_gradient(low = "blue", high = "pink", name = "Observed Activity", 
                               labels = c("none", "rare", "periodic", "frequent", "maximum"),
                               breaks = c(75,150,225,300,375)) + 
           scale_alpha(range = c(0.05,0.38), guide = "none") + 
           facet_wrap(~ Intensity) +
           labs(title = paste("Hurricane Heatmap | Year: ", i, sep = ""),
                x = "Longitude",
                y = "Latitude") 
         , filename = paste("HurricanesD", i, ".png",sep = "")
  )
}


# End
