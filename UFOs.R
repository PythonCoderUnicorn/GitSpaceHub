# UFO sitings data 

#======== the libraries for mapping

library(tidyverse)
library(lubridate) 
library(sf)
library(leaflet)
library(mapview)
library(huxtable)
library(gdtools)
library(systemfonts)
library(flextable)
library(ggplot2)
library(ggsn)
library(dplyr)
library(tibble)
library(ggpubr)
library(vegan)
# library(nationalparkcolors) 
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(rgeos)



# read in the data
ufo_sightings =  readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv") 


# see countries in dataset
unique(ufo_sightings$country)



## Subset data to only one country
# let's subset to just Country of choice 

ufo2 <- subset(ufo_sightings, country == "ca")



# drop the NA values
ufo3<- na.omit(ufo2)

## Remove "unknown" UFO shapes 
ufo4<- ufo3[ufo3$ufo_shape != "unknown",] 




# base R basic map
gx = ufo4$longitude
gy = ufo4$latitude


## Start with a basic plot. 
plot(gx, gy, xlab = "Latitude", ylab = "Longitude", main="CANADA UFO Sightings")




## Reducing to only one state in the dataset 
ufo.ONT<-ufo4[ufo4$state == "on",] 


## Now let's get picky about which UFO shapes we will include
unique(ufo.ONT$ufo_shape)      # to print out the different shape options



# Reduce the shapes to a few
ufo5<-ufo.ONT[ufo.ONT$ufo_shape %in% c("fireball",
                                       "chevron",
                                       "flash",
                                       "flare",
                                       "disk"),]


## Base plot again to see how this differs
rx = ufo5$longitude
ry = ufo5$latitude

## Start with a basic plot. 
plot(rx, ry, xlab = "Latitude", ylab = "Longitude", main="Ontario UFO Sightings")





## Simplest GGPLOT 
ufo5 %>% 
  ggplot()+
  geom_point(mapping = (aes(x = longitude, y = latitude)), alpha=0.4)+
  ggtitle("Ontario UFO Sightings")+
  ggdark::dark_mode()
  
  

# bubble plot
cities_ufo <- ufo5 %>% 
  group_by(city_area) %>% 
  summarize(
    latitude=mean(latitude), 
  # because summarize is going to shrink our rows we want to keep 
  # this data in it. We could use it as a grouping variable,
  # but likely there are lat long differences within city areas?
    longitude=mean(longitude),
    n_sightings = length(description)
  )
 
 
## Beginnings of a bubble plot...
cities_ufo %>% 
  ggplot()+
  geom_point(mapping = (aes(x = longitude, y = latitude, size=n_sightings)), alpha=0.4)+
  ggtitle("Ontario UFO Sightings by city")+
  ggdark::dark_mode()
  


## Boundaries & backgrounds  
#  Start with the natural earth package and data set, then zoom into west coast. 

library(mapview)
mapview(ufo2, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)
 
 
 
 
 














