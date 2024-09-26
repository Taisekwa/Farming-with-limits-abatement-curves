
# install.packages("maps")
# install.packages('ggmap')
# install.packages("maptools", repos="http://R-Forge.R-project.org")
# install.packages("quanteda.textmodels")
# install.packages("quanteda.textstats")
# install.packages("quanteda.textplots")
# install.packages("sf")
# install.packages("mapview")
#Load the necessary libraries
library(tidyverse)
# library(ggplot12)
# library("reshape2")
# library("ggplot2")
# library(KernSmooth)
# library(dplyr)
# library(maps)
# library("ggmap")
# library(maptools)
# library(maps)
# library(tidyverse)
library(sf)
library(sp)
library(raster)
library(rgdal)
# library(mapview)

world_map <- map_data("world")
nz1 <- getData("GADM", country = "NZ", level = 1)
ggplot(nz1) +
  geom_path(aes(x = long, y = lat, group = group))

df<-read.csv("C:/Users/chikazhet/Otagomap.csv")
nzmap <- map("nz1")
ggplot() + 
  #geom_polygon(data=df, aes(x=longitude, y=latitude, group=region)) +
  geom_path(data = df, aes(x = longitude, y = latitude, group = region)) +
  geom_point(data=df, aes(x=longitude, y=latitude, colour=region), size=5, alpha=I(0.7))


nz1 <- getData("GADM", country = "NZ", level = 1)
nz1$NAME_2 <- as.factor(nz1$NAME_1)
nz1$fake.data <- runif(length(nz1$NAME_1)) 
spplot(nz1,
       "NAME_2", 
       xlim=c(163,180), 
       scales=list(draw=T), 
       ylim=c(-50,-32), 
       col.regions=rgb(nz1$fake.data, 1-nz1$fake.data, 0), 
       colorkey=F)+
ggplot() + 
  #geom_polygon(data=df, aes(x=longitude, y=latitude, group=region)) +
  geom_path(data = nz1, aes(x = longitude, y = latitude, group = region)) +
  geom_point(data=df, aes(x=longitude, y=latitude, colour=region), size=5, alpha=I(0.7))
   
