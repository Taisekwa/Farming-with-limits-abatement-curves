
#Load the necessary libraries
library(tidyverse)
library(ggplot2)
library("reshape2")
library("ggplot2")
library(KernSmooth)
library(dplyr)

df<-read.csv("C:/Users/chikazhet/horizon.csv")

# All mitigations plotted
MyData2<- df


#plotting profit and N leaching
ggplot(data = MyData2, mapping = aes(y = profit_change, x = leaching_change, group= Farmid))+
scale_x_reverse(position="top")+
  geom_line(aes(color= Farmid),size=1)+ 
  scale_colour_manual(values = c("green","black","yellow","blue","red", "orange", "purple" ,"grey" ,"cyan"))+
  geom_point(aes(shape=mitigation), size=2)+scale_shape_manual(values=c("base"=10, "GMP"=16,"Plantain"=7, "Crops"=15, "Half N"= 17, "Zero N"=25, "Standoff"=8, "Retire land"=4))+  
  labs(y="Change in operating profit(%)", x="Change in N leaching(%)") 


##plotting profit and GHG
  ggplot(data = MyData2, mapping = aes(y = profit_change, x = ghg_change, group= Farmid))+
  scale_x_reverse(position="top")+
  geom_line(aes(color= Farmid),size=1)+ 
  scale_colour_manual(values = c("green","black","yellow","blue","red", "orange", "purple" ,"grey" ,"cyan"))+
  geom_point(aes(shape=mitigation), size=2)+scale_shape_manual(values=c("Base"=10, "GMP"=16,"Plantain"=7, "Crops"=15, "Half N"= 17, "Zero N"=25, "Standoff"=8, "Retire land"=4))+  
  labs(y="Change in operating profit(%)", x="Change in GHG(%)") 

## Plotting profit and p loss
  ggplot(data = MyData2, mapping = aes(y = profit_change, x = p_change, group= Farmid))+
    scale_x_reverse(position="top")+
    geom_line(aes(color= Farmid),size=1)+ 
    scale_colour_manual(values = c("green","black","yellow","blue","red", "orange", "purple" ,"grey" ,"cyan"))+
    geom_point(aes(shape=mitigation), size=2)+scale_shape_manual(values=c("Base"=10, "GMP"=16,"Plantain"=7, "Crops"=15, "Half N"= 17, "Zero N"=25, "Standoff"=8, "Retire land"=4))+  
    labs(y="Change in operating profit(%)", x="Change in P loss(%)") 
  
  MyData2$mitigation <- factor(MyData2$mitigation, levels= c("Base", "GMP", "Plantain", "Crops", "Half N", "Zero N", "Standoff", "Retire land"))
  ## plotting profit and N leaching
  ggplot(data =MyData2, mapping = aes(y = profit_change, x = leaching_change, group= mitigation)) +
  scale_x_reverse(position="top") +
  geom_point(aes(color= mitigation),size=2)+ scale_colour_manual(values = c("grey", "green","black","yellow","blue","red", "orange", "purple" ))+
  scale_fill_discrete(limits = c("Base", "GMP", "Plantain", "Crops", "Half N", "Zero N", "Standoff", "Retire land")) +
  labs(y="Change in operating profit(%)", x="Change in N leaching(%)") + 
    theme(legend.title=element_blank()) 
  
  MyData2$mitigation <- factor(MyData2$mitigation, levels= c("Base", "GMP", "Plantain", "Crops", "Half N", "Zero N", "Standoff", "Retire land"))
  ## plotting profit and N leaching
  ggplot(data =MyData2, mapping = aes(y = profit_change, x = ghg_change, group= mitigation)) +
    scale_x_reverse(position="top") +
    geom_point(aes(color= mitigation),size=2)+ scale_colour_manual(values = c("grey", "green","black","yellow","blue","red", "orange", "purple" ))+
    scale_fill_discrete(limits = c("Base", "GMP", "Plantain", "Crops", "Half N", "Zero N", "Standoff", "Retire land")) +
    labs(y="Change in operating profit(%)", x="Change in GHG(%)") + 
    theme(legend.title=element_blank()) 
  
  # Farms location
  
  library(dplyr)
  library(leaflet)
  
  df1 <- read.csv("C:/Users/chikazhet/horizonmap.csv")
  colnames(df1)[1] <- "Farm"
  df1$region <- factor(df1$region)
  #df1$region <- LETTERS[df1$region]
  
  # If you want to set your own colors manually:
  pal <- colorFactor(
    palette = c('red', 'blue', 'green', 'black'),
    domain = df1$region
  )
  
  ## OR automatically generate color palettes
  pal <- colorFactor(
    palette = 'Dark2',
    domain = df1$region
  )
  
  # Create leaflet map
  leaflet(df1) %>%
    addTiles() %>%
    addCircles(lng = ~longitude, 
               lat = ~latitude, 
               color = ~pal(region), 
               radius = 3000)%>% 
    addLegend("topright",  pal = pal,labels =df1$Farm,  values = df1$region, title="Selected Horizon case study farms")
  