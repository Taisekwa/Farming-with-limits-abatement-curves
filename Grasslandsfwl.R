
#Load the necessary libraries
library(ggplot2)
library(tidyverse)
library("reshape2")
library(KernSmooth)
library(tidyr)
library(dplyr)

df<-read.csv("C:/Users/chikazhet/hauraki2.csv")

MyData2<- df

aggregated_data <- MyData2 %>%
  group_by(surplus, Mitigation,off_paddock ) %>%
  summarise(
    mean_profit_change = mean(profit_change, na.rm = TRUE),
    mean_leaching_change = mean(leaching_change, na.rm = TRUE)
  )

  
  #plotting profit and N leaching
  aggregated_data$Mitigation <- factor(aggregated_data$Mitigation, levels= c("Base", "Nitrogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification", "Off-paddock structure"))
  aggregated_data$surplus <- factor(aggregated_data$surplus, levels = c("High N surplus", "Low N surplus"))  # Custom order
  aggregated_data$off_paddock <- factor(aggregated_data$off_paddock, levels = c("Existing off paddock structure", "No existing off paddock structure"))  # Custom order
  ggplot(data = aggregated_data, mapping = aes(y = mean_profit_change, x = mean_leaching_change))+
    scale_x_continuous(position="top") +
    scale_y_continuous(position="right")+
    geom_point(aes(shape=Mitigation,colour = Mitigation), size=4)+
    scale_shape_manual(values=c("Base"=10, "Nitrogen use efficiency"=16,"Nutrient recycling"=15, "Plantain"=7, "Deintensification"= 17, "Off-paddock structure"=8))+ 
    geom_line()+
    facet_grid(off_paddock ~ surplus)+
    theme(legend.position = "bottom") +
    labs(y="Change in operating profit(%)", x="Change in N leaching(%)") 
  
  
  
  #Plotting profit aggregate
  # Ensure correct ordering of data
  aggregated_data2$Mitigation <- factor(aggregated_data2$Mitigation,
                              levels = c("Base", "Nitrogen use efficiency", "Nutrient recycling", 
                              "Plantain", "Deintensification", "Off-paddock structure"))
  
  aggregated_data2 <- aggregated_data2 %>%
    arrange(profit, Mitigation)
  
  # Plot with explicit grouping and correct ordering
  ggplot(data = aggregated_data2, 
         mapping = aes(y = mean_profit_change, 
                       x = mean_leaching_change, 
                       group = profit)) +  # Ensure correct grouping
    scale_x_continuous(position = "top") +
    scale_y_continuous(position = "right") +
    geom_path(aes(group = profit, colour = Mitigation)) +  # Ensure color by profit
    geom_point(aes(shape = Mitigation, colour = Mitigation), size = 3) +
    scale_shape_manual(values = c("Base" = 10, 
                                  "Nitrogen use efficiency" = 16,
                                  "Nutrient recycling" = 15, 
                                  "Plantain" = 7, 
                                  "Deintensification" = 17, 
                                  "Off-paddock structure" = 8)) +  
    facet_wrap(~ profit) +
    theme(legend.position = "bottom") +
    labs(y = "Change in operating profit (%)", 
         x = "Change in N leaching (%)")
  
  
  
  
  


