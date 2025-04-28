library(ggplot2)
library(readr) 

mt<-read.csv("C:/Users/chikazhet/MACC2.csv")
mt$mitigation <- factor(mt$mitigation, levels= c("Nitrogen use efficiency", "nutrient recycling", "Plantain", "Deintensification", "Off-paddock structure"))

ggplot(data = mt, mapping = aes(y = cost, x = abatement, group = mitigation, 
                                colour = mitigation, shape = mitigation)) +
  geom_point(size = 3) +  # Ensure points are plotted
  
  # Define shape scale manually for all factor levels
  scale_shape_manual(values = c("Nitrogen use efficiency" = 16,
                                "nutrient recycling" = 15,
                                "Plantain" = 7,
                                "Deintensification" = 17,
                                "Off-paddock structure" = 8)) +
  
  # Ensure colour and shape guides are merged
  guides(colour = guide_legend(title = "Mitigation"), 
         shape = guide_legend(title = "Mitigation")) +
  
  scale_x_continuous(position = "bottom") +
  scale_y_continuous(position = "right") +
  facet_grid(off_paddock ~ surplus, switch = "y")+
 
  labs(y = "MAC($/kg N reduced)", x = "Abatement potential (% N leaching reduced)") +
  
  theme(legend.position = "bottom")



library(ggplot2)
library(dplyr)

# Load data
df <- read.csv("C:/Users/chikazhet/MACC2.csv")

# Ensure ordering by cost for MACC
MyData3<- df

aggregated_data <- MyData3 %>%
  group_by(surplus, mitigation, off_paddock ) %>%
  arrange(cost) %>%
  mutate(cumulative_abatement = cumsum(abatement))%>%
  #filter(off_paddock == "No existing off paddock structure")%>%
  filter(surplus == "Low N surplus")%>%
  summarise(
    mean_cumulative_abatement = mean(abatement, na.rm = TRUE),
    mean_cost = mean(cost, na.rm = TRUE)
  )
aggregated_data <- aggregated_data[order(aggregated_data$mean_cost), ]

aggregated_data$xmin <- c(0, head(cumsum(aggregated_data$mean_cumulative_abatement), -1))
aggregated_data$xmax <- cumsum(aggregated_data$mean_cumulative_abatement)

# Plot MACC with facets
ggplot(aggregated_data, aes(x = mean_cumulative_abatement, y = mean_cost, fill = mitigation)) +
  geom_col() +
  #facet_wrap(~surplus ) +
  labs(x = "Cumulative Abatement Potential", 
       y = "Abatement Cost ($/kg N reduced)",
       title = "Marginal Abatement Cost Curve (MACC)") +
  theme_minimal()



