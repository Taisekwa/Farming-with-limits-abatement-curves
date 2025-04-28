library(tidyverse)
library(reshape2)
library(dplyr)

# Read data from file
data <- read.csv("C:/Users/chikazhet/ludftai.csv")

#Inspect the data
#Inspect the data
str(data)
# filtering data based on season
#filtered_data <- data %>%
# filter(Season %in% c(2012-13, 2015-16,2021-22, 2023-24))




#Selecting the variables we need
ding2 <- select(data,Season,Anon.ID,op_profit_ha,few_kgms,milksolids_ha,methane_tco2eq_ha,total_feed_eaten_ha,pasture_crop_harvested_ha,purchased_n_surplus_ha,n_applied_ha,purchased_supp_eaten_tha)

# Presenting the data into a long format
molten.data <- melt(ding2, id = c("Season","Anon.ID" ))


# Plotting data on multiple-variables 2021-22
p2 <- ggplot(molten.data, aes(y = value, x = Season, fill = Season)) + 
  geom_boxplot() +
  geom_point(data = subset(molten.data, Anon.ID == "LUDF"), color = "green", size = 2) +
  geom_text(data = subset(molten.data, Anon.ID == "LUDF"), 
            aes(label = "LUDF"), 
            hjust = -0.2, vjust = -0.2, size = 2,fontface="bold", color = "black") +
  theme(legend.position = "none") +
  facet_wrap(~variable, scales = "free", labeller = as_labeller(
    c(op_profit_ha = "Operating profit $/ha", few_kgms = "Farm working expenses $/kgMS",
      milksolids_ha = "Milksolids kg/ha", methane_tco2eq_ha = "Methane t CO2 equiv/ha",
      total_feed_eaten_ha = "Total feed eaten t DM/ha", pasture_crop_harvested_ha = "Pasture and crop eaten t DM/ha",
      purchased_n_surplus_ha = "N surplus kg N/ha", n_applied_ha = "N fertiliser kg N/ha",
      purchased_supp_eaten_tha = "Imported supplements eaten t DM/ha"))) +
  labs(x = NULL, y = NULL) +
  theme(axis.title.x = element_text(face = "bold", size = 10), axis.text.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(face = "bold", size = 10), axis.text.y = element_text(size = 8)) +
  theme(axis.line.x = element_line(color = "black", size = 0.5), axis.line.y = element_line(color = "black", size = 0.5)) 

p2


# Load ggplot2 package
library(ggplot2)

# Sample Data for N Leaching MACC
macc_data <- data.frame(
  Strategy = c("P21", "10 in 7", "Ecopond", "Future low replacement", "Future Italian", "Future Italian + Plantain", "Future DCD"),
  Abatement = c(22, 14, 0, 0.2, 3, 3, 4),   # Abatement potential in kg N leaching reduced
  Cost = c(4, 111, 0, -100, 39, 62, 113)  # Cost per  per kg N leaching reduced in NZD (negative values for cost-saving measures)
)

# Order data by cost for proper visualization
macc_data <- macc_data[order(macc_data$Cost), ]

# Define xmin and xmax for each bar
macc_data$xmin <- c(0, head(cumsum(macc_data$Abatement), -1))
macc_data$xmax <- cumsum(macc_data$Abatement)

# Reference line for carbon credit price (example value, adjust as needed)
carbon_credit_price <- 50 

# Plot MACC with connected bars
ggplot(macc_data, aes(y = Cost, fill=Strategy)) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Cost), color = "black", size = 0.3) +
  #geom_hline(yintercept = carbon_credit_price, linetype = "dashed", color = "black") +  # Reference line
  #annotate("text", x = max(macc_data$xmax) * 0.5, y = carbon_credit_price + 5, 
           #label = "Price of carbon credits", size = 5, fontface = "italic") +
  scale_x_continuous("Cummulative Abatement Potential (kg N/ha leaching reduced)", expand = c(0, 0)) +
  scale_y_continuous("Marginal Abatement Cost (NZ$ per kg N/ha leaching reduced)", expand = c(0, 0)) +
  scale_fill_discrete(breaks = c("Future low replacement",
                                 "P21","Ecopond",
                                 "Future Italian",
                                 "Future Italian + Plantain", 
                                  "10 in 7", 
                                  "Future DCD")) +  
  #labs(fill = "Strategy") +
  labs(title = "LUDF N leaching Marginal Abatement Cost Curve (MACC)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 



# Sample Data for GHG MACC
macc_data <- data.frame(
  Strategy = c("P21", "10 in 7", "Ecopond", "Future low replacement", "Future Italian", "Future Italian + Plantain", "Future DCD"),
  Abatement = c(1.6, 1.5, 0.845, 0.07, 0.54, 0, 0.26),   # Abatement potential in tons of CO2  reduced
  Cost = c(48, 1003, 78, -2541, 207, 0, 1744)  # Cost per ton or per kg in USD (negative values for cost-saving measures)
)

# Order data by cost for proper visualization
macc_data <- macc_data[order(macc_data$Cost), ]

# Define xmin and xmax for each bar
macc_data$xmin <- c(0, head(cumsum(macc_data$Abatement), -1))
macc_data$xmax <- cumsum(macc_data$Abatement)

# Reference line for carbon credit price (example value, adjust as needed)
carbon_credit_price <- 50 

# Plot MACC with connected bars
ggplot(macc_data, aes(y = Cost, fill=Strategy)) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Cost), color = "black", size = 0.3) +
  #geom_hline(yintercept = carbon_credit_price, linetype = "dashed", color = "black") +  # Reference line
  #annotate("text", x = max(macc_data$xmax) * 0.5, y = carbon_credit_price + 5, 
  #label = "Price of carbon credits", size = 5, fontface = "italic") +
  scale_x_continuous("Cummulative Abatement Potential (tCO2equiv/ha reduced)", expand = c(0, 0)) +
  scale_y_continuous("Marginal Abatement Cost (NZ$ per tCO2equiv/ha reduced)", expand = c(0, 0)) +
  scale_fill_discrete(breaks = c("Future low replacement",
                                 "P21","Ecopond",
                                 "Future Italian","10 in 7","Future DCD"
                                  
                                 )) +  
  #labs(fill = "Strategy") +
  labs(title = "LUDF GHG Marginal Abatement Cost Curve (MACC)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 




