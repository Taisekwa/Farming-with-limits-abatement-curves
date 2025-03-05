

# Load ggplot2 package
library(ggplot2)

# Horizon Farm8
# Sample Data for N Leaching MACC
macc_data <- data.frame(
  Strategy = c("Nutrient recycling", "Plantain", "Off-paddock structure"),
  Abatement = c(1, 6, 14),   # Abatement potential in kg N leaching reduced
  Cost = c(52, 56, 155)  # Cost per  per kg N leaching reduced in NZD (negative values for cost-saving measures)
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
  scale_x_continuous("Cummulative Abatement Potential (% N leaching reduced)", expand = c(0, 0)) +
  scale_y_continuous("Marginal Abatement Cost (NZ$ per kg N/ha leaching reduced)", expand = c(0, 0)) +
  scale_fill_discrete(breaks = c("Nutrient recycling","Plantain",
                                 "Off-paddock structure")) +  
  #labs(fill = "Strategy") +
  labs(title = "Horizon Farm 8 N leaching Marginal Abatement Cost Curve (MACC)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 


#Horizon Farm 6
# Load ggplot2 package
library(ggplot2)

# Sample Data for N Leaching MACC
macc_data <- data.frame(
  Strategy = c("Nitrogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification", "Off-paddock structure"),
  Abatement = c(8, 2, 7, 23,10),   # Abatement potential in kg N leaching reduced
  Cost = c(-1.25, 48, 14, 31, 94)  # Cost per  per kg N leaching reduced in NZD (negative values for cost-saving measures)
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
  scale_x_continuous("Cummulative Abatement Potential (% N leaching reduced)", expand = c(0, 0)) +
  scale_y_continuous("Marginal Abatement Cost (NZ$ per kg N/ha leaching reduced)", expand = c(0, 0)) +
  scale_fill_discrete(breaks = c("Nitrogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification", "Off-paddock structure")) +  
  #labs(fill = "Strategy") +
  labs(title = "Horizon Farm 6 N leaching Marginal Abatement Cost Curve (MACC)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 



#Horizon Farm 4
# Load ggplot2 package
library(ggplot2)

# Sample Data for N Leaching MACC
macc_data <- data.frame(
  Strategy = c("Nitrogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification", "Off-paddock structure"),
  Abatement = c(3, 8, 6, 11,13),   # Abatement potential in kg N leaching reduced
  Cost = c(-2, -2, 48, 82, 313)  # Cost per  per kg N leaching reduced in NZD (negative values for cost-saving measures)
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
  scale_x_continuous("Cummulative Abatement Potential (% N leaching reduced)", expand = c(0, 0)) +
  scale_y_continuous("Marginal Abatement Cost (NZ$ per kg N/ha leaching reduced)", expand = c(0, 0)) +
  scale_fill_discrete(breaks = c("Nitrogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification", "Off-paddock structure")) +  
  #labs(fill = "Strategy") +
  labs(title = "Horizon Farm 4 N leaching Marginal Abatement Cost Curve (MACC)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 


#Horizon Farm 5
# Load ggplot2 package
library(ggplot2)

# Sample Data for N Leaching MACC
macc_data <- data.frame(
  Strategy = c("Plantain", "Off-paddock structure", "Deintensification"),
  Abatement = c(16, 21, 9),   # Abatement potential in kg N leaching reduced
  Cost = c(12, 93, 360)  # Cost per  per kg N leaching reduced in NZD (negative values for cost-saving measures)
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
  scale_x_continuous("Cummulative Abatement Potential (% N leaching reduced)", expand = c(0, 0)) +
  scale_y_continuous("Marginal Abatement Cost (NZ$ per kg N/ha leaching reduced)", expand = c(0, 0)) +
  scale_fill_discrete(breaks = c("Plantain", "Deintensification", "Off-paddock structure")) +  
  #labs(fill = "Strategy") +
  labs(title = "Horizon Farm 5 N leaching Marginal Abatement Cost Curve (MACC)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 



#Hauraki Farm 9
# Load ggplot2 package
library(ggplot2)

# Sample Data for N Leaching MACC
macc_data <- data.frame(
  Strategy = c("Nitrogen use efficiency","Plantain", "Off-paddock structure","Deintensification"),
  Abatement = c(4, 9, 9,24),   # Abatement potential in kg N leaching reduced
  Cost = c(1.5, 20, 70, 106)  # Cost per  per kg N leaching reduced in NZD (negative values for cost-saving measures)
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
  scale_x_continuous("Cummulative Abatement Potential (% N leaching reduced)", expand = c(0, 0)) +
  scale_y_continuous("Marginal Abatement Cost (NZ$ per kg N/ha leaching reduced)", expand = c(0, 0)) +
  scale_fill_discrete(breaks = c("Nitrogen use efficiency","Plantain", "Off-paddock structure", "Deintensification")) +  
  #labs(fill = "Strategy") +
  labs(title = "Hauraki Farm 9 N leaching Marginal Abatement Cost Curve (MACC)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 


#Hauraki Farm 5
# Load ggplot2 package
library(ggplot2)

# Sample Data for N Leaching MACC
macc_data <- data.frame(
  Strategy = c("Nitrogen use efficiency", "Plantain", "Deintensification"),
  Abatement = c(8, 10, 16),   # Abatement potential in kg N leaching reduced
  Cost = c(1, 25, 175)  # Cost per  per kg N leaching reduced in NZD (negative values for cost-saving measures)
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
  scale_x_continuous("Cummulative Abatement Potential (% N leaching reduced)", expand = c(0, 0)) +
  scale_y_continuous("Marginal Abatement Cost (NZ$ per kg N/ha leaching reduced)", expand = c(0, 0)) +
  scale_fill_discrete(breaks = c("Nitrogen use efficiency", "Plantain", "Deintensification")) +  
  #labs(fill = "Strategy") +
  labs(title = "Horizon Farm 5 N leaching Marginal Abatement Cost Curve (MACC)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 


