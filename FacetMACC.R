library(ggplot2)
library(readr) 
mt<-read.csv("C:/Users/chikazhet/MACChauraki.csv")
df <- df[order(df$cost), ]  # Sorting in ascending order
ggplot(df, aes(x = cumsum(abatement), y = cost, fill = factor(mitigation))) +
  geom_bar(stat = "identity", width = 0.9) +
  labs(x = "Cumulative Abatement (kg N/ha)", 
       y = "Cost ($/kg N reduced)", 
       title = "Marginal Abatement Cost Curve (MACC) by Farm") +
  theme_minimal() +
  facet_wrap(~ farmid, scales = "free_x") +  # Facet by farm
  theme(legend.position = "bottom")  # Remove legend if not needed


# Load ggplot2 package
library(ggplot2)

# Sample Data for N Leaching MACC
macc_data <- data.frame(
  Strategy = c("Nitogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification", "Off-paddock structures"),
  Abatement = c(8, 2, 7, 23, 10),   # Abatement potential in kg N leaching reduced
  Cost = c(-1.3, 48, 14, 31, 94)  # Cost per  per kg N leaching reduced in NZD (negative values for cost-saving measures)
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
  scale_fill_discrete(breaks = c("Nitogen use efficiency",
                                 "Nutrient recycling","Plantain",
                                 "Deintensification",
                                 "Off-paddock structures" )) +  
  #labs(fill = "Strategy") +
  labs(title = "LUDF N leaching Marginal Abatement Cost Curve (MACC)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 




# Sample Data for N Leaching MACC farm 7
macc_data <- data.frame(
  Strategy = c("Nitrogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification", "Off-paddock structures"),
  Abatement = c(4, 0, 9, 24, 9),   # Abatement potential in kg N leaching reduced
  Cost = c(-1, 0, 20, 97, 61)  # Cost per  per kg N leaching reduced in NZD (negative values for cost-saving measures)
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
  scale_fill_discrete(breaks = c("Nitogen use efficiency",
                                 "Nutrient recycling","Plantain",
                                 "Deintensification",
                                 "Off-paddock structures" )) +  
  #labs(fill = "Strategy") +
  labs(title = "LUDF N leaching Marginal Abatement Cost Curve (MACC)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) 


mt$mitigation <- factor(mt$mitigation, levels= c("Nitrogen use efficiency", "nutrient recycling", "Plantain", "Deintensification", "Off-paddock structure"))
## plotting profit and N leaching
ggplot(data =mt, mapping = aes(y = cost, x = abatement, group= mitigation)) +
  scale_x_continuous(position="bottom") +
  scale_y_continuous(position="right")+
  geom_point(aes(color= mitigation),size=3)+ scale_colour_manual(values = c( "green","cyan","blue","red", "gray" ))+
  scale_fill_discrete(limits = c("Nitogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification", "Off-paddock structures")) +
  labs(y="Marginal abatement cost($/kg N reduced)", x="Abatement pontential(%)")  +
  theme(legend.position = "left") 

