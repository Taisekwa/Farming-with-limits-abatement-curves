# Sample Data
macc_data <- data.frame(
  LUDF = c("P21", "7 in 10", "VRI", "Plantain", "Low N"),
  Abatement = c(500, 1000, 700, 400, 300),   # Abatement potential in tons of CO2 or kg N reduced
  Cost = c(-10, 5, 20, 15, 30)               # Cost per ton or per kg in USD (negative values for cost-saving measures)
)
# Calculate cumulative abatement
macc_data <- macc_data[order(macc_data$Cost), ]  # Order by cost (low to high)
macc_data$CumulativeAbatement <- cumsum(macc_data$Abatement)

# Load ggplot2 package
library(ggplot2)

# Plotting MACC
ggplot(macc_data, aes(x = CumulativeAbatement, y = Cost, fill = LUDF)) +
  geom_bar(stat = "identity", aes(width = Abatement), color = "black", size = 0.3) +
  scale_x_continuous("Cumulative Abatement (tons CO2)", expand = c(0, 0)) +
  scale_y_continuous("Marginal Abatement Cost (USD/ton CO2)", expand = c(0, 0)) +
  labs(title = "Marginal Abatement Cost Curve (MACC)",
       subtitle = "Cost-effectiveness of Different Mitigation Strategies") +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_brewer(palette = "Set3")  # Choose a color palette
