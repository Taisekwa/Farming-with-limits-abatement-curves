library(ggplot2)


# Black and white grasslands publication, High N surplus farms.

# Load required libraries
library(ggplot2)
library(ggpattern)  # For patterns

# High N surplus farms data
macc_data <- data.frame(
  Strategy = c("Nitrogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification", "Off-paddock structure"),
  Abatement = c(4, 12, 9, 17, 12),   
  Cost = c(-101, 10, 58, 88, 190)  
)

# Order data by cost for proper visualization
macc_data <- macc_data[order(macc_data$Cost), ]

# Define xmin and xmax for each bar
macc_data$xmin <- c(0, head(cumsum(macc_data$Abatement), -1))
macc_data$xmax <- cumsum(macc_data$Abatement)

# Automatically assign a pattern to each strategy
patterns <- c("stripe", "crosshatch", "circle", "wave", "plasma")

# Ensure that each strategy gets a unique pattern
macc_data$Pattern <- rep(patterns, length.out = nrow(macc_data))

# Plot with automatic pattern assignment and working legend
ggplot(macc_data, aes(y = Cost, pattern = Pattern, fill = Strategy)) +
  geom_rect_pattern(aes(
    xmin = xmin, xmax = xmax, ymin = 0, ymax = Cost,
    pattern = Pattern
  ), 
  color = "black", size = 0.3, fill = "white",   # Black-and-white
  pattern_density = 0.4, pattern_spacing = 0.02) +
  
  scale_x_continuous("Cumulative Abatement Potential (% N leaching reduced)", expand = c(0, 0)) +
  scale_y_continuous("Marginal Abatement Cost (NZ$ per kg N leaching reduced)", expand = c(0, 0)) +
  
  scale_pattern_manual(
    values = patterns,   # Automatically assign patterns to strategies
    name = "Strategy",   # Title for the pattern legend
    breaks = patterns,   # Breaks for patterns (so they appear in the legend)
    labels = macc_data$Strategy  # Proper labels in the legend
  ) +
  
  #labs(title = "Aggregated high N surplus farms (MACC)") +
  
  theme_minimal() +
  theme(
    legend.position = "right",  # Ensures the legend is shown
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )


# Black and white grasslands publication, Low N surplus farms.

# Load required libraries
library(ggplot2)
library(ggpattern)  # For patterns

# Low N surplus farms data
macc_data <- data.frame(
  Strategy = c("Nitrogen use efficiency", "Nutrient recycling", "Plantain", "Deintensification","Off-paddock structure"),
  Abatement = c(7, 10, 9, 10, 18),   
  Cost = c(-11, 16, 32, 200, 190)  
)

# Order data by cost for proper visualization
macc_data <- macc_data[order(macc_data$Cost), ]

# Define xmin and xmax for each bar
macc_data$xmin <- c(0, head(cumsum(macc_data$Abatement), -1))
macc_data$xmax <- cumsum(macc_data$Abatement)

# Automatically assign a pattern to each strategy
patterns <- c("stripe", "crosshatch", "circle", "wave", "plasma")

# Ensure that each strategy gets a unique pattern
macc_data$Pattern <- rep(patterns, length.out = nrow(macc_data))

# Plot with automatic pattern assignment and working legend
ggplot(macc_data, aes(y = Cost, pattern = Pattern, fill = Strategy)) +
  geom_rect_pattern(aes(
    xmin = xmin, xmax = xmax, ymin = 0, ymax = Cost,
    pattern = Pattern
  ), 
  color = "black", size = 0.3, fill = "white",   # Black-and-white
  pattern_density = 0.4, pattern_spacing = 0.02) +
  
  scale_x_continuous("Cumulative Abatement Potential (% N leaching reduced)", expand = c(0, 0)) +
  scale_y_continuous("Marginal Abatement Cost (NZ$ per kg N leaching reduced)", expand = c(0, 0)) +
  
  scale_pattern_manual(
    values = patterns,   # Automatically assign patterns to strategies
    name = "Strategy",   # Title for the pattern legend
    breaks = patterns,   # Breaks for patterns (so they appear in the legend)
    labels = macc_data$Strategy  # Proper labels in the legend
  ) +
  
  #labs(title = "Aggregated high N surplus farms (MACC)") +
  
  theme_minimal() +
  theme(
    legend.position = "right",  # Ensures the legend is shown
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )