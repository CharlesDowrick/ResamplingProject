library(ggplot2)
library(dplyr)
library(tidyr)

# Data from your bagging table
bagging_results <- data.frame(
  n_trees = c(2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 75, 100, 250, 500),
  Bias2_Noise = c(805794765, 763020689, 756373243, 752435371, 759921936, 
                  736082275, 746951631, 739519427, 749650763, 
                  741108542, 741291729, 740049554, 748280505, 731938643, 741563629),
  Variance = c(637369121, 364741959, 243897140, 199455429, 183442470, 
               138183732, 117930415, 113488322, 103795850, 
               88999055, 87194755, 75922388, 77653702, 66749169, 63677935),
  MSE = c(1443163886, 1127762648, 1000270382, 951890800, 943364406, 
          874266007, 864882046, 853007749, 853446613, 
          830107597, 828486484, 815971941, 825934207, 798687812, 805241564)
)

# Reshape data for plotting
plot_data <- bagging_results %>%
  select(n_trees, MSE, Bias2_Noise, Variance) %>%
  pivot_longer(cols = c(MSE, Bias2_Noise, Variance),
               names_to = "Component",
               values_to = "Error")

# Order the components for consistent legend
plot_data$Component <- factor(plot_data$Component, 
                              levels = c("MSE", "Bias2_Noise", "Variance"))

# Create the plot
p <- ggplot(plot_data, aes(x = n_trees, y = Error, color = Component, group = Component)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(trans = "log10", 
                     breaks = c(2, 4, 6, 8, 10, 15, 20, 25, 30, 40, 50, 75, 100, 250, 500)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(
    name = "Component",
    values = c("MSE" = "#1f77b4", "Bias2_Noise" = "#2ca02c", "Variance" = "#d62728"),
    labels = c(
      "MSE" = "MSE",
      "Bias2_Noise" = expression(Bias^2 + Noise),
      "Variance" = "Variance"
    )
  ) +
  labs(
    title = "Bagging: Bias-Variance Decomposition",
    subtitle = "Ames Housing",
    x = expression(n[trees]),
    y = "Squared Error"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold")
  )

# Display the plot
print(p)