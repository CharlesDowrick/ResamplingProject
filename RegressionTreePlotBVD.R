library(ggplot2)
library(dplyr)
library(tidyr)

# Pre-computed regression tree results
tree_results <- data.frame(
  n_min = c(1, 2, 3, 4, 5, 6, 8, 10, 15, 20, 30, 50, 100),
  Bias2_Noise = c(763798623, 732735618, 744377712, 768162614, 768401736, 
                  787364965, 818269230, 836061564, 931896598, 994346762, 
                  1115829325, 1357581904, 1808456361),
  Variance = c(1046895700, 1032159670, 922631375, 821295492, 798962115, 
               740110560, 679477793, 628075340, 598538947, 594837333, 
               548437901, 522472058, 579070433),
  MSE = c(1810694323, 1764895288, 1667009087, 1589458107, 1567363850, 
          1527475525, 1497747023, 1464136904, 1530435545, 1589184094, 
          1664267226, 1880053962, 2387526794)
)

# Reshape data for plotting
plot_data <- tree_results %>%
  select(n_min, MSE, Bias2_Noise, Variance) %>%
  pivot_longer(cols = c(MSE, Bias2_Noise, Variance),
               names_to = "Component",
               values_to = "Error")

# Set legend order
plot_data$Component <- factor(plot_data$Component, 
                              levels = c("MSE", "Bias2_Noise", "Variance"))

# Create plot
p <- ggplot(plot_data, aes(x = n_min, y = Error, color = Component, group = Component)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(trans = "log10", breaks = c(1, 2, 3, 4, 5, 6, 8, 10, 15, 20, 30, 50, 100)) +
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
    title = "Regression Tree: Bias-Variance Decomposition",
    subtitle = expression(paste("Ames Housing")),
    x = expression(n[min]),
    y = "Squared Error"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold")
  )

# Display plot
print(p)