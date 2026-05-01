library(tidyverse)

# Pre-computed random forest results
rf_results <- data.frame(
  m_try = c(4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73),
  Bias2_Noise = c(925572493, 841946102, 806565660, 798408811, 773406776, 761938277, 756403174, 747846505, 756884408, 742715192, 743249268, 742995812, 743708547, 745649402, 737546495, 745854206, 738785106, 738733793, 732234114, 739052001, 742772533, 750030945, 750382045, 741126828),
  Variance = c(35396285, 38383793, 40603914, 43076082, 44411793, 45592856, 48805438, 50488895, 49596423, 52345294, 54046764, 53809033, 57100665, 56801658, 59247993, 62170665, 61419149, 65248739, 66265339, 62630307, 64182586, 69940213, 73352557, 76621932),
  MSE = c(960968777, 880329895, 847169575, 841484893, 817818569, 807531133, 805208612, 798335400, 806480831, 795060486, 797296031, 796804844, 800809212, 802451060, 796794488, 808024871, 800204255, 803982532, 798499454, 801682308, 806955119, 819971158, 823734602, 817748760)
)

# Reshape data for plotting
plot_data <- rf_results %>%
  select(m_try, MSE, Bias2_Noise, Variance) %>%
  pivot_longer(cols = c(MSE, Bias2_Noise, Variance),
               names_to = "Component",
               values_to = "Error")

# Set legend order
plot_data$Component <- factor(plot_data$Component, 
                              levels = c("MSE", "Bias2_Noise", "Variance"))

# Create plot
p <- ggplot(plot_data, aes(x = m_try, y = Error, color = Component, group = Component)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
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
    title = "Random Forest: Bias-Variance Decomposition",
    subtitle = "Ames Housing",
    x = expression(m[try]),
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

# MSE curve
p_mse <- ggplot(rf_results, aes(x = m_try, y = MSE)) +
  geom_line(size = 1.2, color = "#1f77b4") +
  geom_point(size = 2.5, color = "#1f77b4") +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  labs(
    title = "Random Forest: MSE",
    subtitle = "Ames Housing",
    x = expression(m[try]),
    y = "MSE"
  ) +
  theme_minimal()

print(p_mse)
