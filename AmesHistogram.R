library(ggplot2)
library(dplyr)
library(modeldata)

# Load data
data(ames, package = "modeldata")

# All summary statistics
stats <- ames %>%
  summarise(
    Count = n(),
    Mean = mean(Sale_Price),
    Median = median(Sale_Price),
    SD = sd(Sale_Price),
    Min = min(Sale_Price),
    Q1 = quantile(Sale_Price, 0.25),
    Q3 = quantile(Sale_Price, 0.75),
    Max = max(Sale_Price)
  ) %>%
  mutate(across(where(is.numeric), round, 0))

print(stats)

# Create histogram with mean and median lines
ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = stats$Mean, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = stats$Median, color = "darkgreen", linetype = "dotted", size = 1) +
  labs(title = "Distribution of Sale Prices", x = "Sale Price (USD)", y = "Frequency") +
  scale_x_continuous(
    labels = scales::dollar_format(), 
    breaks = seq(0, 700000, by = 100000)  
  ) +
  theme_minimal()