library(tidyverse)
library(modeldata)

set.seed(42)

# Load data
data(ames, package = "modeldata")

# Fit linear model using all predictors
predictor_cols <- setdiff(names(ames), "Sale_Price")
formula_lm <- as.formula(paste("Sale_Price ~", paste(predictor_cols, collapse = " + ")))
lm_model <- lm(formula_lm, data = ames)

# Get residuals and fitted values
residuals_lm <- residuals(lm_model)
fitted_lm <- fitted(lm_model)

# Residuals vs Fitted plot
ggplot(data.frame(Fitted = fitted_lm, Residuals = residuals_lm), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.3, size = 1, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", size = 1) +
  labs(title = "Linear Regression: Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Q-Q plot of residuals
ggplot(data.frame(Sample = residuals_lm), aes(sample = Sample)) +
  stat_qq(color = "steelblue", alpha = 0.3, size = 1) +
  stat_qq_line(color = "steelblue", size = 1) +
  labs(title = "Linear Regression: Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
