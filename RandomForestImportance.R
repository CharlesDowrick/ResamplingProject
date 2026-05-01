library(randomForest)
library(tidyverse)
library(modeldata)

set.seed(42)

# Load Ames housing data
data(ames, package = "modeldata")

# Identify predictors
predictor_cols <- setdiff(names(ames), "Sale_Price")

# Random forest model (mtry = 31)
rf_model <- randomForest(Sale_Price ~ ., 
                         data = ames[, c(predictor_cols, "Sale_Price")],
                         ntree = 100,
                         mtry = 31,
                         importance = TRUE)

# Extract permutation importance (type = 1)
rf_importance <- importance(rf_model, type = 1)
colnames(rf_importance) <- "Importance"

# Convert to data frame and sort
rf_df <- data.frame(
  Feature = rownames(rf_importance),
  Importance = rf_importance[, 1]
) %>% arrange(desc(Importance))

# Remove features with zero or negative importance
rf_df <- rf_df %>% filter(Importance > 0)

# Convert to percentage of total
total_imp_rf <- sum(rf_df$Importance)
rf_df <- rf_df %>%
  mutate(Percentage_of_Total = (Importance / total_imp_rf) * 100)

# Print all features with importance (table)
print(rf_df %>% mutate(across(where(is.numeric), ~round(., 2))))

# Take top 15 for plotting
rf_top15 <- rf_df %>% head(15)

# Create bar plot
p <- ggplot(rf_top15, aes(x = reorder(Feature, Percentage_of_Total), y = Percentage_of_Total)) +
  geom_bar(stat = "identity", fill = "#2ca02c", alpha = 0.8) +
  coord_flip() +
  labs(title = "Random Forest: Permutation Importance", 
       x = "Feature", 
       y = "Percentage of Total Importance (%)") +
  theme_minimal()

# Display plot
print(p)
