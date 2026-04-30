library(randomForest)
library(dplyr)
library(ggplot2)
library(modeldata)

set.seed(42)

# Load Ames housing data
data(ames, package = "modeldata")

# Identify predictors
predictor_cols <- setdiff(names(ames), "Sale_Price")

# Bagging model (mtry = all features = 73)
bagging_model <- randomForest(Sale_Price ~ ., 
                              data = ames[, c(predictor_cols, "Sale_Price")],
                              ntree = 100,
                              mtry = 73,
                              importance = TRUE)

# Extract permutation importance (type = 1)
bagging_importance <- importance(bagging_model, type = 1)
colnames(bagging_importance) <- "Importance"

# Convert to data frame and sort
bagging_df <- data.frame(
  Feature = rownames(bagging_importance),
  Importance = bagging_importance[, 1]
) %>% arrange(desc(Importance))

# Remove features with zero or negative importance
bagging_df <- bagging_df %>% filter(Importance > 0)

# Convert to percentage of total
total_imp_bag <- sum(bagging_df$Importance)
bagging_df <- bagging_df %>%
  mutate(Percentage_of_Total = (Importance / total_imp_bag) * 100)

# Print all features with importance (table)
print(bagging_df %>% mutate(across(where(is.numeric), ~round(., 2))))

# Take top 15 for plotting
bagging_top15 <- bagging_df %>% head(15)

# Create bar plot
p_bagging <- ggplot(bagging_top15, aes(x = reorder(Feature, Percentage_of_Total), y = Percentage_of_Total)) +
  geom_bar(stat = "identity", fill = "#1f77b4", alpha = 0.8) +
  coord_flip() +
  labs(title = "Bagging: Permutation Importance", 
       x = "Feature", 
       y = "Percentage of Total Importance (%)") +
  theme_minimal()

# Display plot
print(p_bagging)