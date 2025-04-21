library(randomForest)
library(ggplot2)
library(dplyr)

df <- read.csv("final_data.csv")
vars <- c("Flood", "State.GDP", "Domestic.Electricity.Consumption", "Drainage.Channels", "Non.Agricultural.Land", "Urban.Road.Construction")
df <- df[, vars]

# Normalize all except Flood
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
df_norm <- df
df_norm[,-1] <- lapply(df[,-1], normalize)

set.seed(2025)

# Fit Random Forest
rf_model <- randomForest(Flood ~ ., data = df_norm[, 1:6], importance = TRUE, ntree = 500)
rf_model  # % variance explained = 85.2

# Predictions + Residuals
df_norm$RF_Predicted <- predict(rf_model)
df_norm$RF_Residuals <- df_norm$Flood - df_norm$RF_Predicted
library(Metrics)
rmse(df_norm$Flood, df_norm$RF_Predicted) # RMSE = 71.5

# Variable Importance Plot
varImpPlot(rf_model, main = "Random Forest Variable Importance")

# Predicted vs Actual (Scatter)
ggplot(df_norm, aes(x = Flood, y = RF_Predicted)) +
  geom_point(color = "orange", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
  labs(title = "Random Forest: Predicted vs Actual Flood",
       x = "Actual Flood Count", y = "Predicted Flood Count") +
  theme_minimal()

# 5. Residuals Plot
ggplot(df_norm, aes(x = RF_Predicted, y = RF_Residuals)) +
  geom_point(color = "purple", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Random Forest: Residuals vs Predicted", x = "Predicted Flood Count", y = "Residuals") +
  theme_minimal()

# Observed vs Predicted Over Time
df_line_rf <- data.frame(Year = 2000:2024, Actual = df_norm$Flood, Predicted_RF = df_norm$RF_Predicted)

ggplot(df_line_rf, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +
  geom_line(aes(y = Predicted_RF, color = "Predicted_RF"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "darkgreen", "Predicted_RF" = "orange")) +
  labs(title = "Random Forest: Actual vs Predicted Flood Over Time", color = "Legend") +
  theme_minimal()
