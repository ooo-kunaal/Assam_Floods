# Load required libraries
library(MuMIn)
library(dplyr)
library(ggplot2)
library(MASS)
library(Metrics)

# Load and filter data
df <- read.csv("final_data.csv")
vars <- c("Flood", "State.GDP", "Domestic.Electricity.Consumption", "Drainage.Channels", "Non.Agricultural.Land", "Urban.Road.Construction")
df <- df[, vars]

# Normalize all except Flood
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
df_norm <- df
df_norm[,-1] <- lapply(df[,-1], normalize)

## Fit full GLM with interactions
# full_model <- glm(Flood ~ (.)^2, data = df_norm,family = gaussian(link = "identity"))
# full_model <- glm(Flood ~ (.)^2, data = df_norm,family = poisson(link = 'log'))
# full_model <- glm(Flood ~ (.)^2, data = df_norm,family = quasipoisson())

# Dredge all model combinations (including interactions)
# options(na.action = "na.fail")  # Required for dredge
# model_set <- dredge(full_model, rank = "AIC", trace = TRUE)
# best_glm <- get.models(model_set, 1)[[1]]
# summary(best_glm)

## Negative Binomial GLM
library(MASS)
library(MuMIn)
formula_nb <- as.formula("Flood ~ (Domestic.Electricity.Consumption + Drainage.Channels +  Non.Agricultural.Land + State.GDP + Urban.Road.Construction)^2")
nb_model <- glm.nb(formula = formula_nb, data = df_norm)
options(na.action = "na.fail")
nb_dredge <- dredge(nb_model, rank = "AIC")
head(nb_dredge)
best_nb <- get.models(nb_dredge, 1)[[1]]
summary(best_nb)

df_norm$Predicted <- predict(best_nb, type = "response")
df_norm$Residuals <- df_norm$Flood - df_norm$Predicted

res_dev <- deviance(best_nb)
aic_val <- AIC(best_nb)
cat("Residual Deviance:", res_dev, "\n")
cat("AIC:", aic_val, "\n")

library(Metrics)
rmse(df_norm$Flood, df_norm$Predicted) # RMSE = 67.67

# Predicted vs Actual
ggplot(df_norm, aes(x = Flood, y = Predicted)) +
  geom_point(size = 3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed") +
  labs(title = "Predicted vs Actual Flood Count", x = "Actual Flood Count", y = "Predicted Flood Count") + theme_minimal()

# Residual Analysis
par(mfrow=c(2,2))
plot(best_nb)
par(mfrow=c(1,1))

# Line chart of observed vs predicted
df_line <- data.frame(Year = 2000:2024, Actual = df_norm$Flood, Predicted = df_norm$Predicted)
ggplot(df_line, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Flood Count"), size = 1.2) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "darkgreen", "Predicted" = "orange")) +
  labs(title = "Flood Count Over Time: Actual vs Predicted", color = "Legend") +
  theme_minimal()
