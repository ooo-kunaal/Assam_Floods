# Load required libraries
library(readr)
library(dplyr)
library(caret)
library(glmnet)

### Step 1: Load & Prepare Data
df <- read.csv("final_data.csv")
predictors <- df %>% select(-Year, -Flood)
target <- df$Flood

### Step 2: Normalize the Predictors (0–1)
pre_proc <- preProcess(predictors, method = c("range"))
predictors_norm <- predict(pre_proc, predictors)
df_norm <- data.frame(Flood = target, predictors_norm)

### Lasso Regression for Variable Selection
x <- as.matrix(df_norm %>% select(-Flood)) # Convert to matrix for glmnet
y <- df_norm$Flood
set.seed(2025)
cv_lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 5)
plot(cv_lasso) # Plot CV error curve

lambda_best <- cv_lasso$lambda.min # Best lambda
lambda_best # lambda = 2.05 (best penalty strength selected via cross-validation that minimizes MSE)

lasso_model <- glmnet(x, y, alpha = 1, lambda = lambda_best) # Fit final Lasso model with best lambda

selected_vars <- coef(lasso_model) # Extract non-zero coefficients
selected_vars <- selected_vars[selected_vars[,1] != 0, , drop = FALSE]
selected_vars
# selected variables - total food grains, electricity, urban road, registered companies, drainage channels

### Fit Random Forest and Plot Variable Importance
library(randomForest)
library(caret)
set.seed(2025)

rf_model <- randomForest(Flood ~ ., data = df_norm, importance = TRUE, ntree = 500)
rf_model

importance_vals <- randomForest::importance(rf_model) # Variable importance table
print(importance_vals)

varImpPlot(rf_model, main = "Random Forest Variable Importance") # Plot variable importance
# selected variables (top 5) - GDP, electricity, drainage, non-agricultural land, urban road (instead of food grains)
