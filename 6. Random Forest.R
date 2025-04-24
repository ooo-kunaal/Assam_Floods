library(randomForest)
library(ggplot2)
library(dplyr)
library(fastshap)      # fast, parallel SHAP for tree models
library(iml)           # model-agnostic interpretation (flashlight builds on it)
library(flashlight)    # waterfall & dashboards
library(data.table)    # tidy time panels
library(patchwork)

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

# 6. Observed vs Predicted Over Time
df_line_rf <- data.frame(Year = 2000:2024, Actual = df_norm$Flood, Predicted_RF = df_norm$RF_Predicted)

ggplot(df_line_rf, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +
  geom_line(aes(y = Predicted_RF, color = "Predicted_RF"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "darkgreen", "Predicted_RF" = "orange")) +
  labs(title = "Random Forest: Actual vs Predicted Flood Over Time", color = "Legend") +
  theme_minimal()

### 7 (a) SHAP Summary plot
library(iml)
library(tidyr)
library(gridExtra)
library(cowplot)

X <- df_norm[, -c(1, ncol(df_norm)-1, ncol(df_norm))]
y <- df_norm$Flood
predict_fun <- function(model, newdata) { predict(model, newdata = newdata) }
predictor <- Predictor$new(model = rf_model, data = X, y = y, predict.fun = predict_fun)
set.seed(2025)
sample_ids <- 1:nrow(X)
shap_results <- lapply(sample_ids, function(i) {
  shap <- Shapley$new(predictor, x.interest = X[i, , drop = FALSE])
  data.frame(shap$results, row = i)
})
shap_df <- bind_rows(shap_results)
X_long <- X %>% mutate(row = row_number()) %>%
  pivot_longer(-row, names_to = "feature", values_to = "feature.value")
long_df <- left_join(shap_df, X_long, by = c("feature", "row"))
long_df$Year <- 2000 + long_df$row - 1

### 7 (b) SHAP Summary plot
ggplot(long_df, aes(x = phi, y = reorder(feature, abs(phi), FUN = sum), color = feature.value.y)) +
  geom_point(alpha = 0.7, size = 2) + scale_color_gradient(low = "gold", high = "purple") +
  labs(title = "SHAP Summary Plot for GAM Model", x = "SHAP value (Impact on Prediction)", y = "Feature", color = "Feature value") + theme_minimal()

### 7 (c) SHAP over time
top_features <- long_df %>% group_by(feature) %>% summarise(mean_abs_shap = mean(abs(phi))) %>% arrange(desc(mean_abs_shap)) %>% slice(1:5) %>% pull(feature)
shap_plots <- list()
for (i in seq_along(top_features)) {
  feature_name <- top_features[i]
  
  df_plot <- long_df %>%
    filter(feature == feature_name)
  
  # Normalize SHAP values for coloring (0–1)
  df_plot <- df_plot %>%
    mutate(NormSHAP = (phi - min(phi)) / (max(phi) - min(phi)))
  
  # Plot: Feature Value vs Time, shaded by normalized SHAP
  shap_plots[[i]] <- ggplot(df_plot, aes(x = Year, y = feature.value.y)) +
    geom_tile(aes(fill = NormSHAP), height = Inf) +
    geom_line(color = "black") +
    geom_point(color = "black") +
    scale_fill_gradientn(colors = c("blue", "orange", "red"), limits = c(0, 1), name = "") +
    labs(title = feature_name, y = '') +
    theme_minimal()
}

plot_actual <- ggplot(df_line_rf, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted_RF, color = "Predicted_RF")) +
  scale_color_manual(values = c("Actual" = "black", "Predicted_RF" = "red")) +
  labs(title = "Flood Count Over Time", y='', color = "Legend") +
  theme_minimal()

combined_plot <- plot_grid(plotlist = c(list(plot_actual), shap_plots), ncol = 1, align = "v")
combined_plot

### 7 (d) SHAP Waterfall
library(flashlight)
df_clean = df_norm[,-c(ncol(df_norm),ncol(df_norm)-1 )]
fl <- flashlight(model = rf_model, data = df_clean, y = "Flood", label = "RandomForest")

rf_waterfall <- function(year){
  valid_years <- 2000:(2000 + nrow(df_norm) - 1)
  year_index <- which(valid_years == year)
  if (length(year_index) == 0) stop(paste("❌ Error: Year must be one of:", paste(valid_years, collapse = ", ")))
  actual = df_norm$Flood[year_index]
  pred = df_norm$RF_Predicted[year_index]
  
  wl <- light_breakdown(fl, new_obs = df_clean[year_index, ], type = "shap", n_max = 5)
  
  plot(wl) + ggtitle(label = paste("SHAP Waterfall Plot – Year", year), 
                          subtitle = paste("Prediction =", round(pred), "| Actual =", actual, "| RMSE =", round(rmse(actual, pred))))
  
}
rf_waterfall(2020)
rf_waterfall(2024)

### 7 (e) Rule Extraction
library(inTrees)
library(reprtree) 
reprtree::plot.getTree(rf_model, k = 1, depth = 4)

tree_list   <- RF2List(rf_model)
raw_rules   <- extractRules(tree_list, df_clean[ , -1])
rule_metric <- getRuleMetric(raw_rules, df_clean[ , -1], df_clean$Flood)
top_rules   <- selectRuleRRF(rule_metric, df_clean[ , -1], df_clean$Flood)

format_rules <- function(rules, variable_names, digits = 3, min_freq = 0.5) {
  
  formatted_rules <- apply(rules, 1, function(rule) {
    # Extract and format conditions
    conditions <- strsplit(rule["condition"], " & ")[[1]]
    formatted_conditions <- sapply(conditions, function(cond) {
      parts <- strsplit(cond, "X\\[,")[[1]][2]
      index_value <- strsplit(parts, "\\]")[[1]]
      variable_index <- as.numeric(index_value[1])
      operator_value <- index_value[2]
      value <- as.numeric(gsub("[^0-9.]", "", operator_value))
      operator <- gsub("[0-9.]", "", operator_value)
      rounded_value <- round(value, digits)
      paste(variable_names[variable_index], operator, rounded_value)
    })
    formatted_rule <- paste(formatted_conditions, collapse = " AND ")
    
    # Extract prediction and frequency
    pred_value <- round(as.numeric(rule["pred"]), digits)
    freq_value <- as.numeric(rule["freq"])
    
    # Format the rule with prediction and frequency
    paste("Rule:", formatted_rule, "-> Prediction:", pred_value, "(Frequency:", freq_value, ")")
  })
  
  # Filter rules by minimum frequency
  filtered_rules <- formatted_rules[as.numeric(rules[, "freq"]) >= min_freq]
  
  return(filtered_rules)
}
# Format the rules with rounding and filtering by frequency
variable_names <- colnames(df_clean)[-1]
formatted_rules <- format_rules(top_rules, variable_names, digits = 3, min_freq = 0.2)
formatted_rules

