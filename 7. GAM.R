library(mgcv)
library(ggplot2)
library(dplyr)
library(gam)

# 1. load data & selected variables
df <- read.csv("final_data.csv")
vars <- c("Flood", "State.GDP", "Domestic.Electricity.Consumption",  "Drainage.Channels", "Non.Agricultural.Land", "Urban.Road.Construction")
df <- df[, vars]

# 2. Normalize predictors
normalize <- function(x) (x - min(x)) / (max(x))
df_norm <- df
df_norm[,-1] <- lapply(df[,-1], normalize)
df_clean <- df_norm 

# Create a look-up table with min and max values
min_max_values <- sapply(df[,-1], function(col) c(min = min(col), max = max(col)))
denormalize <- function(norm_value, variable_name) {
  if (!(variable_name %in% colnames(min_max_values))) {
    stop(paste("Variable", variable_name, "not found in min-max table."))
  }
  min_val <- min_max_values["min", variable_name]
  max_val <- min_max_values["max", variable_name]
  return(norm_value * (max_val - min_val) + min_val)
}
# # 3. Most Optimal GAM Model search (across families like Gaussian, Poisson & Negative Binomial)
# # Gaussian Family
# gam_full <- gam::gam(Flood ~ s(State.GDP) + s(Domestic.Electricity.Consumption) + s(Drainage.Channels) +  s(Non.Agricultural.Land) + s(Urban.Road.Construction), data = df_clean, family = gaussian())
# scope <- gam.scope(df_clean[, -1], arg = "default")
# gam_step <- step.Gam(gam_full, scope = scope, direction = "both")
# summary(gam_step)
# 
# # Poisson Family
# gam_full <- gam::gam(Flood ~ s(State.GDP) + s(Domestic.Electricity.Consumption) + s(Drainage.Channels) +  s(Non.Agricultural.Land) + s(Urban.Road.Construction), data = df_clean, family = poisson())
# scope <- gam.scope(df_clean[, -1], arg = "default")
# gam_step <- step.Gam(gam_full, scope = scope, direction = "both")
# summary(gam_step)
# # every variable contributes non-linearly and significantly, even if the linear F-test doesn’t catch it
# # AIC: 380.77 → higher than Gaussian GAM (~288)

# Negative Binomial Family
gam_nb_full <- gam(Flood ~ s(State.GDP) + s(Domestic.Electricity.Consumption) +  s(Drainage.Channels) + s(Non.Agricultural.Land) + s(Urban.Road.Construction), data = df_clean, family = nb())
# gam_nb_reduced1 <- gam(Flood ~ s(State.GDP) + s(Domestic.Electricity.Consumption) +  s(Drainage.Channels), data = df_clean, family = nb())
# gam_nb_reduced2 <- gam(Flood ~ s(State.GDP) + s(Domestic.Electricity.Consumption), data = df_clean, family = nb())
# gam_nb_reduced3 <- gam(Flood ~ s(State.GDP), data = df_clean, family = nb())
# 
# AIC(gam_nb_full, gam_nb_reduced1, gam_nb_reduced2, gam_nb_reduced3)
#                 df       AIC
# gam_nb_full      6  227.3313
# gam_nb_reduced1  4 1278.2119
# gam_nb_reduced2  3  263.2753
# gam_nb_reduced3  2  253.9758
# AIC-wise full model is the best 

# 4. AIC-based comparison across family-best models
# Gaussian Family's best model aic = 288.1
# Poisson Family's best model aic = 380.77
# NB Family's best model aic = 227.33

# 5. Understanding the overall best model
summary(gam_nb_full)
# dispersion parameter = 1.57 - this confirms over-dispersion (since value is >1) justifying the choice of Negative Binomial over Poisson
# ANOVA for parametric effects - only GDP shows a clear, statistically significant association with flood counts
# ANOVA for non-parametric effects - even though GAM allows non-linear terms, none of the smooth terms are significantly non-linear
# thus, a GLM with linear terms (and interactions) may work as well - especially for interpretability

# conclusion
# State GDP is the strongest indicator of flood frequency — consistent with the hypothesis that economic growth correlates with rapid urban expansion and environmental degradation.
# Electricity, roads, and land-use variables, while conceptually tied to urbanization, show weak standalone effects statistically under this GAM structure.
# The nonparametric smoothing didn't help much → possibly due to the small dataset limiting detection of curvature.

# 6. Diagnostics 
# par(mfrow = c(2,2))
# gam.check(gam_nb_full)
# par(mfrow = c(1,1))

# 7. visualize smooth terms
par(mfrow = c(2,3))
plot(gam_nb_full, shade = TRUE, seWithMean = TRUE)
par(mfrow = c(1,1))

# 8. Predicted vs actual
df_norm$GAM_Predicted <- predict(gam_nb_full, type = "response")  # Use for plots

library(Metrics)
rmse(df_norm$Flood, df_norm$GAM_Predicted)

ggplot(df_norm, aes(x = Flood, y = GAM_Predicted)) + 
  geom_point(size = 3, color = "darkorange") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "GAM: Predicted vs Actual Flood Counts", x = "Actual", y = "Predicted") + theme_minimal()

# 9. Line chart of observed vs predicted
df_line <- data.frame(Year = 2000:2024, Actual = df_clean$Flood, Predicted = df_norm$GAM_Predicted)

ggplot(df_line, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Flood Count"), size = 1.2) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "darkgreen", "Predicted" = "orange")) +
  labs(title = "Flood Count Over Time: Actual vs Predicted", color = "Legend") + theme_minimal()

### 10. XAI 
### 10 (a) SHAP Summary plot
library(iml)
library(tidyr)
library(gridExtra)
library(cowplot)

X <- df_norm[, -c(1, ncol(df_norm))]
y <- df_norm$Flood
predict_fun <- function(model, newdata) { predict(model, newdata = newdata, type = "response") }
predictor <- Predictor$new(model = gam_nb_full, data = X, y = y, predict.fun = predict_fun)
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

### 10 (b) SHAP Summary plot
ggplot(long_df, aes(x = phi, y = reorder(feature, abs(phi), FUN = sum), color = feature.value.y)) +
  geom_point(alpha = 0.7, size = 2) + scale_color_gradient(low = "gold", high = "purple") +
  labs(title = "SHAP Summary Plot for GAM Model", x = "SHAP value (Impact on Prediction)", y = "Feature", color = "Feature value") + theme_minimal()

### 10 (c) SHAP over time
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
plot_actual <- ggplot(df_line, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
  labs(title = "Flood Count Over Time", y = "", color = "Legend") +
  theme_minimal()

combined_plot <- plot_grid(plotlist = c(list(plot_actual), shap_plots), ncol = 1, align = "v")
combined_plot

### 10 (d) GAM Waterfall
library(forcats)
library(tibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Metrics)

gam_waterfall <- function(year, data_train = df_clean) {
  valid_years <- 2000:(2000 + nrow(data_train) - 1)
  year_index <- which(valid_years == year)
  if (length(year_index) == 0) stop(paste("❌ Error: Year must be one of:", paste(valid_years, collapse = ", ")))
  
  obs <- data_train[year_index, , drop = FALSE]
  actual <- data_train$Flood[year_index]
  
  # Contributions on link scale
  smooths_link <- predict(gam_nb_full, newdata = obs, type = "terms")
  smooth_contribs <- as.numeric(smooths_link)
  names(smooth_contribs) <- colnames(smooths_link)
  
  linear_pred <- as.numeric(predict(gam_nb_full, newdata = obs, type = "link"))
  pred_response <- exp(linear_pred)
  avg_link_pred <- mean(predict(gam_nb_full, type = "link", newdata = data_train))
  base_response_pred <- exp(avg_link_pred)
  
  term_names_clean <- gsub("s\\(|\\)", "", names(smooth_contribs))
  denorm_inputs <- sapply(term_names_clean, function(var) { if (var %in% names(df)) round(denormalize(obs[[var]], var), 3) else NA })
  
  df_contrib <- tibble(Feature = term_names_clean, Link_Contribution = smooth_contribs, Value = denorm_inputs) %>% arrange(desc(abs(Link_Contribution)))
  
  df_contrib <- df_contrib %>% mutate(Start_Link = avg_link_pred + lag(cumsum(Link_Contribution), default = 0), End_Link   = avg_link_pred + cumsum(Link_Contribution) )
  
  base_row <- tibble(Feature = "Base Value (Avg Link Pred)", Link_Contribution = 0, Value = NA, Start_Link = avg_link_pred, End_Link = avg_link_pred)
  
  # Bar for predicted value
  prediction_bar <- tibble(Feature = "Prediction", Link_Contribution = 0, Value = NA, Start_Link = linear_pred, End_Link = linear_pred)
  
  df_plot <- bind_rows(base_row, df_contrib, prediction_bar) %>% mutate(Start_Response = exp(Start_Link), End_Response = exp(End_Link), Response_Change = End_Response - Start_Response, Type = case_when( Feature == "Base Value (Avg Link Pred)" ~ "Base", Feature == "Prediction" ~ "Final", Response_Change >= 0 ~ "Increase", TRUE ~ "Decrease" ) )
  
  df_plot <- df_plot %>% mutate(Feature_Factor = fct_inorder(Feature), y_pos = as.numeric(Feature_Factor), ymin = y_pos - 0.3, ymax = y_pos + 0.3, Feature_Label = case_when( Feature == "Base Value (Avg Link Pred)" ~ paste0("Base = ", round(base_response_pred, 2)), Feature == "Prediction" ~ paste0(""), TRUE ~ paste0("Value = ", round(Value), " (Δ ", ifelse(Response_Change >= 0, "+", ""), round(Response_Change), ")") ) )
  
  ggplot(df_plot, aes(y = Feature_Factor)) +
    geom_rect(data = df_plot %>% filter(Type != "Final"), aes(xmin = Start_Response, xmax = End_Response, ymin = ymin, ymax = ymax, fill = Type), color = "black") +
    geom_segment(data = df_plot %>% filter(Type != "Final") %>% mutate(yend_next = lead(ymin)) %>% filter(!is.na(yend_next)), aes(x = End_Response, xend = End_Response, y = ymax, yend = yend_next), color = "gray50", linetype = "dashed" ) +
    geom_text(aes(x = End_Response, label = Feature_Label), size = 4, hjust = 0, nudge_x = 0.01 * (max(df_plot$End_Response) - min(df_plot$Start_Response)) ) +
    geom_rect(data = df_plot %>% filter(Feature == "Prediction"), aes(xmin = Start_Response - 10, xmax = End_Response + 10, ymin = ymin, ymax = ymax), fill = "black", alpha = 0.3 ) +
    geom_text(data = df_plot %>% filter(Feature == "Prediction"), aes(x = End_Response, label = Feature_Label), color = "black", fontface = "bold", size = 4, hjust = 0 ) +
    geom_vline(xintercept = pred_response, linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("Increase" = "skyblue", "Decrease" = "salmon", "Base" = "gray")) +
    labs(title = paste("GAM Waterfall Plot – Year", year), subtitle = paste("Prediction =", round(pred_response), "| Actual =", actual, "| RMSE =", round(rmse(actual, pred_response))), x = NULL, y = NULL ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", axis.text.y = element_text(hjust = 1), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank() ) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15)))
}

gam_waterfall(2020)
gam_waterfall(2024)
