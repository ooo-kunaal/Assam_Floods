# Load necessary libraries
library(forecast)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(utils) # For combn

# 1. Load data and create time series
df <- read.csv("final_data.csv")
df_ts <- ts(df$Flood, start = c(2000), frequency = 1) # annual frequency
xreg <- df %>% select(State.GDP, Domestic.Electricity.Consumption, Drainage.Channels, Non.Agricultural.Land, Urban.Road.Construction)
xreg_mat <- as.matrix(xreg)
regressor_names <- colnames(xreg_mat)

# --- Modified Code: Generate combinations (excluding the empty set) ---
# 1. Generate all possible combinations of regressor names (size 1 to k)
all_combinations <- list()
# Include combinations of size 1 up to the total number of regressors
for (k in 1:length(regressor_names)) {
  combinations_k <- combn(regressor_names, k, simplify = FALSE)
  all_combinations <- c(all_combinations, combinations_k)
}
# --- Note: The line adding the empty set 'list(character(0))' is removed ---

# Check if any combinations were generated (requires at least one potential regressor)
if (length(all_combinations) == 0 && length(regressor_names) > 0) {
  stop("Error: No combinations generated, check regressor names.")
} else if (length(all_combinations) == 0) {
  stop("Error: No regressors defined in xreg_mat.")
}

# 2. Initialize variables to store the best model and its AIC
best_model <- NULL
# Use AIC as per your provided snippet. Consider AICc (forecast::AICc) for smaller samples.
min_aic <- Inf 
best_regressors <- NULL
model_results <- list()

# 3. Loop through each combination, fit model, and compare AIC
#    (Now only loops through combinations with at least one regressor)
cat("Starting ARIMAX model search (excluding pure ARIMA)...\n")

for (i in 1:length(all_combinations)) {
  current_selection <- all_combinations[[i]]
  
  # --- Modified Code: Simplified xreg preparation ---
  # Since length(current_selection) is always > 0 now, no need for the 'if' check
  # Ensure it remains a matrix even if only one column is selected
  current_xreg <- xreg_mat[, current_selection, drop = FALSE] 
  regressor_desc <- paste(current_selection, collapse=", ")
  
  cat(paste0("Fitting model ", i, "/", length(all_combinations), ": Regressors = ", regressor_desc, "\n"))
  
  # Fit auto.arima with the current set of regressors
  # Using tryCatch to handle potential errors during model fitting
  possible_error <- tryCatch({
    model <- auto.arima(
      df_ts, 
      xreg = current_xreg, 
      seasonal = FALSE,      # As per your original code
      stepwise = FALSE,      # For a more exhaustive search (can be slow)
      approximation = FALSE, # For accuracy
      trace = FALSE          # Suppress intermediate output from auto.arima
    )
  }, error = function(e) {
    cat("  ERROR fitting model:", conditionMessage(e), "\n")
    return(e) # Return the error object
  })
  
  # Check if the model fitting was successful
  if (!inherits(possible_error, "error")) {
    model <- possible_error # Assign the fitted model
    # Using AIC as per your snippet. Use forecast::AICc(model) for AICc.
    current_aic <- AIC(model) # Get the AIC value 
    
    cat(paste("  Model:", model$arma[6], model$arma[1], model$arma[7], model$arma[2]), # Display ARIMA(p,d,q)
        "AIC:", round(current_aic, 2), "\n") # Changed label to AIC
    
    # Optional: Store the model and its AIC
    model_results[[paste(current_selection, collapse="_")]] <- list(
      model = model, 
      aic = current_aic,  # Storing AIC
      regressors = current_selection
    )
    
    # Check if this model is better than the current best
    if (current_aic < min_aic) {
      min_aic <- current_aic
      best_model <- model
      best_regressors <- current_selection
      cat("  *** New best model found! ***\n")
    }
  } else {
    cat("  Skipping combination due to error during fitting.\n")
  }
  cat("---\n") # Separator for readability
}

# 4. Report the best model found
cat("\n--- Optimal ARIMAX Search Complete (among models with regressors) ---\n")
if (!is.null(best_model)) {
  cat("Best model identified based on AIC.\n") # Changed label to AIC
  cat("Lowest AIC:", min_aic, "\n")            # Changed label to AIC
  # Since we excluded the pure ARIMA case, best_regressors should not be empty if a model was found
  if (length(best_regressors) > 0) { 
    cat("Optimal Regressors:", paste(best_regressors, collapse=", "), "\n")
  } else {
    # This case should ideally not happen if any model fit successfully
    cat("Optimal Regressors: None found (unexpected result, check fitting errors).\n") 
  }
  cat("\nSummary of the Best Model:\n")
  summary(best_model)
  
  # Example: Plot actual vs fitted for the best model
  best_fitted <- fitted(best_model)
  plot(df_ts, xlab='Years', ylab='Flood Count', main=paste('Actual vs Fitted for Best Model:', format(best_model)), ylim=c(min(df_ts, best_fitted), max(df_ts, best_fitted)))
  points(df_ts, cex=0.8, lwd=2)
  lines(best_fitted, col='darkorange', lwd=2, lty=2)
  points(best_fitted, col='darkorange', cex=0.8, lwd=2)
  legend('bottomright', c('Actual', 'Predicted'), col=c('black', 'darkorange'), lty=c(1,2), lwd=2, bty='n')
  
  # Check residuals
  checkresiduals(best_model)
  
} else {
  cat("No suitable ARIMAX model (with regressors) could be found. Check data and potential errors during fitting.\n")
}


# 8a. Actual vs fitted - scatter for ARIMAX
plot(as.numeric(fitted(best_model)), df$Flood, xlab = 'Fitted values', ylab = 'Actual values',  main = 'Actual vs Fitted for ARIMAX')
abline(0, 1, col = 'navy', lty = 2)
# Points lie mostly close to the 45° line.
# Slight underestimation at high flood counts (e.g., > 500), but still tightly clustered.

# 8b. Actual vs fitted - over time for ARIMAX
plot(df_ts, xlab = 'Years', ylab = 'Flood Count',  main = 'Actual vs Fitted for ARIMAX (over time)', ylim=c(min(df_ts, fitted(best_model)),max(df_ts, fitted(best_model))))
points(df_ts, cex = 0.8, lwd = 2)
lines(fitted(best_model), col = 'darkorange', lwd = 2, lty = 2)
points(fitted(best_model), col = 'darkorange', cex = 0.8, lwd = 2)
legend('bottomright', c('Actual', 'Predicted'), col = c('black', 'darkorange'), lty = c(1, 2), lwd = 2, bty = 'n')
# The predicted values closely track the trend and turning points, especially after 2013.
# Model captures peaks, dips, and directional shifts — better than ETS and ARIMA-only models.

best_model
# Model: ARIMA(1,1,0) with Urban road construction as the only regressor
# xt = yt - yt-1
# xt =  -0.5128*xt-1 + 0.0376*urban_road + et
# where, et ~ N(0, 7864)

# each unit increase in construction is associated with a ~3.8% rise in flood counts (holding AR structure constant)
# Other variables (GDP, electricity, drainage, land) were excluded as they did not improve model fit.

checkresiduals(best_model)
# no strong autocorrelation left
# Histogram of residuals looks reasonably symmetric.
# ACF plot shows lags within confidence bounds ⇒ good fit.
# Residuals: Ljung-Box p = 0.45 → no autocorrelation. Bell-shaped residuals, little skewness.
