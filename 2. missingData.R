library(dplyr)
library(readxl)
library(forecast)
library(readxl)
library(mice)
library(missForest)
library(VIM)
library(dplyr)
library(openxlsx)

df <- read_xlsx("input_data.xlsx") %>% as.data.frame()
df <- df %>% mutate(across(-Year, as.numeric))

### 1. time series forecast for flood
ts = ts(df$Flood[-nrow(df)],start = c(1,2001),freq=1)
ts.plot(ts)
points(ts)

fit = auto.arima(ts)
## ARIMA(1,1,0)
# xt = yt - yt-1
# Xt = -0.4648*Xt-1 + et
# et ~ N(0, 9476)

plot(forecast(fit, 1))
f = forecast(fit, 1) # forecast = 371
df$Flood[nrow(df)] = as.numeric(f$mean) # replace with missing value

### 2. time series forecast for domestic electricity consumption
ts = ts(df$`Domestic Electricity Consumption (MU)`[-nrow(df)],start = c(1,2001),freq=1)
ts.plot(ts)
points(ts)

fit = auto.arima(ts)
## ARIMA(0,2,1)
# xt = yt - yt-1
# Wt = Xt - Xt-1
# wt = -0.7661*et-1 + et
# et ~ N(0, 65176)

plot(forecast(fit, 1))
f = forecast(fit, 1) # forecast = 6060.417
df$`Domestic Electricity Consumption (MU)`[nrow(df)] = as.numeric(f$mean) # replace with missing value

### 3. time series forecast for drainage channel
ts = ts(df$`Drainage Channels (kms)`[-nrow(df)],start = c(1,2001),freq=1)
ts.plot(ts)
points(ts)

fit = auto.arima(ts)
## ARIMA(0,1,0)
# xt = yt - yt-1
# Xt = 2.7404 + et
# et ~ N(0, 12.76)

plot(forecast(fit, 1))
f = forecast(fit, 1) # forecast = 911.76
df$`Drainage Channels (kms)`[nrow(df)] = as.numeric(f$mean) # replace with missing value

### 4. time series forecast for food grain production
ts = ts(df$`total food grains (yeild/hect)`[-nrow(df)],start = c(1,2001),freq=1)
ts.plot(ts)
points(ts)

fit = auto.arima(ts)
## ARIMA(1,1,0)
# xt = yt - yt-1
# Xt = 32.3869 -0.6434*xt-1 + et
# et ~ N(0, 19731)

plot(forecast(fit, 1))
f = forecast(fit, 1) # forecast = 2069.65
df$`total food grains (yeild/hect)`[nrow(df)] = as.numeric(f$mean) # replace with missing value

# Drop Year column for imputation
df_data <- df %>% select(-Year)
# Normalize to [0, 1]
mins <- sapply(df_data, min, na.rm = TRUE)
maxs <- sapply(df_data, max, na.rm = TRUE)

normalize <- function(x, min_val, max_val) {
  return((x - min_val) / (max_val - min_val))
}

df_norm <- as.data.frame(Map(normalize, df_data, mins, maxs))

# Imputation Methods
## 1. MICE
mice_model <- mice(df_norm, m = 1, method = 'pmm', seed = 123)
df_mice_norm <- complete(mice_model)

## 2. MissForest
set.seed(123)
df_mf_norm <- missForest(df_norm)$ximp

## 3. KNN
df_knn_norm <- kNN(df_norm, k = 5, imp_var = FALSE)

# De-normalize back to original scale
denormalize <- function(x, min_val, max_val) {
  return(x * (max_val - min_val) + min_val)
}

df_mice <- as.data.frame(Map(denormalize, df_mice_norm, mins, maxs))
df_mf   <- as.data.frame(Map(denormalize, df_mf_norm, mins, maxs))
df_knn  <- as.data.frame(Map(denormalize, df_knn_norm, mins, maxs))

# Add Year back
df_mice$Year <- df$Year
df_mf$Year   <- df$Year
df_knn$Year  <- df$Year

# Reorder columns
df_mice <- df_mice %>% select(Year, everything())
df_mf   <- df_mf   %>% select(Year, everything())
df_knn  <- df_knn  %>% select(Year, everything())

# write.xlsx(list(Original = df, MICE_Imputed = df_mice, MissForest_Imputed = df_mf, KNN_Imputed = df_knn ), file = "Imputation_Comparison.xlsx")

