# Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(corrplot)
library(GGally)
library(scales)

# Load the final dataset
df <- read.csv("final_data.csv")

# ==========================
# 1. Correlation Analysis
# ==========================
cor_matrix <- cor(df %>% select(-Year), use = "complete.obs")
# write.csv(cor_matrix, 'corr.csv')
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.9, number.cex = 0.7, addCoef.col = "black", col = colorRampPalette(c("white", "steelblue"))(200), mar = c(0, 0, 2, 0))

# ==========================
# 2. Time Trends (Urbanization + Flood)
# ==========================
vars <- names(df)[!(names(df) %in% c("Year", "Flood"))]

for (v in vars) {
  p <- ggplot(df, aes_string(x = "Year", y = v)) +
    geom_line(color = "#0073C2FF", size = 1) +
    geom_point(size = 1.5) +
    labs(title = paste("Trend of", v, "over time"), y = v, x = "Year") +
    theme_minimal()
  print(p)
}

# Flood trend over time
ggplot(df, aes(x = Year, y = Flood)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(size = 2, color = "darkred") +
  labs(title = "Flood Count Trend Over Time", y = "Flood Count") +
  theme_minimal()

# ==========================
# 3. Distribution of Predictors
# ==========================
par(mfrow=c(3,3))
for(i in 2:ncol(df)){
  hist(df[,i], prob = T,main=paste('Sample Density of', colnames(df)[i]), xlab='',ylab='', ylim=c(min(density(df[,i])$y), max(density(df[,i])$y) ))
  lines(density(df[,i]))
}
par(mfrow=c(1,1))

## over-dispersion i.e. variance > mean?
mean(df$Flood) # mean flood count = 171.84
var(df$Flood)  #  var flood count = 36023.89
# clearly there over-dispersion in flood count

# ==========================
# 4. Scatter plots vs Flood
# ==========================
par(mfrow=c(3,3))
for(i in 3:ncol(df)){
  plot(df[,i],df$Flood ,main=paste('Scatter Plot Flood vs', colnames(df)[i]), xlab=colnames(df)[i],ylab='Flood')
  abline(lm(df$Flood ~ df[,i]), lty=2, col='navy',lwd=2)
}
par(mfrow=c(1,1))

# ==========================
# 5. Multivariate Visualization
# ==========================
ggpairs(df%>% select(-Year))
