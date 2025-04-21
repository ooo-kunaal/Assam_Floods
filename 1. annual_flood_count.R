library(readxl)
library(dplyr)
library(lubridate)

# Load district-wise rainfall data
df <- read_xlsx("assam_rain.xlsx")
df$Date <- as.Date(df$Date, format = "%d/%m/%Y")

# Add Year and define 5-year block
df <- df %>% mutate(Year = year(Date), Block = case_when(
  Year >= 2000 & Year <= 2005 ~ "2000-2005",
  Year >= 2006 & Year <= 2010 ~ "2006-2010",
  Year >= 2011 & Year <= 2015 ~ "2011-2015",
  Year >= 2016 & Year <= 2020 ~ "2016-2020",
  Year >= 2021 ~ "2021-2025" ) )

# Step 1: Calculate block-specific 90th percentile threshold
q_block <- df %>% group_by(Block) %>% summarise(Q90 = quantile(rainfall, 0.9, na.rm = TRUE), .groups = "drop")

# Step 2: Compare daily rainfall with block-level Q90 and flag flood
df <- left_join(df, q_block, by = "Block") %>% mutate(flood = ifelse(rainfall > Q90, 1, 0))

# Step 3: Aggregate flood counts to monthly level
df2 <- df %>% mutate(Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Monthly_Flood_Count = sum(flood, na.rm = TRUE), .groups = "drop") %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))

# Check flood count in 2014
df2[df2$Year == 2014, ]

# Step 4: Aggregate monthly flood count to annual level
df3 <- data.frame(Year = 2000:2025)
df3$Annual_Flood_Count <- sapply(df3$Year, function(y) {
  sum(df2$Monthly_Flood_Count[df2$Year == y], na.rm = TRUE)
})

df3
