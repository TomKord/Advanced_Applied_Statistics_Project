#install.packages("gamlss.add")
#install.packages("gamlss")

library(gamlss)
library(gamlss.add)

df <- read.csv("Weather_data.csv")

df$date <- as.Date(df$date)
df$time_num <- as.numeric(df$date)

lm <- lm(data = df, acc_precip ~ mean_temp + I(mean_temp^2) +
           mean_relative_hum + I(mean_relative_hum^2) +
           mean_temp:mean_relative_hum +
           mean_wind_speed + bright_sunshine + mean_pressure,
)
summary(lm)

AIC(lm)
