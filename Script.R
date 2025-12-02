#install.packages("gamlss.add")
#install.packages("gamlss")

library(gamlss)
library(gamlss.add)

df <- read.csv("Weather_data.csv")

df$date <- as.Date(df$date)
df$time_num <- as.numeric(df$date)


plot(df$date, df$acc_precip)

head(df)


hist(df$acc_precip)

d1 <- subset(df, acc_precip > 0 & acc_precip < 10)

hist(d1$acc_precip, breaks = 50)



zaga <- gamlss( # Zero-Adjusted-Gamma
  acc_precip ~ mean_temp + mean_relative_hum + mean_wind_speed +
    bright_sunshine + mean_pressure,
  
  sigma.formula = ~ mean_relative_hum,
  
  nu.formula = ~ mean_temp + mean_relative_hum + mean_wind_speed,
  
  family = ZAGA,
  data = df
)

summary(zaga)

wp(zaga, xvar = df$mean_temp)

vcov(zaga)

df$y_hat <- predict(zaga, type = "response")

df$y_hat


quantile(df$acc_precip, c(0.25,0.5,0.75, 0.8, 0.85, 0.9, 0.95))
quantile(df$y_hat, c(0.25,0.5,0.75, 0.8, 0.85, 0.9, 0.95))



zaga2 <- gamlss(
  acc_precip ~ mean_temp + I(mean_temp^2) +
    mean_relative_hum + I(mean_relative_hum^2) +
    mean_temp:mean_relative_hum +
    mean_wind_speed + bright_sunshine + mean_pressure,
  
  sigma.formula = ~ mean_temp + mean_relative_hum +
    mean_wind_speed + bright_sunshine,
  
  nu.formula = ~ mean_temp + mean_relative_hum +
    mean_pressure + mean_wind_speed,
  
  family = ZAGA,
  data = df
)

wp(zaga2, xvar = df$mean_temp)
wp(zaga2, xvar = df$mean_relative_hum)
plot(zaga2)
GAIC(zaga, zaga2)
