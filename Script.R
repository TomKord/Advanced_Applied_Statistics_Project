#install.packages("gamlss.add")
#install.packages("gamlss")

library(gamlss)
library(gamlss.add)

df <- read.csv("Weather_data.csv")

df$date <- as.Date(df$date)
df$time_num <- as.numeric(df$date)

df <- subset(df, df$acc_precip < 15) # removed outlier

plot(df$date, df$acc_precip)

head(df)

hist(df$acc_precip)

#d1 <- subset(df, df$acc_precip >0& df$acc_precip > 50)

#plot(d1$date, d1$acc_precip)
#hist(d1$acc_precip, breaks = 50)



'zaga <- gamlss( # Zero-Adjusted-Gamma
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
quantile(df$y_hat, c(0.25,0.5,0.75, 0.8, 0.85, 0.9, 0.95))'

### First Model

zaga1 <- gamlss(
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

### Current model

zaga2 <- gamlss(
  acc_precip ~ mean_relative_hum +
    mean_temp:mean_relative_hum +
    bright_sunshine + mean_pressure,
  
  sigma.formula = ~ I(mean_relative_hum^2) +
    mean_temp:mean_relative_hum +
    mean_wind_speed + mean_pressure,
  
  nu.formula = ~ I(mean_temp^2) +
    mean_relative_hum +
    mean_temp:mean_relative_hum +
    mean_wind_speed + bright_sunshine + mean_pressure,
  
  family = ZAGA,
  data = df
)

GAIC(zaga1, zaga2)
summary(zaga2)

vcov(zaga2)

wp(zaga2, xvar = df$mean_relative_hum)
plot(zaga2)
GAIC(zaga, zaga2)


df$y_hat <- predict(zaga2, type = "response")

df$y_hat


quantile(df$acc_precip, c(0.25,0.5,0.75, 0.8, 0.85, 0.9, 0.95))
quantile(df$y_hat, c(0.25,0.5,0.75, 0.8, 0.85, 0.9, 0.95))



### Model with autoregressive part

'df$acc_precip_lag1 <- dplyr::lag(df$acc_precip, 2, default = 0)
#df$acc_precip_lag1


zaga3 <- gamlss(
  acc_precip ~ mean_relative_hum +
    mean_temp:mean_relative_hum +
    bright_sunshine + mean_pressure,
  
  sigma.formula = ~ I(mean_relative_hum^2) +
    mean_temp:mean_relative_hum +
    mean_wind_speed + bright_sunshine + mean_pressure,
  
  nu.formula = ~ I(mean_temp^2) +
    mean_relative_hum +
    mean_temp:mean_relative_hum +
    mean_wind_speed + bright_sunshine + mean_pressure + acc_precip_lag1,
  
  family = ZAGA,
  data = df
)

GAIC(zaga3)
summary(zaga3)'


