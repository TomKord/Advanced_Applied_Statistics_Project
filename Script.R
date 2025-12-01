#install.packages("gamlss.add")

library(gamlss)
library(gamlss.add)

df <- read.csv("Weather_data.csv")

df$date <- as.Date(df$date)
df$time_num <- as.numeric(df$date)


plot(df$date, df$acc_precip)

head(df)


hist(df$acc_precip)

d1 <- subset(df, acc_precip > 0 & acc_precip < 10)

d1$acc_precip

hist(d1$acc_precip, breaks = 50)



zaga <- gamlss(
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