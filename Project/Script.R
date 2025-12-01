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





# ZAGA
#m_zaga <- gamlss(
#  acc_precip ~ pb(time_num), sigma.formula = ~ 1, nu.formula = ~ pb(time_num), family = ZAGA, data = data,
#  n.cyc = 1000,
#  c.crit = 0.1)

# ZALG
#m_zalg <- gamlss(acc_precip ~ pb(time_num), sigma.formula = ~ 1, nu.formula = ~ pb(time_num), family = ZALG, data = data,
#  n.cyc = 1000,
#  c.crit = 0.1)

# ZAPIG
#m_zalg <- gamlss(acc_precip ~ pb(time_num), sigma.formula = ~ 1, nu.formula = ~ pb(time_num), family = ZAPIG, data = data,
#  n.cyc = 1000,
#  c.crit = 0.1)

#GAIC(m_zaga, m_zalg, m_zapig, k = 2)


# ZAPIG
#ctrl <- gamlss.control(
#  n.cyc = 50,
#  c.crit = 0.1,
#  mu.step = 0.5,
#  sigma.step = 0.5,
#  nu.step = 0.5
#)



zaga <- gamlss(
  acc_precip ~ mean_temp + mean_relative_hum + mean_wind_speed +
    bright_sunshine + mean_pressure,
  
  sigma.formula = ~ mean_relative_hum,
  
  nu.formula = ~ mean_temp + mean_relative_hum + mean_wind_speed,
  
  family = ZAGA,
  data = df,
#  control = ctrl
)

summary(zaga)

wp(zaga, xvar = df$mean_temp)

vcov(zaga)
