library(gamlss)

df <- read.csv("Weather_data.csv")

df$date <- as.Date(df$date)
df$time_num <- as.numeric(df$date)

df <- subset(df, df$acc_precip < 50)

possible_vars <- ~ mean_temp + mean_relative_hum + mean_pressure + 
  bright_sunshine + mean_wind_speed + 
  I(mean_temp^2) + I(mean_relative_hum^2) + 
  mean_temp:mean_relative_hum

m1 <- stepGAIC(m_null, 
               what = "mu", 
               scope = list(lower = ~1, upper = possible_vars),
               direction = "both",
               k = log(nrow(df)))

m2 <- stepGAIC(m1, 
               what = "sigma", 
               scope = list(lower = ~1, upper = possible_vars),
               direction = "both",
               k = log(nrow(df)))

m3 <- stepGAIC(m2, 
               what = "nu", 
               scope = list(lower = ~1, upper = possible_vars),
               direction = "both",
               k = log(nrow(df)))

summary(m3)
