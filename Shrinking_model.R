library(gamlss)
library(gamlss.add)
library(gamlss.dist)
library(gamlss.lasso)

df <- read.csv("Weather_data.csv")

df$date <- as.Date(df$date)
df$time_num <- as.numeric(df$date)

df <- subset(df, df$acc_precip < 50) # removed outlier




#--- Build design matrices that match your formulas ---
X_mu <- model.matrix(
  ~ mean_relative_hum +
    mean_temp:mean_relative_hum +
    bright_sunshine +
    mean_pressure,
  data = df
)[, -1, drop = FALSE]

X_sigma <- model.matrix(
  ~ I(mean_relative_hum^2) +
    mean_temp:mean_relative_hum +
    mean_wind_speed +
    mean_pressure,
  data = df
)[, -1, drop = FALSE]

X_nu <- model.matrix(
  ~ I(mean_temp^2) +
    mean_relative_hum +
    mean_temp:mean_relative_hum +
    mean_wind_speed +
    bright_sunshine +
    mean_pressure,
  data = df
)[, -1, drop = FALSE]
  
# Standardize (important for lasso)
X_mu    <- scale(X_mu)
X_sigma <- scale(X_sigma)
X_nu    <- scale(X_nu)
  

zaga2_ridge <- gamlss(
  acc_precip ~ gnet(X_mu, alpha = 0),
  sigma.formula = ~ gnet(X_sigma, alpha = 0),
  nu.formula    = ~ gnet(X_nu, alpha = 0),
  family = ZAGA,
  data = df
)


# Coefficients per parameter
b_mu    <- coef(zaga2_ridge, what = "mu")
b_sigma <- coef(zaga2_ridge, what = "sigma")
b_nu    <- coef(zaga2_ridge, what = "nu")


rank_mu <- sort(abs(b_mu[-1]), decreasing = TRUE)  # drop intercept
rank_mu

rank_nu <- sort(abs(b_nu[-1]), decreasing = TRUE)
rank_nu

score <- abs(b_mu[-1]) + abs(b_nu[-1])
sort(score, decreasing = TRUE)

