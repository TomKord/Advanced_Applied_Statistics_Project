library(gamlss)

#data
df <- read.csv("Weather_data.csv")
df$date <- as.Date(df$date)
df$time_num <- as.numeric(df$date)
df$time_index <- seq_len(nrow(df))

df <- subset(df, df$acc_precip < 15)

# Find the first row index where the Date is "2023-01-01"
split_idx <- which(df$date == "2023-01-02")[1]

# Then split using the same logic as above
train_df <- df[seq_len(split_idx - 1), ]
test_df  <- df[split_idx:nrow(df), ]

#latest stepGAIC model
zaga2 <- gamlss(
  acc_precip ~ I(mean_relative_hum^2) +
    I(mean_temp^2) +
    bright_sunshine + mean_pressure+ time_index,
  
  sigma.formula = ~ bright_sunshine + mean_pressure + mean_wind_speed + 
    I(mean_relative_hum^2) + mean_temp + mean_relative_hum + time_index,
  
  nu.formula = ~ mean_temp + mean_relative_hum + mean_wind_speed + 
    mean_temp:mean_relative_hum + bright_sunshine + mean_pressure,
  
  family = ZAGA,
  data = train_df
)

# 1. Predict the average rain amount (conditional on it raining)
pred_mu <- predict(zaga2, newdata = test_df, what = "mu", type = "response")

# 2. Predict the probability of ZERO rain
pred_nu <- predict(zaga2, newdata = test_df, what = "nu", type = "response")

# 3. Calculate the Expected Value (The Combined Prediction)
# Formula: (Probability of Rain) * (Average Rain Amount)
test_df$predicted_rain <- (1 - pred_nu) * pred_mu


# Calculate RMSE (Root Mean Squared Error)
test_rmse <- sqrt(mean((test_df$acc_precip - test_df$predicted_rain)^2))

# Calculate MAE (Mean Absolute Error)
test_mae <- mean(abs(test_df$acc_precip - test_df$predicted_rain))

cat("Test RMSE:", round(test_rmse, 3), "\n")
cat("Test MAE: ", round(test_mae, 3), "\n")


library(ggplot2)

ggplot(test_df, aes(x = date)) +
  # Actual Rainfall (Black Bars/Lines)
  geom_line(aes(y = acc_precip, color = "Actual")) +
  # Predicted Rainfall (Red Dashed Line)
  geom_line(aes(y = predicted_rain, color = "Predicted"), linetype = "dashed") +
  labs(title = "Model Test: Actual vs Predicted Rainfall",
       y = "Accumulated Precipitation", x = "Date") +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
  theme_minimal()

