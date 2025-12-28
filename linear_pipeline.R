library(gamlss)
###### StepGAIC parameter selection 
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

full_formula <- acc_precip ~ mean_temp + mean_relative_hum + mean_pressure + 
  bright_sunshine + mean_wind_speed + 
  I(mean_temp^2) + I(mean_relative_hum^2) + 
  mean_temp:mean_relative_hum + time_index

# start with full the remove parameters
lm_full <- lm(full_formula, data = train_df)
final_lm <- step(lm_full, direction = "both")

summary(final_lm)

predictions <- predict(final_lm, newdata = test_df, type = "response")
# Add raw predictions to the test dataframe
test_df$pred_lm <- predictions

# Physically, rain cannot be negative, so we must clamp these to 0.
test_df$pred_lm_corrected <- ifelse(test_df$pred_lm < 0, 0, test_df$pred_lm)

# Check how many impossible predictions we fixed
#n_neg <- sum(test_df$pred_lm < 0)
#cat(sprintf("Note: The linear model predicted negative rain for %d days. These were clamped to 0.\n", n_neg))

# Calculate the errors (Residuals) using the CORRECTED predictions
#errors <- test_df$pred_lm_corrected - test_df$acc_precip
errors <- test_df$pred_lm - test_df$acc_precip
# A. RMSE (Root Mean Square Error)
# Penalizes large errors heavily (good for spotting if you missed big storms)
rmse_score <- sqrt(mean(errors^2))

# B. MAE (Mean Absolute Error)
# The average "distance" your prediction is from the truth
mae_score <- mean(abs(errors))

# C. Bias (Mean Error)
# Positive = Model generally over-predicts (cries wolf)
# Negative = Model generally under-predicts (misses rain)
bias_score <- mean(errors)

# ==========================================
# 4. Print Results
# ==========================================
cat("\n--- Model Evaluation Results ---\n")
cat("RMSE: ", round(rmse_score, 4), " cm\n")
cat("MAE:  ", round(mae_score, 4), " cm\n")
cat("Bias: ", round(bias_score, 4), " cm\n")

