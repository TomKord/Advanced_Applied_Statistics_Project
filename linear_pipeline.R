library(gamlss)
###### StepGAIC parameter selection 
#data
df <- read.csv("Weather_data_2.csv")
df$date <- as.Date(df$date)
df$time_num <- as.numeric(df$date)
df$time_index <- seq_len(nrow(df))

df <- subset(df, df$acc_precip < 15)

# Find the first row index where the Date is "2023-01-01"
split_idx <- which(df$date == "2023-01-02")[1]

# Then split using the same logic as above
train_df <- df[seq_len(split_idx - 1), ]
test_df  <- df[split_idx:nrow(df), ]

#full_formula <- acc_precip ~ mean_temp + mean_relative_hum + mean_pressure + bright_sunshine + mean_wind_speed + I(mean_temp^2) + I(mean_relative_hum^2) + mean_temp:mean_relative_hum + time_index

possible_vars <- ~ mean_temp + mean_relative_hum + mean_pressure + 
  bright_sunshine + mean_wind_speed + 
  I(mean_temp^2) + I(mean_relative_hum^2) + 
  mean_temp:mean_relative_hum + time_index

# start with full the remove parameters
#lm_full <- lm(full_formula, data = train_df)
#final_lm <- step(lm_full, direction = "both")
lm_init <- gamlss(acc_precip ~ 1, sigma.formula = ~1, data = train_df, family = NO)
final_lm <- stepGAIC(lm_init, what = "mu", scope = list(lower = ~1, upper = possible_vars), direction = "both", k = 2, trace = FALSE)

summary(final_lm)

predictions <- predict(final_lm, newdata = test_df, type = "response")
# Add raw predictions to the test dataframe
test_df$pred_lm <- predictions

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
AIC(final_lm)
library(ggplot2)

ggplot(test_df, aes(x = date)) +
  # Actual Rainfall (Black Bars/Lines)
  geom_line(aes(y = acc_precip, color = "Actual")) +
  # Predicted Rainfall (Red Dashed Line)
  geom_line(aes(y = predictions, color = "Predicted"), linetype = "dashed") +
  labs(title = "Model Test: Actual vs Predicted Rainfall",
       y = "Accumulated Precipitation", x = "Date") +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
  theme_minimal()

#Covariance penalty

# A. Refit on FULL data
# As per your GAMLSS example, we calculate the penalty on the full dataset structure
final_lm_full <- update(final_lm, data = df)




###### IMPORTANT: RUN MODEL PREDICTED BY CV HERE #########
final_lm_full <- lm_final
# B. Setup Bootstrap
B <- 200       # Number of bootstraps
n <- nrow(train_df_final)

# Matrices to store results
sim_y_mat <- matrix(NA, nrow = n, ncol = B)
new_y_hat_mat <- matrix(NA, nrow = n, ncol = B)

# Extract fitted parameters from the Linear Model (gamlss)
# 1. Fitted Means (mu)
mu_fit <- predict(final_lm_full, type = "response")
# 2. Residual Standard Error (sigma) - assumed constant in LM
sigma_fit <- fitted(final_lm_full, what = "sigma")


set.seed(123)
cat("Running Bootstrap for Linear Model Covariance Penalty...\n")

for(i in 1:B) {
  # --- 1. Generate Synthetic Data ---
  # Since lm() assumes a Normal distribution, we simulate using rnorm.
  # Note: This will generate negative rain values. This is expected for LM.
  y_sim <- rnorm(n, mean = mu_fit, sd = sigma_fit)
  
  # Save the simulated y
  sim_y_mat[, i] <- y_sim
  
  # --- 2. Refit the Model ---
  df_sim <- train_df_final
  df_sim$acc_precip <- y_sim
  
  # Update the model with the new synthetic target variable
  refit_model <- update(final_lm_full, data = df_sim)
  
  # --- 3. Predict ---
  new_y_hat_mat[, i] <- predict(refit_model, type = "response")
  
  if(i %% 10 == 0) cat(i, "of", B, "complete...\n")
}

# ==========================================
# 3. Calculate Penalty & Final Error
# ==========================================
cov_penalties <- numeric(n)

for(i in 1:n) {
  y_vec <- sim_y_mat[i, ]
  y_hat_vec <- new_y_hat_mat[i, ]
  
  # Covariance between the simulated noise and the model's reaction to it
cov_penalties[i] <- cov(y_vec, y_hat_vec)
}

# Total Optimism (2 * Sum of Covariances)
total_optimism <- 2 * sum(cov_penalties)

# Calculate Training Error (SSE) on the original data
# We use the raw predictions (even if negative) to be mathematically consistent with LM
original_predictions <- predict(final_lm_full, type = "response")
training_se <- sum((train_df_final$acc_precip - original_predictions)^2)

final_prediction_error <- training_se + total_optimism

# ==========================================
# 4. Output Results
# ==========================================
cat("------------------------------------------------\n")
cat("Model Type:             Linear Model (Gaussian)\n")
cat("Training Error (SSE):   ", round(training_se, 4), "\n")
cat("Covariance Penalty (Op):", round(total_optimism, 4), "\n")
cat("Estimated True Error:   ", round(final_prediction_error, 4), "\n")
cat("------------------------------------------------\n")

# Set up a 2x2 grid to see all plots at once
par(mfrow = c(2, 2))

# This command automatically generates the 4 standard diagnostic plots
plot(final_lm)

# Reset grid to normal
par(mfrow = c(1, 1))
AIC(final_lm)

#Wormplot

wp(final_lm_full, 
   xvar = df$mean_relative_hum, 
   n.inter = 4, 
   ylim.worm = 1,  # Linear models often have huge errors, so we increase the y-limits
   main = "Worm Plot: Linear Model (Gaussian) on Rainfall")

