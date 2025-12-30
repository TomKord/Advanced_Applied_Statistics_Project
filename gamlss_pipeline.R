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

 Then split using the same logic as above
train_df <- df[seq_len(split_idx - 1), ]
test_df  <- df[split_idx:nrow(df), ]


possible_vars <- ~ mean_temp + mean_relative_hum + mean_pressure + 
  bright_sunshine + mean_wind_speed + 
  I(mean_temp^2) + I(mean_relative_hum^2) + 
  mean_temp:mean_relative_hum + time_index

m_null <- gamlss(acc_precip ~ 1, 
                 data = train_df, 
                 family = ZAGA)

m1 <- stepGAIC(m_null, 
               what = "mu", 
               scope = list(lower = ~1, upper = possible_vars),
               direction = "both",
               k = 2)

m2 <- stepGAIC(m1, 
               what = "sigma", 
               scope = list(lower = ~1, upper = possible_vars),
               direction = "both",
               k = 2)

m3 <- stepGAIC(m2, 
               what = "nu", 
               scope = list(lower = ~1, upper = possible_vars),
               direction = "both",
               k = 2)

summary(m3)


######### Model testing using train/test split

# 1. Predict the average rain amount (conditional on it raining)
pred_mu <- predict(m3, newdata = test_df, what = "mu", type = "response")

# 2. Predict the probability of ZERO rain
pred_nu <- predict(m3, newdata = test_df, what = "nu", type = "response")

# 3. Calculate the Expected Value (The Combined Prediction)
# Formula: (Probability of Rain) * (Average Rain Amount)
test_df$predicted_rain <- (1 - pred_nu) * pred_mu


# Calculate RMSE (Root Mean Squared Error)
test_rmse <- sqrt(mean((test_df$acc_precip - test_df$predicted_rain)^2))

# Calculate MAE (Mean Absolute Error)
test_mae <- mean(abs(test_df$acc_precip - test_df$predicted_rain))

cat("Test RMSE:", round(test_rmse, 3), "\n")
cat("Test MAE: ", round(test_mae, 3), "\n")
AIC(m3)

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

######### Covariance penalty approach
#refit model on full data for cov pen approach
m3 <- update(m3, data = df)
#boostrap




###### IMPORTANT: RUN MODEL PREDICTED BY CV HERE #########
m3 <- zaga_final

# 1. Setup
B <- 100  # Number of bootstraps (Use >= 200 for publication, 50 for testing)
n <- nrow(df)
#original_predictions <- predict(m3, type = "response")

# Matrices to store results
# We store the simulated y values and the corresponding predicted values
sim_y_mat <- matrix(NA, nrow = n, ncol = B)
new_y_hat_mat <- matrix(NA, nrow = n, ncol = B)

# Extract fitted parameters to simulate from
mu_fit <- predict(m3, what="mu", type="response")
sigma_fit <- predict(m3, what="sigma", type="response")
nu_fit <- predict(m3, what="nu", type="response")

# 2. Bootstrap Loop
set.seed(123)
cat("Running Bootstrap for Covariance Penalty...\n")

for(i in 1:B) {
  # A. Generate synthetic data from the fitted ZAGA distribution
  # Note: rZAGA generates random deviations based on model params
  y_sim <- rZAGA(n, mu = mu_fit, sigma = sigma_fit, nu = nu_fit)
  
  # Save the simulated y
  sim_y_mat[, i] <- y_sim
  
  # B. Refit the model to this synthetic data
  # We update the 'data' argument in the call
  # NOTE: gamlss refitting can be tricky with data environments. 
  # We temporarily assign y_sim to the dataframe
  df_sim <- df
  df_sim$acc_precip <- y_sim
  
  # Refit (suppressing trace for cleaner output)
  # We use update() to keep the exact same formula structure
  refit_model <- try(update(m3, data = df_sim, trace = FALSE), silent = TRUE)
  
  if(!inherits(refit_model, "try-error")) {
    # C. Predict on the original x values using the NEW fitted model
    new_y_hat_mat[, i] <- predict(refit_model, type = "response")
  } else {
    # Handle convergence failures if any
    new_y_hat_mat[, i] <- NA 
  }
  
  if(i %% 10 == 0) cat(i, "of", B, "complete...\n")
}

# 3. Calculate Covariance Penalty
cov_penalties <- numeric(n)

# Iterate through every data point (row)
for(i in 1:n) {
  # Get the vector of simulated y's and predicted y's for observation i
  y_vec <- sim_y_mat[i, ]
  y_hat_vec <- new_y_hat_mat[i, ]
  
  # Remove NAs if any fits failed
  valid <- !is.na(y_hat_vec)
  
  # Calculate covariance for this specific data point
  if(sum(valid) > 2) {
    cov_penalties[i] <- cov(y_vec[valid], y_hat_vec[valid])
  } else {
    cov_penalties[i] <- 0
  }
}

# Sum the covariances to get the Total Penalty (Optimism)
# Factor of 2 is from Efron's theorem
total_optimism <- 2 * sum(cov_penalties)

# 4. Final Calculation
# Calculate Training Error (Squared Error for this example)
# Formula: Probability of Rain * Average Amount
original_predictions <- (1- nu_fit)*mu_fit
training_se <- sum((df$acc_precip - original_predictions)^2)

# Estimate of True Prediction Error
final_prediction_error <- training_se + total_optimism

cat("------------------------------------------------\n")
cat("Training Error (SSE):   ", training_se, "\n")
cat("Covariance Penalty (Op):", total_optimism, "\n")
cat("Estimated True Error:   ", final_prediction_error, "\n")

# Simple Worm Plot
m3_refit <- update(m3, data = df)

# Now run the worm plot on the refreshed object
wp(m3_refit, xvar = df$mean_relative_hum, , ylim.worm= 0.5, n.inter=4, main = "Worm Plot: Conditional on Humidity")

