library(gamlss)
df <- read.csv("Weather_data.csv")

df$date <- as.Date(df$date)
df$time_num <- as.numeric(df$date)

df <- subset(df, df$acc_precip < 50) # removed outlier
#gen.Family("BCT", "zero")
# gamlss model
zaga1 <- gamlss(
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
  #family = ZABCT,
  data = df
)
# gamlss 2nd model from stepGAIC
zaga2 <- gamlss(
  acc_precip ~ mean_relative_hum +
    I(mean_temp^2) +
    bright_sunshine + mean_pressure,
  
  sigma.formula = ~ bright_sunshine + mean_pressure + mean_wind_speed + 
    I(mean_relative_hum^2) + mean_temp + mean_relative_hum,
  
  nu.formula = ~ mean_temp + mean_relative_hum + mean_wind_speed + 
    mean_temp:mean_relative_hum + bright_sunshine + mean_pressure,
  
  family = ZAGA,
  #family = ZABCT,
  data = df
)
#model 3
zaga2 <- gamlss(
  # MU: Use the simplified version from zaga2
  acc_precip ~ mean_relative_hum + I(mean_temp^2) + 
    bright_sunshine + mean_pressure,
  
  # SIGMA: Use the robust version from zaga1 (CRITICAL)
  sigma.formula = ~ I(mean_relative_hum^2) + mean_temp:mean_relative_hum + 
    mean_wind_speed + mean_pressure,
  
  # NU: Use the version from zaga2 (it seems to have kept the important stuff)
  nu.formula = ~ mean_temp + mean_relative_hum + mean_wind_speed + 
    mean_temp:mean_relative_hum + bright_sunshine + mean_pressure,
  
  family = ZAGA,
  data = df
)
GAIC(zaga1, zaga2)
summary(zaga2)

# 1. Setup
B <- 100  # Number of bootstraps (Use >= 200 for publication, 50 for testing)
n <- nrow(df)
original_predictions <- predict(zaga2, type = "response")

# Matrices to store results
# We store the simulated y values and the corresponding predicted values
sim_y_mat <- matrix(NA, nrow = n, ncol = B)
new_y_hat_mat <- matrix(NA, nrow = n, ncol = B)

# Extract fitted parameters to simulate from
mu_fit <- predict(zaga2, what="mu", type="response")
sigma_fit <- predict(zaga2, what="sigma", type="response")
nu_fit <- predict(zaga2, what="nu", type="response")

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
  refit_model <- try(update(zaga2, data = df_sim, trace = FALSE), silent = TRUE)
  
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
training_se <- sum((df$acc_precip - original_predictions)^2)

# Estimate of True Prediction Error
final_prediction_error <- training_se + total_optimism

cat("------------------------------------------------\n")
cat("Training Error (SSE):   ", training_se, "\n")
cat("Covariance Penalty (Op):", total_optimism, "\n")
cat("Estimated True Error:   ", final_prediction_error, "\n")

# Simple Worm Plot
zaga2_refit <- update(zaga2, data = df)

# Now run the worm plot on the refreshed object
wp(zaga2_refit, xvar = df$mean_relative_hum, , ylim.worm= 1, n.inter=4, main = "Worm Plot: Conditional on Humidity")

# Split the worm plot by 'mean_relative_hum' intervals
# n.inter = 4 splits the data into 4 ranges of humidity
wp(zaga2_refit, xvar = df$mean_relative_hum, ylim.worm = 0.5, n.inter = 4, 
   main = "Worm Plot: Conditional on Humidity")
