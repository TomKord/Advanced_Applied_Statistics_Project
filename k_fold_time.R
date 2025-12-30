library(gamlss)

set.seed(42)  # Keep fold split reproducible

# Load data
df_all <- read.csv("Weather_data_2.csv", stringsAsFactors = FALSE)

# Parse date and remove 2025
df_all$date <- as.Date(df_all$date)
df_all$year <- as.integer(format(df_all$date, "%Y"))
df_all$time_index <- seq_len(nrow(df_all))
df_all <- subset(df_all, year != 2025)

# Apply the same preprocessing as in Thomas’s script
df_all <- subset(df_all, acc_precip < 15)  

# model parameters for selection
possible_vars <- ~ mean_temp + mean_relative_hum + mean_pressure + 
  bright_sunshine + mean_wind_speed + 
  I(mean_temp^2) + I(mean_relative_hum^2) + 
  mean_temp:mean_relative_hum + time_index

# define error functions
rmse_fn <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
mae_fn  <- function(y, yhat) mean(abs(y - yhat), na.rm = TRUE)

# 1. Sort data by time 
df_all <- df_all[order(df_all$time_index), ]

# 2. Define Time-Series Folds
# We will use 9 folds. Each fold k will train on blocks 1:k and test on k+1
K <- 12
fold_size <- floor(nrow(df_all) / (K + 1))

cv_res <- data.frame()

for (k in 1:K) {
  # Define indices
  train_end   <- k * fold_size
  test_start  <- train_end + 1
  test_end    <- (k + 1) * fold_size
  
  train_df <- df_all[1:train_end, ]
  test_df  <- df_all[test_start:test_end, ]
  
  # ------------------------------------------------------------
  # Honest Variable Selection (Inside the loop)
  # This prevents data leakage.
  # ------------------------------------------------------------
  
  m_null <- gamlss(acc_precip ~ 1, data = train_df, family = ZAGA, trace = FALSE)
  
  # Explicitly define the scope list for mu
  mu_scope <- list(lower = ~1, upper = possible_vars)
  
  m1 <- stepGAIC(m_null, what = "mu", scope = mu_scope, direction = "both", k = 2, trace = FALSE)
  
  # Note: For sigma and nu, the scope should usually target the same predictors
  m2 <- stepGAIC(m1, what = "sigma", scope = mu_scope, direction = "both", k = 2, trace = FALSE)
  m3 <- stepGAIC(m2, what = "nu", scope = mu_scope, direction = "both", k = 2, trace = FALSE)
  
  # Linear Model Selection
  lm_init <- gamlss(acc_precip ~ 1, sigma.formula = ~1, data = train_df, family = NO)
  lm_cv <- stepGAIC(lm_init, what = "mu", scope = mu_scope, direction = "both", k = 2, trace = FALSE)
  # ------------------------------------------------------------
  # Evaluation
  # ------------------------------------------------------------
  
  # GAMLSS Metrics
  if (!inherits(m3, "try-error")) {
    y_hat_g <- predict(m3, newdata = test_df, type = "response", data = train_df)
    rmse_g  <- rmse_fn(test_df$acc_precip, y_hat_g)
    mae_g   <- mae_fn(test_df$acc_precip, y_hat_g)
    ok_g    <- TRUE
  } else {
    rmse_g <- mae_g <- NA
    ok_g   <- FALSE
  }
  
  # LM Metrics
  if (!inherits(lm_cv, "try-error")) {
    y_hat_l <- predict(lm_cv, newdata = test_df)
    rmse_l  <- rmse_fn(test_df$acc_precip, y_hat_l)
    mae_l   <- mae_fn(test_df$acc_precip, y_hat_l)
    ok_l    <- TRUE
  } else {
    rmse_l <- mae_l <- NA
    ok_l   <- FALSE
  }
  
  # Append results
  cv_res <- rbind(cv_res, data.frame(
    fold = k,
    rmse_gamlss = rmse_g, mae_gamlss = mae_g, converged_gamlss = ok_g,
    rmse_lm = rmse_l, mae_lm = mae_l, converged_lm = ok_l
  ))
}

print(cv_res)


######## IGNORE JUST FOR MODEL TESTING  ##########

# Define a threshold (anything below 0.05mm is effectively dry)
threshold <- 0.01

# GAMLSS: Probability of being zero is the 'nu' parameter
# To get a hard prediction, we check if nu > 0.5 (or use the response)
y_zero_gamlss <- ifelse(y_hat_g < threshold, 0, 1)

# LM: Simply check if prediction is near zero or negative
y_zero_lm <- ifelse(y_hat_l < threshold, 0, 1)

# Actual zeros
y_actual_zero <- ifelse(test_df$acc_precip < threshold, 0, 1)

# Accuracy of "Dry Day" prediction
acc_g <- mean(y_zero_gamlss == y_actual_zero)
acc_l <- mean(y_zero_lm == y_actual_zero)

cat("Dry Day Accuracy - GAMLSS:", acc_g, "\n")
cat("Dry Day Accuracy - LM:", acc_l, "\n")

# 1. Count negative predictions for both models
neg_count_lm <- sum(y_hat_l < 0, na.rm = TRUE)
neg_count_g  <- sum(y_hat_g < 0, na.rm = TRUE)

# 2. Calculate the percentage of predictions that are physically impossible
neg_pct_lm <- mean(y_hat_l < 0, na.rm = TRUE) * 100
neg_pct_g  <- mean(y_hat_g < 0, na.rm = TRUE) * 100

# 3. Find the "worst" negative prediction (the most impossible value)
min_val_lm <- min(y_hat_l, na.rm = TRUE)
min_val_g  <- min(y_hat_g, na.rm = TRUE)

# Print the findings
cat("--- Linear Model Physical Consistency ---\n")
cat("Number of negative predictions:", neg_count_lm, "\n")
cat("Percentage of impossible values:", round(neg_pct_lm, 2), "%\n")
cat("Most extreme negative value predicted:", round(min_val_lm, 4), "\n\n")

cat("--- GAMLSS (ZAGA) Physical Consistency ---\n")
cat("Number of negative predictions:", neg_count_g, "\n")
cat("Percentage of impossible values:", round(neg_pct_g, 2), "%\n")
cat("Most extreme negative value predicted:", round(min_val_g, 4), "\n")

# Helper to get the SD for a specific fold's test set
get_test_sd <- function(k_val) {
  train_end   <- k_val * fold_size
  test_start  <- train_end + 1
  test_end    <- (k_val + 1) * fold_size
  return(sd(df_all$acc_precip[test_start:test_end], na.rm = TRUE))
}

cat("SD of Actual Rain in Fold 10:", get_test_sd(10), "\n")
cat("SD of Actual Rain in Fold 11:", get_test_sd(11), "\n")
cat("SD of Actual Rain in Fold 12:", get_test_sd(12), "\n")




# 1. Get the Range (Max - Min) for each test fold
test_ranges <- sapply(1:K, function(k) {
  test_start <- k * fold_size + 1
  test_end   <- (k + 1) * fold_size
  
  fold_values <- df_all$acc_precip[test_start:test_end]
  
  # Calculate Max - Min
  r <- diff(range(fold_values, na.rm = TRUE))
  
  # Safety: return NA or a very small number if range is 0 to avoid division by zero
  if(r == 0) return(NA) else return(r)
})

# 2. Calculate Normalized RMSE (nRMSE) using the Range
cv_res$range_actual <- test_ranges
cv_res$nRMSE_gamlss <- cv_res$rmse_gamlss / cv_res$range_actual
cv_res$nRMSE_lm     <- cv_res$rmse_lm / cv_res$range_actual

# 3. Print results with the new normalization
print(cv_res[, c("fold", "rmse_gamlss", "rmse_lm", "range_actual", "nRMSE_gamlss", "nRMSE_lm")])


# 1. Define the final fold indices
k <- 12 
train_end_final  <- k * fold_size
test_start_final <- train_end_final + 1
test_end_final   <- (k + 1) * fold_size

test_df_final  <- df_all[test_start_final:test_end_final, ]
train_df_final <- df_all[1:train_end_final, ]

# 2. Refit the model structure (m3) on the full training set
zaga_final <- update(m3, data = train_df_final, trace = FALSE)

# 3. Extract all predicted parameters (mu, sigma, nu)
# We use predictAll to get the components for the classification rule
pa_final <- predictAll(zaga_final, newdata = test_df_final, data = train_df_final)

# 4. Apply the Classification Rule
# We use pa_final$mu (the predicted amount) 
# only if pa_final$nu (probability of zero) is <= 0.5
y_hat_g_zeroed <- ifelse(pa_final$nu > 0.5, 0, pa_final$mu)

# 5. Get LM prediction for comparison
# (Ensure final_lm was the result of your step() function earlier)
lm_final <- update(lm_cv, data = train_df_final)
y_hat_l_final <- predict(lm_final, newdata = test_df_final)

# 6. Final Plot
plot(test_df_final$acc_precip, type = "l", col = "black", lwd = 2,
     main = "GAMLSS Classification vs Observed (Fold 9)",
     ylab = "Precipitation (mm)", xlab = "Days in Fold")
lines(y_hat_g_zeroed, col = "blue", lwd = 1.5)
lines(y_hat_l_final, col = "red", lty = 2)
legend("topright", legend=c("Actual", "GAMLSS (Classified)", "Linear Model"),
       col=c("black", "blue", "red"), lty=c(1,1,2), lwd=2)

# 1. Define metrics (in case they were cleared from memory)
rmse_fn <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
mae_fn  <- function(y, yhat) mean(abs(y - yhat), na.rm = TRUE)

# 2. Re-calculate the two prediction types
# Raw: The full expected value (1-nu)*mu
y_hat_g_raw <- (1 - pa_final$nu) * pa_final$mu

# Hybrid: 0 if prob of zero > 0.5, else the full expected value
y_hat_g_hybrid <- ifelse(pa_final$nu > 0.5, 0, y_hat_g_raw)

# 3. Calculate Scores
actuals <- test_df_final$acc_precip

results_comp <- data.frame(
  Metric = c("RMSE", "MAE"),
  Raw_GAMLSS = c(rmse_fn(actuals, y_hat_g_raw), mae_fn(actuals, y_hat_g_raw)),
  Hybrid_GAMLSS = c(rmse_fn(actuals, y_hat_g_hybrid), mae_fn(actuals, y_hat_g_hybrid))
)

# 4. Print Comparison
print(results_comp)

# 5. Calculate "Hit Rate" for Dry Days (Occurrence Accuracy)
# Actual zeros vs predicted zeros
actual_zeros <- actuals == 0
predicted_zeros_hybrid <- y_hat_g_hybrid == 0

hit_rate <- mean(actual_zeros == predicted_zeros_hybrid)
cat("\nDry Day Prediction Hit Rate (Hybrid):", round(hit_rate * 100, 2), "%\n")

## Hit rate for dry days becomes 70% from 28% !!!!!!!!!!!!!!!

total_actual <- sum(test_df_final$acc_precip)
total_gamlss <- sum(y_hat_g_hybrid)
total_lm     <- sum(y_hat_l_final)

cat("Actual Total Rain:", total_actual, "\n")
cat("GAMLSS Predicted Total:", total_gamlss, "\n")
cat("LM Predicted Total:", total_lm, "\n")

cat("Actual Max Rain:", max(test_df_final$acc_precip), "\n")
cat("GAMLSS Max Prediction:", max(y_hat_g_hybrid), "\n")
cat("LM Max Prediction:", max(y_hat_l_final), "\n")

# Extract Sigma for the test set
sigma_values <- pa_final$sigma

# Summary of sigma
summary(sigma_values)

# Find which day had the highest predicted volatility (extreme sigma)
max_sigma_idx <- which.max(sigma_values)
cat("Highest predicted Sigma:", max(sigma_values), "on day", max_sigma_idx, "\n")

# 1. Calculate the 95th Percentile (The "High End" Risk)
# We use the qZAGA function which gives the quantile for the ZAGA distribution
upper_95 <- qZAGA(0.95, mu = pa_final$mu, sigma = pa_final$sigma, nu = pa_final$nu)

# 2. Create the Plot
plot(test_df_final$acc_precip, type = "n", # "n" creates the frame without plotting points yet
     main = "GAMLSS Risk Assessment (Fold 12)",
     ylab = "Precipitation (mm)", xlab = "Days in Fold",
     ylim = c(0, max(max(test_df_final$acc_precip), max(upper_95))))

# 3. Add the 95% Uncertainty Ribbon
# This represents the area between 0 and the 95th percentile
polygon(c(1:length(upper_95), rev(1:length(upper_95))),
        c(upper_95, rep(0, length(upper_95))),
        col = rgb(0.1, 0.4, 0.8, 0.2), border = NA)

# 4. Add the Observed Rain (The "Truth")
lines(test_df_final$acc_precip, col = "black", lwd = 2)

# 5. Add the Hybrid Mean Prediction (The "Guess")
lines(y_hat_g_hybrid, col = "blue", lwd = 1.5)

legend("topright", 
       legend = c("Observed Rain", "Mean Prediction", "95% Prediction Interval"),
       col = c("black", "blue", rgb(0.1, 0.4, 0.8, 0.2)), 
       lty = c(1, 1, 1), lwd = c(2, 1.5, 10))

# 1. Calculate the Coverage
# Was the actual rain less than or equal to the model's 95th percentile?
covered <- test_df_final$acc_precip <= upper_95

# 2. Calculate the Coverage Probability
coverage_rate <- mean(covered) * 100

# 3. Calculate the "Miss" magnitude
# When the model was wrong, by how much did the rain exceed the 95th percentile?
misses <- test_df_final$acc_precip[test_df_final$acc_precip > upper_95] - upper_95[test_df_final$acc_precip > upper_95]

cat("--- GAMLSS Probabilistic Scorecard ---\n")
cat("Coverage Probability (Target 95%):", round(coverage_rate, 2), "%\n")
cat("Number of days rain exceeded 'Extreme' threshold:", sum(!covered), "\n")
if(length(misses) > 0) {
  cat("Average exceedance on 'surprise' days:", round(mean(misses), 3), "mm\n")
}
# Set up a 1x2 plotting area
par(mfrow = c(1, 2))

# 1. Worm Plot for GAMLSS (zaga_final)
# The wp function is native to gamlss. 
# It checks if the ZAGA distribution correctly handles the skewness and kurtosis.
wp(zaga_final,ylim.all = 0.8, main = "Worm Plot: GAMLSS (ZAGA)")

# 2. Worm Plot for Linear Model (lm_final)
# Standard lm objects don't have a native wp method, 
# so we create a dummy gamlss object with a Normal (NO) distribution 
# using the LM's formula to visualize its residuals properly.
lm_as_gamlss <- gamlss(formula(lm_final), data = train_df_final, family = NO, trace = FALSE)
wp(lm_as_gamlss,ylim.all = 0.8, main = "Worm Plot: Linear Model (Normal)")

# Reset plotting layout
par(mfrow = c(1, 1))

