library(gamlss)

set.seed(42)  # Keep fold split reproducible

# Load data
df_all <- read.csv("Weather_data.csv", stringsAsFactors = FALSE)

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
K <- 8
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
  
  # GAMLSS Selection
  m_null <- gamlss(acc_precip ~ 1, data = train_df, family = ZAGA, trace = FALSE)
  
  # 
  m1 <- stepGAIC(m_null, what = "mu", scope = list(lower = ~1, upper = possible_vars), direction = "both", k = 2, trace = FALSE)
  m2 <- stepGAIC(m1, what = "sigma", scope = list(lower = ~1, upper = possible_vars), direction = "both", k = 2, trace = FALSE)
  m3 <- stepGAIC(m2, what = "nu", scope = list(lower = ~1, upper = possible_vars), direction = "both", k = 2, trace = FALSE)
  
  
  # Linear Model Selection
  lm_init <- lm(acc_precip ~ 1, data = train_df)
  lm_cv   <- try(step(lm_init, 
                      scope = list(lower = ~1, upper = possible_vars), 
                      direction = "both", trace = FALSE), 
                 silent = TRUE)
  
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

cat("SD of Actual Rain in Fold 7:", get_test_sd(7), "\n")
cat("SD of Actual Rain in Fold 8:", get_test_sd(8), "\n")
cat("SD of Actual Rain in Fold 9:", get_test_sd(9), "\n")



# We need to get the SD for each test fold to normalize
test_sds <- sapply(1:K, function(k) {
  test_start <- k * fold_size + 1
  test_end   <- (k + 1) * fold_size
  sd(df_all$acc_precip[test_start:test_end], na.rm = TRUE)
})

# Calculate Normalized RMSE (nRMSE)
cv_res$sd_actual <- test_sds
cv_res$nRMSE_gamlss <- cv_res$rmse_gamlss / cv_res$sd_actual
cv_res$nRMSE_lm     <- cv_res$rmse_lm / cv_res$sd_actual

print(cv_res[, c("fold", "rmse_gamlss", "rmse_lm", "sd_actual", "nRMSE_gamlss", "nRMSE_lm")])




# 1. Define the final fold indices
k_final <- 8 
train_end_final  <- k_final * fold_size
test_start_final <- train_end_final + 1
test_end_final   <- (k_final + 1) * fold_size

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

