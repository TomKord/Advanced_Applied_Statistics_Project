data <- read.csv("Weather_data.csv")
data$date <- as.Date(data$date)

## -------------------------
## Ausreißer in acc_precip entfernen
## -------------------------
q99 <- quantile(data$acc_precip, 0.99, na.rm = TRUE)  # 99%-Perzentil
data <- subset(data, acc_precip <= q99)          # Extreme Values raus

# Zielvariable
y <- data$acc_precip

# Prädiktoren (ohne Datum, ohne stationId)
X <- subset(data, select = -c(stationId, date, acc_precip))

library(glmnet)

X_matrix <- as.matrix(X)
y_vector <- as.numeric(y)

## -------------------------
## Ridge
## -------------------------
ridge_model <- glmnet(X_matrix, y_vector, alpha = 0)

# Plot der Koeffizientenpfade
plot(ridge_model, xvar = "lambda")

cv_ridge <- cv.glmnet(X_matrix, y_vector, alpha = 0)
plot(cv_ridge)

best_lambda_ridge <- cv_ridge$lambda.min
best_lambda_ridge

# Modell mit optimalem Lambda
ridge_best <- glmnet(X_matrix, y_vector, alpha = 0, lambda = best_lambda_ridge)
coef(ridge_best)

## -------------------------
## Lasso
## -------------------------
lasso_model <- glmnet(X_matrix, y_vector, alpha = 1)
plot(lasso_model, xvar = "lambda")

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 1)
plot(cv_lasso)

best_lambda_lasso <- cv_lasso$lambda.min
best_lambda_lasso

# Finales Modell
lasso_best <- glmnet(X_matrix, y_vector, alpha = 1, lambda = best_lambda_lasso)
coef(lasso_best)

## =========================================================
## PREDICTIONS VS ACTUAL VALUES – PLOTS
## =========================================================

# Ridge: Vorhersagen
pred_ridge <- as.numeric(predict(ridge_best, newx = X_matrix))

# Lasso: Vorhersagen
pred_lasso <- as.numeric(predict(lasso_best, newx = X_matrix))

## 1) Zeitreihenplots: Actual vs Predicted über das Datum

par(mfrow = c(2, 1))  # zwei Plots untereinander

# Ridge
plot(data$date, y_vector, type = "l",
     main = "Ridge: Actual vs Predicted (acc_precip)",
     xlab = "date", ylab = "acc_precip")
lines(data$date, pred_ridge, col = "red")
legend("topleft",
       legend = c("Actual", "Predicted (Ridge)"),
       col = c("black", "red"), lty = 1, bty = "n")

# Lasso
plot(data$date, y_vector, type = "l",
     main = "Lasso: Actual vs Predicted (acc_precip)",
     xlab = "date", ylab = "acc_precip")
lines(data$date, pred_lasso, col = "blue")
legend("topleft",
       legend = c("Actual", "Predicted (Lasso)"),
       col = c("black", "blue"), lty = 1, bty = "n")

## 2) Scatterplots: Actual vs Predicted mit 45°-Linie

par(mfrow = c(1, 2))  # zwei Plots nebeneinander

# Ridge
plot(y_vector, pred_ridge,
     main = "Ridge: Predicted vs Actual",
     xlab = "Actual acc_precip",
     ylab = "Predicted acc_precip")
abline(0, 1, col = "red", lwd = 2)  # ideale Linie

# Lasso
plot(y_vector, pred_lasso,
     main = "Lasso: Predicted vs Actual",
     xlab = "Actual acc_precip",
     ylab = "Predicted acc_precip")
abline(0, 1, col = "blue", lwd = 2)

hist(data$acc_precip, breaks = 50)
data1 <- data$acc_precip
data1 = data[,data$acc_precip > 0]
hist(data1)
