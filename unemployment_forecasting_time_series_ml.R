library(fredr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(forecast)
library(glmnet)
library(rpart)
library(rpart.plot)

set.seed(123)

#Use environment variable for API key
fredr_set_key(Sys.getenv("FRED_API_KEY"))

get_fred_series <- function(series_id, from = "1990-01-01") {
  fredr(series_id = series_id,
        observation_start = as.Date(from)) |>
    select(date, value) |>
    rename(!!series_id := value)
}

unrate <- get_fred_series("UNRATE")

fedfunds <- get_fred_series("FEDFUNDS")
cpi      <- get_fred_series("CPIAUCSL")
indpro   <- get_fred_series("INDPRO")
payems   <- get_fred_series("PAYEMS")
pce      <- get_fred_series("PCE")
gs10     <- get_fred_series("GS10")

data_raw <- unrate |>
  left_join(fedfunds, by = "date") |>
  left_join(cpi,      by = "date") |>
  left_join(indpro,   by = "date") |>
  left_join(payems,   by = "date") |>
  left_join(pce,      by = "date") |>
  left_join(gs10,     by = "date")

data <- data_raw |>
  drop_na()

data <- data |>
  arrange(date) |>
  mutate(
    dlog_cpi    = 100 * (log(CPIAUCSL) - dplyr::lag(log(CPIAUCSL), 12)),
    dlog_indpro = 100 * (log(INDPRO) - dplyr::lag(log(INDPRO), 12)),
    dlog_payems = 100 * (log(PAYEMS) - dplyr::lag(log(PAYEMS), 12)),
    dlog_pce    = 100 * (log(PCE) - dplyr::lag(log(PCE), 12))
  ) |>
  drop_na()

library(rsample)

data_ts <- data |> arrange(date)

n <- nrow(data_ts)
train_n <- floor(0.8 * n)

#train_data <- data_ts[1:train_n, ]
#test_data  <- data_ts[(train_n + 1):n, ]

covid_start <- as.Date("2020-03-01")
train_data <- data_ts |> filter(date < covid_start)
test_data  <- data_ts |> filter(date >= covid_start)
#---------------TREE REGRESSION---------------
t_fit <- rpart(
  UNRATE ~ dlog_cpi + dlog_indpro + dlog_payems + dlog_pce + FEDFUNDS + GS10,
  data    = train_data,
  method  = "anova",
  control = rpart.control(cp = 0.01)
)

best_cp <- t_fit$cptable[which.min(t_fit$cptable[,"xerror"]), "CP"]
t_pruned <- rpart::prune.rpart(t_fit, cp = best_cp)

rpart.plot(t_pruned)

#tree predictions & errors
tree_pred <- predict(t_pruned, newdata = test_data)
mae_tree  <- mean(abs(tree_pred - test_data$UNRATE))
rmse_tree <- sqrt(mean((tree_pred - test_data$UNRATE)^2))

rmse_tree; mae_tree
t_pruned$variable.importance
#-------------------LASSO---------------
x <- model.matrix(
  UNRATE ~ dlog_cpi + dlog_indpro + dlog_payems + dlog_pce + FEDFUNDS + GS10, data = train_data)[, -1]
y <- train_data$UNRATE

cv_lasso <- cv.glmnet(x, y, alpha = 1)

best_lambda <- cv_lasso$lambda.min
best_lambda

#Fit final LASSO model with lambda.min
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

#Get coefficients
coef(lasso_model)

X_test <- model.matrix(UNRATE ~ dlog_cpi + dlog_indpro + dlog_payems + dlog_pce + FEDFUNDS + GS10, data = test_data)[, -1]

lasso_pred <- predict(lasso_model, newx = X_test)

#Build results for accuracy
lasso_results <- test_data |> mutate(.pred = as.numeric(lasso_pred))

lasso_rmse <- sqrt(mean((lasso_results$UNRATE - lasso_results$.pred)^2))
lasso_mae  <- mean(abs(lasso_results$UNRATE - lasso_results$.pred))

lasso_rmse; lasso_mae
#---------------ARIMA---------------
un_ts <- ts(data$UNRATE, start = c(1990, 1), frequency = 12)
fit_arima <- auto.arima(un_ts)

un_train <- ts(train_data$UNRATE, frequency = 12)
un_test  <- test_data$UNRATE

# Fit ARIMA on train only
fit_arima <- auto.arima(un_train)

# Forecast ahead for exactly the length of the test set
fc_arima <- forecast(fit_arima, h = length(un_test))

# Errors
arima_rmse <- sqrt(mean((fc_arima$mean - un_test)^2))
arima_mae  <- mean(abs(fc_arima$mean - un_test))

coef(fit_arima)
arima_rmse;arima_mae
fit_arima
#---------------ARIMAX---------------
y_train <- ts(train_data$UNRATE, frequency = 12)
y_test  <- test_data$UNRATE

# Predictors: dlogs + level interest rates
x_train <- as.matrix(train_data[, c(
  "dlog_cpi", "dlog_indpro", "dlog_payems", "dlog_pce",
  "FEDFUNDS", "GS10"
)])

x_test <- as.matrix(test_data[, c(
  "dlog_cpi", "dlog_indpro", "dlog_payems", "dlog_pce",
  "FEDFUNDS", "GS10"
)])

# Fit ARIMAX
fit_arimax <- auto.arima(y_train, xreg = x_train)
summary(fit_arimax)

# Forecast over test window
fc_arimax <- forecast(fit_arimax, xreg = x_test, h = length(y_test))

arimax_pred <- as.numeric(fc_arimax$mean)

arimax_rmse <- sqrt(mean((arimax_pred - y_test)^2))
arimax_mae  <- mean(abs(arimax_pred - y_test))

coef(fit_arimax)
arimax_rmse;arimax_mae
fit_arimax
#---------------RIDGE---------------
# 1) Design matrix and response (TRAIN)
x_train <- model.matrix(
  UNRATE ~ dlog_cpi + dlog_indpro + dlog_payems + dlog_pce +
    FEDFUNDS + GS10,
  data = train_data
)[, -1]

y_train <- train_data$UNRATE

# 2) Cross-validated Ridge
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0)

best_lambda_ridge <- ridge_cv$lambda.min
best_lambda_ridge

# 3) Final Ridge model
ridge_model <- glmnet(
  x_train, y_train,
  alpha = 0,
  lambda = best_lambda_ridge
)

# 4) Test design matrix
x_test <- model.matrix(
  UNRATE ~ dlog_cpi + dlog_indpro + dlog_payems + dlog_pce +
    FEDFUNDS + GS10,
  data = test_data
)[, -1]

# 5) Predictions + errors
ridge_pred <- predict(ridge_model, newx = x_test)
ridge_pred <- as.numeric(ridge_pred)

ridge_rmse <- sqrt(mean((ridge_pred - test_data$UNRATE)^2))
ridge_mae  <- mean(abs(ridge_pred - test_data$UNRATE))

coef(ridge_model)
ridge_rmse; ridge_mae

#----
ridge_rmse; ridge_mae
arimax_rmse; arimax_mae1
arima_rmse; arima_mae
lasso_rmse; lasso_mae
rmse_tree; mae_tree
