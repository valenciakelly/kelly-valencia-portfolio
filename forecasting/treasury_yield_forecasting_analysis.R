library('forecast')
library('readxl')
library('ggplot2')
library("writexl")

WGS10YR_data <- read_excel("WGS10YR.xlsx")
head(WGS10YR_data)

WGS10YR_data <- read_excel("WGS10YR.xlsx", col_types = c("date", "numeric"))
WGS10YR_data$observation_date <- as.Date(WGS10YR_data$observation_date)

head(WGS10YR_data)
tail(WGS10YR_data)

WGS10YR_ts <- ts(WGS10YR_data$WGS10YR, start = c(2000, 1), frequency = 52)
plot(WGS10YR_ts, main = "10-Year Treasury Yield", xlab = "Year", ylab = "Yield (%)")

y <- as.numeric(WGS10YR_ts)
train_end <- sum(WGS10YR_data$observation_date < as.Date("2011-01-01"))

#store forecasts
dates <- as.Date(WGS10YR_data$observation_date)
n <- length(y)

# For each model, store forecasts aligned to the target period
fc_1A <- rep(NA, n)
fc_1B <- rep(NA, n)
fc_1C <- rep(NA, n)

fc_rw  <- rep(NA, n)
fc_rwd  <- rep(NA, n)

R <- train_end 
cat("The numerical value of R (the in-sample estimation period used to construct first forecast is): ",R, "\n")

P <- n - R
cat("The numerical value of P (the out-of-sample forecast period is):",P, "\n")

T <- n
cat("The numerical value of T (the total sample size is):",T,"\n")

#empty vectors to store the forecast errors after we observe what actually happened
errors_1A <- c()
errors_1B <- c()
errors_1C <- c()

errors_rw <- c()
errors_rwd <- c()

#Simple Model of Mean

#Model 1A #1/1(yt)
for (t in train_end:(n-1)) {
  fc_1A[t+1] <- y[t]
  actual1A <- y[t+1]
  error1A <- actual1A - fc_1A[t+1]
  errors_1A <- c(errors_1A, error1A)
} 

#Model 1B 1/4(yt-3 + yt-2 + yt-1 + yt)
for (t in train_end:(n-1)) {
  fc_1B[t+1] <- mean(y[(t-3):t])
  actual1B <- y[t+1]
  error1B <- actual1B - fc_1B[t+1]
  errors_1B <- c(errors_1B, error1B)
}

#Model 1C 1/52(yt-51 + ... + yt)
for (t in train_end:(n-1)) {
  fc_1C[t+1] <- mean(y[(t-51):t])
  actual1C <- y[t+1]
  error1C <- actual1C - fc_1C[t+1]
  errors_1C <- c(errors_1C, error1C)
}

#Model 2 Random Walk with Drift

for (t in train_end:(n-1)) {
  drift <- mean(diff(y[1:t])) #drift is the average change up to time t
  fc_rwd[t+1] <- drift + y[t] 
  actualrwd <- y[t+1]
  errorrwd <- actualrwd - fc_rwd[t+1]
  errors_rwd <- c(errors_rwd, errorrwd)
}

#Model 3 Random Walk without Drift

for (t in train_end:(n-1)) {
  fc_rw[t+1] <- y[t] # random walk without drift
  actualrw <- y[t+1]
  errorrw <- actualrw - fc_rw[t+1]
  errors_rw <- c(errors_rw, errorrw)
}

msfe <- function(e) mean(e^2, na.rm = TRUE)
rmse <- function(e) sqrt(mean(e^2, na.rm=TRUE))

MSFE_results <- data.frame(
  Model = c("Model 1A", "Model 1B", "Model 1C", "Random Walk with Drift", "Random Walk without Drift"),
  MSFE = c(msfe(errors_1A), msfe(errors_1B), msfe(errors_1C), msfe(errors_rwd), msfe(errors_rw)),
  RMSE = c(rmse(errors_1A), rmse(errors_1B), rmse(errors_1C), rmse(errors_rwd), rmse(errors_rw))
)

print(MSFE_results)

#Print first 10 rows of the forecast errors 
errors_10 <- data.frame(
  Date = dates[(train_end+1):(train_end+10)],
  Model_1A = errors_1A[1:10],
  Model_1B = errors_1B[1:10],
  Model_1C = errors_1C[1:10],
  RWD = errors_rwd[1:10],
  RW = errors_rw[1:10]
)

cat("First 10 Out-of-Sample Forecast Errors (h = 1)\n")

print(errors_10)

forecasts_10 <- data.frame(
  Date = dates[(train_end+1):(train_end+10)],
  Model_1A = fc_1A[(train_end+1):(train_end+10)],
  Model_1B = fc_1B[(train_end+1):(train_end+10)],
  Model_1C = fc_1C[(train_end+1):(train_end+10)],
  RWD = fc_rwd[(train_end+1):(train_end+10)],
  RW = fc_rw[(train_end+1):(train_end+10)]
)

cat("First 10 Out-of-Sample Forecasts (h = 1)\n")

print(forecasts_10)

plot1 <- data.frame(
  Date   = dates,
  Actual = y,
  Model_1A = fc_1A,
  Model_1B = fc_1B,
  Model_1C = fc_1C,
  RWD = fc_rwd,
  RW = fc_rw
)

#Plot with the forecasts from all models 
ggplot(plot1, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = Model_1A, color = "Model 1A"), linetype = "dashed") +
  geom_line(aes(y = Model_1B, color = "Model 1B"), linetype = "dotted") +
  geom_line(aes(y = Model_1C, color = "Model 1C"), linetype = "solid") +
  geom_line(aes(y = RWD, color = "Random Walk with Drift"), linetype = "longdash") +
  geom_line(aes(y = RW, color = "Random Walk without Drift"), linetype = "twodash") +
  labs(title = "10-Year Treasury Yield: Actual vs Forecasts",
       x = "Date", y = "Interest rate (%)") +
  scale_color_manual(values = c(
    "Actual" = "black", 
    "Model 1A" = "blue", 
    "Model 1B" = "green", 
    "Model 1C" = "orange", 
    "Random Walk with Drift" = "red", 
    "Random Walk without Drift" = "purple"
  )) +
  theme_minimal() +
  theme(legend.title = element_blank())

#Plot 1: Actual vs Model 1A-1C
ggplot(plot1, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = Model_1A, color = "Model 1A"), size = 0.5, linetype = "dashed") +
  geom_line(aes(y = Model_1B, color = "Model 1B"), size = 0.5, linetype = "dashed") +
  geom_line(aes(y = Model_1C, color = "Model 1C"), size = 0.5, linetype = "dashed") +
  labs(title = "10-Year Treasury Yield: Actual vs Model 1 Forecasts",
       x = "Date", y = "Yield (%)") +
  scale_color_manual(values = c("Actual" = "black",
                                "Model 1A" = "blue",
                                "Model 1B" = "green",
                                "Model 1C" = "orange")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Plot 2: Actual vs Random Walk Models
ggplot(plot1, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = RWD, color = "Random Walk with Drift"), size = 0.5, linetype = "dashed") +
  geom_line(aes(y = RW, color = "Random Walk without Drift"), size = 0.5, linetype = "dashed") +
  labs(title = "10-Year Treasury Yield: Actual vs Random Walk Forecasts",
       x = "Date", y = "Yield (%)") +
  scale_color_manual(values = c("Actual" = "black",
                                "Random Walk with Drift" = "red",
                                "Random Walk without Drift" = "purple")) +
  theme_minimal() +
  theme(legend.title = element_blank())


#Produce excel file 
combined_table <- data.frame(
  Date = dates[(train_end+1):n],
  Actual = y[(train_end+1):n],
  
  Fc_1A = fc_1A[(train_end+1):n],
  Err_1A = errors_1A,
  
  Fc_1B = fc_1B[(train_end+1):n],
  Err_1B = errors_1B,
  
  Fc_1C = fc_1C[(train_end+1):n],
  Err_1C = errors_1C,
  
  RWD_Fc = fc_rwd[(train_end+1):n],
  RWD_Err = errors_rwd,
  
  RW_Fc = fc_rw[(train_end+1):n],
  RW_Err = errors_rw
)
combined_table[,-1] <- round(combined_table[,-1], 5)
write_xlsx(combined_table, "ValenciaKelly_Midterm_supplemental_materials.xlsx")