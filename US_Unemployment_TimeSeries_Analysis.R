
# US Unemployment Rate Time Series Analysis - MA5781 Final Project
# Vineeth Karjala & Cédric Keller

# summaries:
# SARIMA model performs poorly on the 2022–2024 forecast
# Strong monthly seasonality
# Model fails forecasting post-COVID
# manual model ARIMA(3,0,1)(2,1,2)[12] model, auto model SARIMA(1,1,1)(2,0,0)[12]
# both models failed in forecasting
# Residual Plot: Mostly centered around 0 except the 2020 spike.
# ACF of Residuals mostly within bounds.
# Histogram is nearly normal except for the outlier spike.


library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(urca)
library(ggplot2)

# Load data
data <- read.csv("C:\\Users\\vinee\\Downloads\\UNRATENSA.csv")
data$DATE <- as.Date(data$observation_date)
ts_data <- ts(data$UNRATENSA, start = c(1948, 1), frequency = 12)  # Monthly data

# Step 1: Visualize the Time Series
autoplot(ts_data) +
  ggtitle("US Unemployment Rate (1948–2024)") +
  xlab("Year") + ylab("Unemployment Rate (%)")

# Step 2: Seasonality Check
ggseasonplot(ts_data, year.labels = TRUE, continuous = TRUE) +
  ggtitle("Seasonal Plot")

ggsubseriesplot(ts_data) +
  ggtitle("Seasonal Subseries Plot")

# Step 3: Stationarity Testing
Acf(ts_data)
Pacf(ts_data)
adf.test(ts_data)
kpss.test(ts_data)
pp.test(ts_data)

# Step 4: Differencing (if needed)
ts_diff <- diff(ts_data, differences = 1)
ts_seasonal_diff <- diff(ts_data, lag = 12)
autoplot(ts_seasonal_diff)
ts_diff_both <- diff(ts_seasonal_diff)
autoplot(ts_diff_both) +
  ggtitle("Differenced Series (First + Seasonal)")
adf.test(ts_diff_both)
kpss.test(ts_diff_both)
pp.test(ts_diff_both)


# Step 5: Fit ARIMA Model
sarimafit <- auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(sarimafit)
checkresiduals(sarimafit)


# Step 6: Forecasting (Excluding COVID Spike)
train <- window(ts_data, end = c(2019,12))
test <- window(ts_data, start = c(2022,1))
model <- auto.arima(train)
forecast_result <- forecast(model, h = length(test))

autoplot(forecast_result) +
  autolayer(test, series = "Actual", PI = FALSE)

accuracy(forecast_result, test)

# Step 7: Residual Analysis
checkresiduals(model)
Box.test(residuals(model), lag=20, type="Ljung-Box")



#step 8: manual training
model <- Arima(train,
               order = c(3,0,1),
               seasonal = list(order = c(2,1,2), period = 12),
               include.drift = TRUE)
summary(model)

# Forecast for the length of the test set
h <- length(test)
forecast_result <- forecast(model, h = h)

# Plot forecast vs actual
autoplot(forecast_result) +
  autolayer(test, series = "Actual", PI = FALSE) +
  ggtitle("Forecast vs Actual: US Unemployment Rate (2022–2024)") +
  xlab("Year") + ylab("Unemployment Rate (%)")

# Accuracy of the forecast on test set
accuracy(forecast_result, test)

# Residual check (white noise validation)
checkresiduals(model)
Box.test(residuals(model), lag = 20, type = "Ljung-Box")
