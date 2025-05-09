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
library(sandwich)
library(lmtest)




# Load data
data <- read.csv("C:\\Users\\vinee\\Downloads\\UNRATENSA.csv", head=TRUE)
data$observation_date = as.Date(data$observation_date, format="%Y-%m-%d")
data <- data[data$observation_date < as.Date("2025-01-01"),] # remove data past 2024
head(data)
summary(data)

start.year = as.numeric(format(min(data$observation_date), "%Y"))
start.month = as.numeric(format(min(data$observation_date), "%m"))
ts_data <- ts(data$UNRATENSA
            ,start=c(start.year, start.month)
            ,frequency = 12)
unrate.2019 = window(ts_data, end=c(2019, 12))
unrate.1999.2005 = window(ts_data, start=c(1999, 1), end=c(2005, 12))
unrate.2003.2006 = window(ts_data, start=c(2003, 1), end=c(2006, 12))
plot(ts_data, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 1948-2024")

data$DATE <- as.Date(data$observation_date)
ts_data <- ts(data$UNRATENSA, start = c(1948, 1), frequency = 12)# Monthly data
ts_data <- window(ts_data, end = c(2019, 12))
ts_sm_train <- window(ts_data, end = c(2015, 12))
ts_sm_test <- window(ts_data, start = c(2016, 1), end = c(2019, 12))


# Step 1: Visualize the Time Series
autoplot(ts_data) +
  ggtitle("US Unemployment Rate (1948–2019)") +
  xlab("Year") + ylab("Unemployment Rate (%)")

# Step 2: Seasonality Check
ggseasonplot(ts_data, year.labels = TRUE, continuous = TRUE) +
  ggtitle("Seasonal Plot")
ggseasonplot(ts_data, month.labels = TRUE, continuous = TRUE) +
  ggtitle("Seasonal Plot")


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
autoplot(ts_diff)
ts_diff_both <- diff(ts_seasonal_diff)
autoplot(ts_diff_both) +
  ggtitle("Differenced Series (First + Seasonal)")
adf.test(ts_diff_both)
kpss.test(ts_diff_both)
pp.test(ts_diff_both)

adf.test(ts_diff)
kpss.test(ts_diff)
pp.test(ts_diff)

adf.test(ts_seasonal_diff)
kpss.test(ts_seasonal_diff)
pp.test(ts_seasonal_diff)
Acf(ts_diff, main = "ACF: First Difference of Data")
Pacf(ts_diff)

# Convert ts_diff (a 'ts' object) to a data.frame with proper dates
diff_dates <- seq(from = as.Date("1948-02-01"),  # one month after start
                  by = "month", 
                  length.out = length(ts_diff))
ts_diff_df <- data.frame(
  DATE = diff_dates,
  VALUE = as.numeric(ts_diff)
)
ts_diff_df$year <- as.numeric(format(ts_diff_df$DATE, "%Y"))
ts_diff_df$month <- as.numeric(format(ts_diff_df$DATE, "%m"))
ts_diff_df$t <- 1:nrow(ts_diff_df)
model_seasonal <- lm(VALUE ~ month, data = ts_diff_df)
summary(model_seasonal)
res_model_seasonal <- residuals(model_seasonal)
adf.test(res_model_seasonal)
pp.test(res_model_seasonal)
kpss.test(res_model_seasonal)
shapiro.test(res_model_seasonal)
coeftest(model_seasonal, vcov = NeweyWest(model_seasonal))
qqnorm(res_model_seasonal, main = "Q-Q Plot of Box-Cox Model Residuals")
qqline(res_model_seasonal, col = "red", lwd = 2)
hist(res_model_seasonal,
     breaks = 10,
     main = "Histogram of Box-Cox Model Residuals",
     xlab = "Residuals",
     col = "skyblue",
     border = "white")




n <- nrow(ts_diff_df)
t <- ts_diff_df$t
cos_term <- cos(2 * pi * t / 12)
sin_term <- sin(2 * pi * t / 12)
model_cosine <- lm(VALUE ~ cos_term + sin_term, data = ts_diff_df)
summary(model_cosine)
res_cosine <- residuals(model_cosine)
adf.test(res_cosine)
pp.test(res_cosine)
kpss.test(res_cosine)
shapiro.test(res_cosine)
coeftest(model_cosine, vcov = NeweyWest(model_cosine))
qqnorm(res_cosine, main = "Q-Q Plot of Cosine Model Residuals")
qqline(res_cosine, col = "red", lwd = 2)
hist(res_cosine,
     breaks = 10,
     main = "Histogram of Cosine Model Residuals",
     xlab = "Residuals",
     col = "skyblue")




model_poly <- lm(VALUE ~ poly(t, 2), data = ts_diff_df)  # Quadratic trend
summary(model_poly)
res_model_poly <- residuals(model_poly)
adf.test(res_model_poly)
pp.test(res_model_poly)
kpss.test(res_model_poly)
shapiro.test(res_model_poly)
coeftest(model_poly, vcov = NeweyWest(model_poly))
qqnorm(res_model_poly, main = "Q-Q Plot of Quadratic Model Residuals")
qqline(res_model_poly, col = "red", lwd = 2)
hist(res_model_poly,
     breaks = 10,
     main = "Histogram of Box-Cox Model Residuals",
     xlab = "Residuals",
     col = "skyblue")




# Boxcox
ts_diff_df$VALUE_shifted <- ts_diff_df$VALUE + abs(min(ts_diff_df$VALUE)) + 1
lambda_bc <- BoxCox.lambda(ts_diff_df$VALUE_shifted)
ts_diff_df$VALUE_boxcox <- BoxCox(ts_diff_df$VALUE_shifted, lambda_bc)
model_boxcox <- lm(VALUE_boxcox ~ poly(t, 2), data = ts_diff_df)
summary(model_boxcox)
res_boxcox <- residuals(model_boxcox)
adf.test(res_boxcox)
pp.test(res_boxcox)
kpss.test(res_boxcox)
shapiro.test(res_boxcox)
coeftest(model_boxcox, vcov = NeweyWest(model_boxcox))
qqnorm(res_boxcox, main = "Q-Q Plot of Box-Cox Model Residuals")
qqline(res_boxcox, col = "red", lwd = 2)
# Histogram of residuals
hist(res_boxcox,
     breaks = 10,
     main = "Histogram of Box-Cox Model Residuals",
     xlab = "Residuals",
     col = "skyblue",
     border = "white")



# Sqrt
ts_diff_df$VALUE_sqrt <- sqrt(ts_diff_df$VALUE_shifted)
model_sqrt <- lm(VALUE_sqrt ~ poly(t, 2), data = ts_diff_df)
summary(model_sqrt)
res_sqrt <- residuals(model_sqrt)
adf.test(res_sqrt)
pp.test(res_sqrt)
kpss.test(res_sqrt)
shapiro.test(res_sqrt)
coeftest(model_sqrt, vcov = NeweyWest(model_sqrt))
qqnorm(res_sqrt, main = "Q-Q Plot of sqrt Model Residuals")
qqline(res_sqrt, col = "red", lwd = 2)
hist(res_sqrt,
     breaks = 10,
     main = "Histogram of sqrt Model Residuals",
     xlab = "Residuals",
     col = "skyblue")



#log - producing NaNs
ts_diff_df$UNRATENSA_log <- log(ts_diff_df$VALUE)
model_poly_log <- lm(UNRATENSA_log ~ poly(t, 2), data = ts_df_filtered)
model_cubic <- lm(UNRATENSA ~ poly(t, 3), data = ts_df_filtered)
summary(model_cubic)
res <- residuals(model_cubic)
adf.test(res)
pp.test(res)
kpss.test(res)
shapiro.test(res)
coeftest(model_poly, vcov = NeweyWest(model_poly))



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
