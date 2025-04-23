# Time-Series Analysis and Forecasting of the Unemployment Rate in the US

library(forecast)
library(TSA)
library(tseries)

# ##################################################
# environment variables
# ##################################################
cwd = getwd() # cwd = setwd("") # setwd if not in correct working directory
plots.export.path = paste0(cwd, "/plots")

# ##################################################
# exporting all plots present in temp directory (RUN AFTER GENERATING PLOTS)
# ##################################################
#plots.dir.path = list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
#plots.png.paths = list.files(plots.dir.path, pattern=".png", full.names = TRUE)
#file.copy(from=plots.png.paths, to=plots.export.path)
# ##################################################



# ##################################################
# data import and setup
# ##################################################
unrate.df = read.csv("data/UNRATENSA.csv",head=TRUE)

unrate.df$observation_date = as.Date(unrate.df$observation_date, format="%Y-%m-%d")
unrate.df = unrate.df[unrate.df$observation_date < as.Date("2025-01-01"),] # remove data past 2024

head(unrate.df)
summary(unrate.df)

# time series conversion
start.year = as.numeric(format(min(unrate.df$observation_date), "%Y"))
start.month = as.numeric(format(min(unrate.df$observation_date), "%m"))

unrate = ts(unrate.df$UNRATENSA
                ,start=c(start.year, start.month)
                ,frequency = 12)

unrate.2015 = window(unrate, end=c(2015, 12)) # base
unrate.2016.2019 = window(unrate, start=c(2016, 1), end=c(2019, 12)) # prediction
unrate.2016.2024 = window(unrate, start=c(2016, 1)) # prediction (including covid)


unrate.1999.2005 = window(unrate, start=c(1999, 1), end=c(2005, 12))
unrate.2003.2006 = window(unrate, start=c(2003, 1), end=c(2006, 12))



# ##################################################
# Plotting Time Series
# ##################################################
#    full ts
plot(unrate, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 1948-2024")

#    ts pre-covid -- will be used for fitting
plot(unrate.2015, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 1948-2015")

#    seasonal demonstration
plot(unrate.1999.2005, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 1999-2005")
grid()
plot(unrate.2003.2006, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 2003-2006")
grid()
# There is a seasonal pattern within each year (freq could be 12)

# histogram
hist(unrate.2015, xlab="Unemployment Rate (%)", main="Histogram of US Unemployment Rate 1948-2015", col="skyblue")




# ##################################################
# Deterministic Trend (probably not) -> available in deterministic_trends.R file
# ##################################################

# ##############################
# # seasonal means model
# month = season(unrate.2015)
# model.sm = lm(unrate.2015~month)
# summary(model.sm)
# 
# model.sm.resid=ts(resid(model.sm),start=c(start.year, start.month),freq=12)
# plot(model.sm.resid, ylab="Residuals", main="Residuals of Seasonal Means Model\n1948-2015")
# abline(h=mean(model.sm.resid)) # mean is basically zero
# acf(model.sm.resid, lag.max=1000, main="ACF of Seasonal Means\nUS Unemployment Rate 1948-2015") # fast decay
# pacf(model.sm.resid, lag.max=100, main="ACF of Seasonal Means\nUS Unemployment Rate 1948-2015")
# 
# # normal distribution check
# hist(model.sm.resid, xlab="Residuals", main="Histogram of Seasonal Means Residuals", col="skyblue")
# qqnorm(model.sm.resid, main="Q-Q Plot of Seasonal Means Residuals")
# qqline(model.sm.resid)
# # It is not normally distributed (positive skew) -> may require transformation or different model
# #     , but it's not bad
# 
# ##############################
# # cosine trend model
# unrate.2015.harmonic = harmonic(unrate.2015, m=1) # 1/12
# model.cos = lm(unrate.2015~unrate.2015.harmonic)
# summary(model.cos)
# 
# model.cos.resid = resid(model.cos)
# plot(model.cos.resid, type="l", ylab="Residuals", main="Residuals of Cosine Trend Model\n1948-2015")
# abline(h=mean(model.cos.resid)) # mean is basically zero, but may lack homoscedasticity and normal distribution.
# acf(model.cos.resid, lag.max=100, main="ACF of Cosine Trend\nUS Unemployment Rate 1948-2015") # slow decay
# pacf(model.cos.resid, lag.max=100, main="PACF of Cosine Trend\nUS Unemployment Rate 1948-2015")
# 
# #     normal distribution check
# hist(model.cos.resid, xlab="Residuals", main="Histogram of Cosine Trend Residuals", col="skyblue")
# qqnorm(model.cos.resid, main="Q-Q Plot of Cosine Trend Residuals")
# qqline(model.cos.resid)
# # It is not normally distributed
# 
# 
# ##############################
# # polynomial trend
# model.poly = lm(unrate.2015~month+time(unrate.2015))
# 
# model.poly.resid = resid(model.poly)
# plot(model.poly.resid, type="l", ylab="Residuals", main="Residuals of Poly Trend Model\n1948-2015")
# abline(h=mean(model.poly.resid)) # mean is basically zero
# acf(model.poly.resid, lag.max=100, main="ACF of Poly Trend\nUS Unemployment Rate 1948-2015") # slow decay
# pacf(model.poly.resid, lag.max=100, main="PACF of Poly Trend\nUS Unemployment Rate 1948-2015")
# 
# #     normal distribution check
# hist(model.poly.resid, xlab="Residuals", main="Histogram of Poly Trend Residuals", col="skyblue")
# qqnorm(model.poly.resid, main="Q-Q Plot of Poly Trend Residuals")
# qqline(model.poly.resid)
# # It is not normally distributed


# ##################################################
# Seasonal and Stochastic Trend Transformations
# ##################################################
runs(unrate.2015) # fails to run (output: -1)
adf.test(unrate.2015) # fail to reject -> NOT stationary
pp.test(unrate.2015) # fail to reject -> NOT stationary
kpss.test(unrate.2015) # rejected -> NOT stationary
# stochastic trend likely -> transformation is necessary

##############################
# first difference (d=1)
diff.2015 = diff(unrate.2015)
# diff.2015 = resid(Arima(unrate.2015, order=c(0,1,0))) # same as above

plot(diff.2015, type="l", ylab="Differenced Unemployment Rate", main="Differenced\nUS Unemployment Rate 1948-2015")
abline(h=mean(diff.2015)) # mean is basically zero, looks homoscedastic
acf(diff.2015, lag.max=1000, main="ACF of Differenced\nUS Unemployment Rate 1948-2015") # slow decay
acf(diff.2015, lag.max=100, main="ACF of Differenced\nUS Unemployment Rate 1948-2015") # seasonal
pacf(diff.2015, lag.max=100, main="PACF of Differenced\nUS Unemployment Rate 1948-2015")

#     normal distribution check
hist(diff.2015, xlab="Residuals", main="Histogram of First Difference Residuals", col="skyblue")
qqnorm(diff.2015, main="Q-Q Plot of First Difference Residuals")
qqline(diff.2015)


##############################
# seasonal differencing
s.diff.2015 = diff(unrate.2015,lag=12)
# s.diff.2015 = resid(Arima(unrate.2015, seasonal=c(0,1,0))) # same as above

#     residuals (zero mean and homoscedasticity)
plot(s.diff.2015, type="l", ylab="Seasonally Differenced Unemployment Rate", main="Seasonally Differenced\nUS Unemployment Rate 1948-2015")
abline(h=mean(s.diff.2015)) # mean is basically zero, may not be homoscedastic
acf(s.diff.2015, lag.max=1000, main="ACF of Seasonally Differenced\nUS Unemployment Rate 1948-2015") # fast decay & better than seasonal means
pacf(s.diff.2015, lag.max=100, main="PACF of Seasonally Differenced\nUS Unemployment Rate 1948-2015")

#     normal distribution check
hist(s.diff.2015, xlab="Residuals", main="Histogram of Seasonal Difference Residuals", col="skyblue")
qqnorm(s.diff.2015, main="Q-Q Plot of Seasonal Difference Residuals")
qqline(s.diff.2015)
# not normal (skewed right + high kurtosis + heavy tail) - stochastic trend? / may be acceptable for SARIMA
#checkresiduals(s.diff.2015)
shapiro.test(s.diff.2015) # violated p-value = < 2.2e-16

#     independence
runs(s.diff.2015) # rejected (p<0.05) -> NOT independent

#     stationarity
adf.test(s.diff.2015) # rejected -> stationary
pp.test(s.diff.2015) # rejected -> stationary
kpss.test(s.diff.2015) # fail to reject -> stationary


##############################
# first + seasonal difference (d=1, D=1)
diff2.2015 = diff(diff(unrate.2015),lag=12)
# diff2.2015 = resid(Arima(unrate.2015, order=c(0,1,0), seasonal=c(0,1,0))) # same as above

#     residuals (zero mean and homoscedasticity)
plot(diff2.2015, type="l", ylab="Double Differenced Unemployment Rate", main="Double Differenced\nUS Unemployment Rate 1948-2015")
abline(h=mean(diff2.2015)) # mean is basically zero, looks homoscedastic
# looks like noise -> may be overdifferenced
acf(diff2.2015, lag.max=1000, main="ACF of First & Seasonal Difference\nof US Unemployment Rate 1948-2015") # extremely fast decay (after 1 lag)
pacf(diff2.2015, lag.max=100, main="PACF of Seasonally Differenced\nUS Unemployment Rate 1948-2015")

#     normal distribution check
hist(diff2.2015, xlab="Residuals", main="Histogram of First & Seasonal\nDifference Residuals", col="skyblue") # heavy tails
qqnorm(diff2.2015, main="Q-Q Plot of First & Seasonal\nDifference Residuals") # heavy tails
qqline(diff2.2015)
#checkresiduals(diff2.2015) # looks pretty good here
shapiro.test(diff2.2015) # violated p-value = 1.067e-10

#     independence
runs(diff2.2015) # fails to reject (p>0.05) -> independent

#     stationarity
adf.test(diff2.2015) # rejected -> stationary
pp.test(diff2.2015) # rejected -> stationary
kpss.test(diff2.2015) # rejected -> NOT stationary




# ##################################################
# Choosing Model parameters
# ##################################################

##############################
# original data (clipped)
plot(unrate.2015)
# p & q
auto.arima(unrate.2015) # SARIMA(3,0,1)(2,1,2)[12]

##############################
# seasonal difference (D=1)
plot(s.diff.2015)

# p & q
auto.arima(unrate.2015,
           D=1) # SARIMA(3,0,1)(2,1,2)[12]
acf(s.diff.2015,lag.max=80, main="ACF of Seasonal Difference\nof US Unemployment Rate 1948-2015")
# MA(1)
pacf(s.diff.2015,lag.max=80, main="PACF of Seasonal Difference\nof US Unemployment Rate 1948-2015")
# AR(3)
eacf(s.diff.2015)
# ARMA(3,1)

# P & Q
acf(s.diff.2015,lag.max=500, main="ACF of Seasonal Difference\nof US Unemployment Rate 1948-2015")
# MA(2)?
pacf(s.diff.2015,lag.max=300, main="PACF of Seasonal Difference\nof US Unemployment Rate 1948-2015")
# AR(0)/AR(3)?
#   (P=0, Q=2 performed the best)

##############################
# first + seasonal difference (d=1, D=1)
plot(diff2.2015)

# p & q
auto.arima(unrate.2015,
           d=1, D=1) # SARIMA(2,1,1)(2,1,2)[12]
acf(diff2.2015,lag.max=80, main="ACF of First & Seasonal Difference\nof US Unemployment Rate 1948-2015")
# MA(1)
pacf(diff2.2015,lag.max=80, main="PACF of First & Seasonal Difference\nof US Unemployment Rate 1948-2015")
# AR(3)
eacf(diff2.2015)
# ARMA(1,2), ARMA(2,2), maybe ARMA(4,2)

# P & Q
acf(diff2.2015,lag.max=700, main="ACF of First & Seasonal Difference\nof US Unemployment Rate 1948-2015")
# MA(0)/MA(1)/MA(2)/MA(3)?
pacf(diff2.2015,lag.max=700, main="PACF of First & Seasonal Difference\nof US Unemployment Rate 1948-2015")
# AR(0)/AR(3)?
#   (P=2, Q=1 performed the best)




# ##################################################
# Candidate Models
# ##################################################
# can't compare AIC/BIC for models with different d or D

##############################
# original data
o.auto =     Arima(unrate.2015,order=c(3,0,1),seasonal=c(2,1,2),include.mean=F)

##############################
# seasonal difference (D=1)
sd.auto =    Arima(unrate.2015,order=c(3,0,1),seasonal=c(2,1,2),include.mean=F)
sd.PQ =      Arima(unrate.2015,order=c(3,0,1),seasonal=c(0,1,2),include.mean=F)
sd.ma1 =     Arima(unrate.2015,order=c(0,0,1),seasonal=c(2,1,2),include.mean=F)
sd.ar3 =     Arima(unrate.2015,order=c(3,0,0),seasonal=c(2,1,2),include.mean=F)
sd.arma31 =  Arima(unrate.2015,order=c(3,0,1),seasonal=c(2,1,2),include.mean=F)

##############################
# first + seasonal difference (d=1, D=1)
fsd.auto =   Arima(unrate.2015,order=c(2,1,1),seasonal=c(3,1,3),include.mean=F)
fsd.PQ =     Arima(unrate.2015,order=c(2,1,1),seasonal=c(2,1,1),include.mean=F)
fsd.ma1 =    Arima(unrate.2015,order=c(0,1,1),seasonal=c(2,1,2),include.mean=F)
fsd.ar3 =    Arima(unrate.2015,order=c(3,1,0),seasonal=c(2,1,2),include.mean=F)
fsd.arma12 = Arima(unrate.2015,order=c(1,1,2),seasonal=c(2,1,2),include.mean=F)
fsd.arma22 = Arima(unrate.2015,order=c(2,1,2),seasonal=c(2,1,2),include.mean=F)
fsd.arma42 = Arima(unrate.2015,order=c(4,1,2),seasonal=c(2,1,2),include.mean=F)


##############################
# models with D=1
o.auto
sd.auto
sd.PQ # best
sd.ma1
sd.ar3
sd.arma31

D1.best = sd.PQ

##############################
# models with d=1 & D=1
fsd.auto
fsd.PQ # best
fsd.ma1
fsd.ar3
fsd.arma12
fsd.arma22
fsd.arma42

d1D1.best = fsd.PQ




# ##################################################
# Checking Assumptions (Residuals)
#   1) Zero Mean (Residuals Plot)
#   2) Homoscedasticity (Residuals Plot Variance)
#   3) Normally distributed (Histogram, Q-Q Plot, Shapiro-Wilk Test)
#   4) Independence (Ljung-Box, Runs)
# Checking Stationarity (ADF, PP, KPSS)
# ##################################################

##############################
# best model with D=1
tsdiag(D1.best) # zero mean, homoscedastic, looks fairly normal, is independent
D1.best.resid = resid(D1.best)

#     residuals (zero mean and homoscedasticity)
plot(D1.best.resid, type="l", ylab="Residuals", main="SARIMA(3,0,1)(0,1,2)[12] Residuals\nUS Unemployment Rate 1948-2015")
abline(h=mean(D1.best.resid)) # mean is basically zero, looks homoscedastic (not necessarily equal, but decreasing variance)
acf(D1.best.resid, lag.max=1000, main="ACF of SARIMA(3,0,1)(0,1,2)[12] Residuals\nof US Unemployment Rate 1948-2015")
pacf(D1.best.resid, lag.max=1000, main="PACF of SARIMA(3,0,1)(0,1,2)[12] Residuals\nUS Unemployment Rate 1948-2015")
# all significant lags are very close to the border in ACF/PACF

#     normal distribution check
hist(D1.best.resid, xlab="Residuals", main="Histogram of SARIMA(3,0,1)(0,1,2)[12] Residuals", col="skyblue")
# looks normal, a few outliers
qqnorm(D1.best.resid, main="Q-Q Plot of SARIMA(3,0,1)(0,1,2)[12] Residuals")
qqline(D1.best.resid, col=2)
shapiro.test(D1.best.resid) # normality is violated (p-value<0.05)

# independence
runs(D1.best.resid) # fail to reject (p>0.05) -> independent

#     stationarity
adf.test(D1.best.resid) # rejected -> stationary
pp.test(D1.best.resid) # rejected -> stationary
kpss.test(D1.best.resid) # fail to reject -> stationary


##############################
# best model with d=1 & D=1
tsdiag(d1D1.best) # zero mean, homoscedastic, looks fairly normal, is independent
d1D1.best.resid = resid(d1D1.best)

#     residuals (zero mean and homoscedasticity)
plot(d1D1.best.resid, type="l", ylab="Residuals", main="SARIMA(2,1,1)(2,1,1)[12] Residuals\nUS Unemployment Rate 1948-2015")
abline(h=mean(d1D1.best.resid)) # mean is basically zero, looks homoscedastic (not necessarily equal, but decreasing variance)
acf(d1D1.best.resid, lag.max=1000, main="ACF of SARIMA(2,1,1)(2,1,1)[12] Residuals\nof US Unemployment Rate 1948-2015")
pacf(d1D1.best.resid, lag.max=1000, main="PACF of SARIMA(2,1,1)(2,1,1)[12] Residuals\nUS Unemployment Rate 1948-2015")
# all significant lags are very close to the border in ACF/PACF

#     normal distribution check
hist(d1D1.best.resid, xlab="Residuals", main="Histogram of SARIMA(2,1,1)(2,1,1)[12] Residuals", col="skyblue")
# looks normal, a few outliers
qqnorm(d1D1.best.resid, main="Q-Q Plot of SARIMA(2,1,1)(2,1,1)[12] Residuals")
qqline(d1D1.best.resid, col=2)
shapiro.test(d1D1.best.resid) # normality is violated (p-value<0.05)

# independence
runs(d1D1.best.resid) # fail to reject (p>0.05) -> independent

#     stationarity
adf.test(d1D1.best.resid) # rejected -> stationary
pp.test(d1D1.best.resid) # rejected -> stationary
kpss.test(d1D1.best.resid) # fail to reject -> stationary




# ##################################################
# Fitting to data
# ##################################################
# best model with D=1
plot(unrate.2015, ylab="Unemployment Rate (%)", main="Unemployment Rate 1948-2016 with\nfitted SARIMA(3,0,1)(0,1,2)[12] model")
lines(fitted(D1.best),col=2,lty=2)

# best model with d=1 & D=1
plot(unrate.2015, ylab="Unemployment Rate (%)", main="Unemployment Rate 1948-2016 with\nfitted SARIMA(2,1,1)(2,1,1)[12] model")
lines(fitted(d1D1.best),col=2,lty=2)

# Mean Squared Error Comparison
mse_fit.D1best = mean((unrate.2015 - fitted(D1.best))^2, na.rm = TRUE)
mse_fit.d1D1best = mean((unrate.2015 - fitted(d1D1.best))^2, na.rm = TRUE)


# ##################################################
# Forecasting & Error
# ##################################################
# forecast horizon
ts_future = unrate.2016.2024
h = length(ts_future)
short_h = 48 # short horizon (months)


##############################
# best model with D=1
pred.D1best = predict(D1.best, n.ahead=h)
pr.D1 = pred.D1best$pred
uci.D1 = pr.D1 + 2 * pred.D1best$se
lci.D1 = pr.D1 - 2 * pred.D1best$se

pr.D1.ts = ts(pr.D1, start=start(ts_future), freq=frequency(ts_future))
uci.D1.ts = ts(uci.D1, start=start(ts_future), freq=frequency(ts_future))
lci.D1.ts = ts(lci.D1, start=start(ts_future), freq=frequency(ts_future))

plot(unrate.2015, xlim=c(2000, 2024), ylim=c(0, 15),
     main="SARIMA(3,0,1)(0,1,2)[12] Forecast", ylab="Unemployment Rate (%)")
lines(unrate.2016.2024, lty=3)
lines(pr.D1.ts, col=2)
lines(uci.D1.ts, col=3)
lines(lci.D1.ts, col=3)
abline(h=0)

mse_full.D1best = mean((ts_future - pr.D1)^2, na.rm=TRUE)
mse_short.D1best = mean((ts_future[1:short_h] - pr.D1[1:short_h])^2, na.rm=TRUE)


##############################
# best model with d=1 & D=1
pred.d1D1best = predict(d1D1.best, n.ahead=h)
pr.d1D1 = pred.d1D1best$pred
uci.d1D1 = pr.d1D1 + 2 * pred.d1D1best$se
lci.d1D1 = pr.d1D1 - 2 * pred.d1D1best$se

pr.d1D1.ts = ts(pr.d1D1, start=start(ts_future), freq=frequency(ts_future))
uci.d1D1.ts = ts(uci.d1D1, start=start(ts_future), freq=frequency(ts_future))
lci.d1D1.ts = ts(lci.d1D1, start=start(ts_future), freq=frequency(ts_future))

plot(unrate.2015, xlim=c(2000, 2024), ylim=c(0, 15),
     main="SARIMA(2,1,1)(2,1,1)[12] Forecast", ylab="Unemployment Rate (%)")
lines(unrate.2016.2024, lty=3)
lines(pr.d1D1.ts, col=2)
lines(uci.d1D1.ts, col=3)
lines(lci.d1D1.ts, col=3)
abline(h=0)

mse_full.d1D1best = mean((ts_future - pr.d1D1)^2, na.rm=TRUE)
mse_short.d1D1best = mean((ts_future[1:short_h] - pr.d1D1[1:short_h])^2, na.rm=TRUE)

##############################
# Mean Squared Error Comparison
mse_df = data.frame(
  Model = c(#'Seasonal Means ARIMA(2,0,1)',
            'SARIMA(3,0,1)(0,1,2)[12]',
            'SARIMA(2,1,1)(2,1,1)[12]'),
  MSE_fit = round(c(#mse_fir.sm, 
    mse_fit.D1best, mse_fit.d1D1best), 3),
  MSE_full = round(c(#mse_full.sm, 
    mse_full.D1best, mse_full.d1D1best), 3),
  MSE_n_months = round(c(#mse_short.sm,
    mse_short.D1best, mse_short.d1D1best), 3)
)

paste0("n_months: ", short_h)
mse_df

