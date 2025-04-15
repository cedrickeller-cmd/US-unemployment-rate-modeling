# Time-Series Analysis and Forecasting of Unemployment Rate in the US

library(forecast)
library(TSA)
library(tseries)
#require(lmtest)
#library(sandwich)

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
hist(unrate.2015)




# ##################################################
# Seasonal Decomposition (STL) <- not too accurate
# ##################################################
#sdecomp = stl(unrate, s.window="periodic")
#plot(sdecomp)
#sdecomp.2015 = stl(unrate.2015, s.window="periodic")
#plot(sdecomp.2015)




# ##################################################
# Deterministinc Trend (probably not)
# ##################################################
# seasonal means model
month = season(unrate.2015)
model.sm = lm(unrate.2015~month)
summary(model.sm)

model.sm.resid=ts(resid(model.sm),start=c(start.year, start.month),freq=12)
plot(model.sm.resid, main="Residuals of seasonal means model\n1948-2015")
abline(h=mean(model.sm.resid)) # mean is basically zero

# normal distribution check
hist(model.sm.resid)
qqnorm(model.sm.resid)
qqline(model.sm.resid)
# It is not normally distributed (positive skew) -> requires transformation or different model


# # cosine trend model
# unrate.2015.harmonic = harmonic(unrate.2015, m=1) # 1/12
# model.cos = lm(unrate.2015~unrate.2015.harmonic)
# summary(model.cos)
# 
# model.cos.resid = resid(model.cos)
# plot(model.cos.resid, type="l", main="Residuals of cosine trend model\n1948-2015")
# abline(h=mean(model.cos.resid)) # mean is basically zero
# 
# #     normal distribution check
# hist(model.cos.resid)
# qqnorm(model.cos.resid)
# qqline(model.cos.resid)
# # It is not normally distributed
# 
# #     fitting model
# #plot(unrate.2015)
# #model.cos.f = ts(fitted(model.cos)
# #                  ,start = start(unrate.2015)
# #                  )#,freq = frequency(unrate.2015)) # 12
# #lines(model.cos.f,col=2,lty=2)
# 
# plot(model.cos.resid)
# abline(h=0,col=2)
# 
# # Both model's residuals have zero mean,
# #   but they may lack homoscedasticity and normal distribution.


# # polynomial trend
# model.poly = lm(unrate.2015~month+time(unrate.2015))
# 
# model.poly.resid = resid(model.poly)
# plot(model.poly.resid, type="l", main="Residuals of poly trend model\n1948-2015")
# abline(h=mean(model.poly.resid)) # mean is basically zero
# 
# #     normal distribution check
# hist(model.poly.resid)
# qqnorm(model.poly.resid)
# qqline(model.poly.resid)
# # It is not normally distributed
# 
# #plot(unrate.2015)
# #model.poly.f = ts(fitted(model.poly)
# #                  ,start = start(unrate.2015)
# #                  )#,freq = frequency(unrate.2015)) # 12
# #lines(model.poly.f,col=2,lty=2)
# 
# plot(rstandard(model.poly))
# abline(h=0,col=2)
# acf(rstandard(model.poly))


# ##################################################
# Seasonal and Stochastic Trend Transformations
# ##################################################
runs(unrate.2015) # fails to run (output: -1)
adf.test(unrate.2015) # fail to reject -> NOT stationary
pp.test(unrate.2015) # fail to reject -> NOT stationary
kpss.test(unrate.2015) # rejected -> NOT stationary
# stochastic trend likely -> transformation is necessary

##############################
# first difference with seasonal means model
unrate.2015.diff = diff(unrate.2015) # differencing before seasonal means
month.diff = season(unrate.2015)[-1] # excluding first observation because of differencing

diff.sm.2015.lm = lm(unrate.2015.diff~month.diff)
diff.sm.2015 = ts(resid(diff.sm.2015.lm), start=c(start.year, start.month+1), freq = 12)

plot(diff.sm.2015, main="Residuals of Seasonal Means Model on Differenced Series")
abline(h=mean(diff.sm.2015))

acf(diff.sm.2015, lag.max=500) # a bit hard to tell, many significant higher lags
pacf(diff.sm.2015, lag.max=100)

#     stationarity
runs(diff.sm.2015) # p>0.05 -> supports stationarity
adf.test(diff.sm.2015) # rejected -> stationary
pp.test(diff.sm.2015) # rejected -> stationary
kpss.test(diff.sm.2015) # rejected -> NOT stationary

#     normal distribution check
hist(diff.sm.2015)
qqnorm(diff.sm.2015)
qqline(diff.sm.2015)
# looks much better than before
#checkresiduals(diff.sm.2015) # looks pretty good here

shapiro.test(diff.sm.2015) # violated p-value = < 2.512e-10

##############################
# seasonal differencing
s.diff.2015 = diff(unrate.2015,lag=12)
plot(s.diff.2015, type="l", ylab="Seasonally Differenced Unemployment Rate", main="Seasonally Differenced\nUS Unemployment Rate 1948-2015")
abline(h=mean(s.diff.2015)) # mean is basically zero

acf(s.diff.2015, max.lag=250)
pacf(s.diff.2015, lag.max=100)

#     stationarity
runs(s.diff.2015) # p<0.05 -> supports stationarity
adf.test(s.diff.2015) # rejected -> stationary
pp.test(s.diff.2015) # rejected -> stationary
kpss.test(s.diff.2015) # fail to reject -> stationary

#     normal distribution check
hist(s.diff.2015)
qqnorm(s.diff.2015)
qqline(s.diff.2015)
# not normal (skewed right + high kurtosis + heavy tail) - stochastic trend? / may be acceptable for SARIMA
#checkresiduals(s.diff.2015)
shapiro.test(s.diff.2015) # violated p-value = < 2.2e-16


##############################
# first + seasonal difference (d=1, D=1)
diff2.2015 = diff(diff(unrate.2015),lag=12)
# diff2.2015 = resid(Arima(unrate.2015, order=c(0,1,0), seasonal=c(0,1,0))) # same as above

#     residuals (zero mean and homoscedasticity)
plot(diff2.2015, type="l", ylab="Double Differenced Unemployment Rate", main="Double Differenced\nUS Unemployment Rate 1948-2015")
abline(h=mean(diff2.2015)) # mean is basically zero
# looks like noise -> may be overdifferenced

#     normal distribution check
hist(diff2.2015) # heavy tails
qqnorm(diff2.2015) # heavy tails
qqline(diff2.2015)
#checkresiduals(diff2.2015) # looks pretty good here
shapiro.test(diff2.2015) # violated p-value = 1.067e-10

#     runs test and ACF plot (independence)
runs(diff2.2015)
acf(diff2.2015)

#     Stationarity
runs(diff2.2015) # p>0.05 -> supports stationarity
adf.test(diff2.2015) # rejected -> stationary
pp.test(diff2.2015) # rejected -> stationary
kpss.test(diff2.2015) # rejected -> NOT stationary


##############################
# # first difference (d=1)
# diff.2015 = diff(unrate.2015)
# plot(diff.2015, type="l", ylab="Differenced Unemployment Rate", main="Differenced\nUS Unemployment Rate 1948-2015")
# 
# model.diff = Arima(unrate.2015, order=c(0,1,0))
# diff.resid = resid(model.diff)
# plot(diff.resid, type="l", main="Residuals of first difference\n1948-2015")
# abline(h=mean(diff.resid)) # mean is basically zero
# 
# #     normal distribution check
# hist(diff.resid)
# qqnorm(diff.resid)
# qqline(diff.resid)




# ##################################################
# Choosing Model parameters
# ##################################################

##############################
# original data
plot(unrate.2015)
# p & q
auto.arima(unrate.2015) # SARIMA(3,0,1)(2,1,2)[12]

##############################
# seasonal means + diff (d=1)
plot(diff.sm.2015)
# p & q
auto.arima(diff.sm.2015,
           d=0, # differenced manually
           D=0, # seasonality removed with seasonal means
           seasonal=F) # ARIMA(2,0,1)
acf(diff.sm.2015,lag.max=300) # MA(1), maybe MA(2)
pacf(diff.sm.2015,lag.max=80) # AR(2), maybe AR(3)/AR(4)
eacf(diff.sm.2015) # AR(3), ARMA(1,2)

##############################
# seasonal difference (D=1)
plot(s.diff.2015)
# p & q
auto.arima(unrate.2015,
           D=1) # SARIMA(3,0,1)(2,1,2)[12]
acf(s.diff.2015,lag.max=80) # MA(1)
pacf(s.diff.2015,lag.max=80) # AR(3)
eacf(s.diff.2015) # ARMA(3,1)
# P & Q
acf(s.diff.2015,lag.max=500) # MA(2)?
pacf(s.diff.2015,lag.max=300) # AR(0)/AR(3)?
#   (P=0, Q=2 performed the best)

##############################
# first + seasonal difference (d=1, D=1)
plot(diff2.2015)
# p & q
auto.arima(unrate.2015,
           d=1, D=1) # SARIMA(2,1,1)(2,1,2)[12]
acf(diff2.2015,lag.max=80) # MA(1)
pacf(diff2.2015,lag.max=80) # AR(3)
eacf(diff2.2015) # ARMA(1,2), ARMA(2,2), maybe ARMA(4,2)
# P & Q
acf(diff2.2015,lag.max=700) # MA(0)/MA(1)/MA(2)/MA(3)?
pacf(diff2.2015,lag.max=700) # AR(0)/AR(3)?
#   (P=2, Q=1 performed the best)




# ##################################################
# Candidate Models
# ##################################################
# can't compare AIC/BIC for models with different d or D

##############################
# original data
o.auto =     Arima(unrate.2015,order=c(3,0,1),seasonal=c(2,1,2),include.mean=F)

##############################
# seasonal means + diff (d=1)
smd.auto =   Arima(diff.sm.2015,order=c(2,0,1),include.mean=F)
smd.ma1 =    Arima(diff.sm.2015,order=c(0,0,1),include.mean=F)
smd.ar2 =    Arima(diff.sm.2015,order=c(2,0,0),include.mean=F)
smd.ar3 =    Arima(diff.sm.2015,order=c(3,0,0),include.mean=F)
smd.arma12 = Arima(diff.sm.2015,order=c(1,0,2),include.mean=F)

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
# models with d=1
smd.auto # best
smd.ma1
smd.ar2 # slightly worse AIC/AICc, but better BIC
smd.ar3 # very close to auto
smd.arma12

d1sm.best = smd.auto

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
#   1) Zero Mean
#   2) Homoscedasticity
#   3) Normally distributed (Histogram, QQ-plot)
#   4) Independence (Ljung-Box)
# ##################################################

##############################
# best model with d=1 (+ seasonal means)
tsdiag(d1sm.best) # zero mean, homoscedastic, looks fairly normal, may not be independent (very close)
hist(resid(d1sm.best)) # a bit skewed
qqnorm(resid(d1sm.best))
qqline(resid(d1sm.best))
shapiro.test(resid(d1sm.best)) # normality is violated (p-value<0.05)
#     stationarity
runs(resid(d1sm.best)) # p>0.05 -> stationarity NOT supported
adf.test(resid(d1sm.best)) # rejected -> stationary
pp.test(resid(d1sm.best)) # rejected -> stationary
kpss.test(resid(d1sm.best)) # fail to reject -> stationary

##############################
# best model with D=1
tsdiag(D1.best) # zero mean, homoscedastic, looks fairly normal, is independent
hist(resid(D1.best)) # looks normal, a few outliers
qqnorm(resid(D1.best))
qqline(resid(D1.best))
shapiro.test(resid(D1.best)) # normality is violated (p-value<0.05)
#     stationarity
runs(resid(D1.best)) # p>0.05 -> stationarity NOT supported
adf.test(resid(D1.best)) # rejected -> stationary
pp.test(resid(D1.best)) # rejected -> stationary
kpss.test(resid(D1.best)) # fail to reject -> stationary

##############################
# best model with d=1 & D=1
tsdiag(d1D1.best) # zero mean, homoscedastic, looks fairly normal, is independent
hist(resid(d1D1.best)) # looks normal, a few outliers
qqnorm(resid(d1D1.best))
qqline(resid(d1D1.best))
shapiro.test(resid(d1D1.best)) # normality is violated (p-value<0.05)
#     stationarity
runs(resid(d1D1.best)) # p>0.05 -> stationarity NOT supported
adf.test(resid(d1D1.best)) # rejected -> stationary
pp.test(resid(d1D1.best)) # rejected -> stationary
kpss.test(resid(d1D1.best)) # fail to reject -> stationary




# ##################################################
# Fitting
# ##################################################
# best model with d=1 (+ seasonal means) # TODO: This looks wrong, doesn't fit the data
plot(unrate.2015)
d1sm.best.f = fitted(d1sm.best)
sm = coef(model.sm)[1] + coef(model.sm)[-1][month] # seasonal means for each month
sm = sm[-length(sm)]  # remove the last value
model.sm.f = ts(d1sm.best.f + sm, start = c(start.year, start.month), freq=12)
lines(model.sm.f,col=2,lty=2)

# best model with D=1
plot(unrate.2015)
lines(fitted(sd.PQ),col=2,lty=2)

# best model with d=1 & D=1
plot(unrate.2015)
lines(fitted(fsd.PQ),col=2,lty=2)




# ##################################################
# Forecasting & Error
# ##################################################
# models to compare
models = list(
  "Seasonal Means ARIMA(2,0,1)" = d1sm.best,
  "SARIMA(3,0,1)(0,1,2)[12]" = sd.PQ,
  "SARIMA(2,1,1)(2,1,1)[12]" = fsd.PQ
)

# forecast horizon
ts_future = unrate.2016.2024
h = length(ts_future)

# short horizon (months)
short_h = 48

# months and newdata for seasonal means
months.future = rep(levels(month), length.out=h)
newdata = data.frame(month=factor(months.future, levels=levels(month)))

# MSE results
mse_full = numeric(length(models))
mse_short = numeric(length(models))
names(mse_full) = names(models)
names(mse_short) = names(models)

# forecasting, plotting, and evaluation loop
for (model_name in names(models)) {
  model = models[[model_name]]
  
  if (model_name == "Seasonal Means ARIMA(2,0,1)") { # forecast may not be correct
    sm.pred = predict(model.sm, newdata = newdata)
    smd.pred.resid = predict(model, n.ahead = h)
    pr = sm.pred + smd.pred.resid$pred
    uci = pr + 2 * smd.pred.resid$se
    lci = pr - 2 * smd.pred.resid$se
  } else {
    pred = predict(model, n.ahead=h)
    pr = pred$pred
    uci = pr + 2 * pred$se
    lci = pr - 2 * pred$se
  }
  
  # ts forecast
  pr.ts = ts(pr, start=start(ts_future), freq=frequency(ts_future))
  uci.ts = ts(uci, start=start(ts_future), freq=frequency(ts_future))
  lci.ts = ts(lci, start=start(ts_future), freq=frequency(ts_future))
  
  # plot
  ymin = 0 # min(c(lci.ts, ts_future)) - 0.1 # realistically can't be below 0
  ymax = max(c(uci.ts, ts_future)) + 0.1
  
  plot(unrate, xlim=c(2010, 2024), ylim=c(ymin, ymax),
       main=paste(model_name, "Forecast"), ylab="Unemployment Rate (%)")
  lines(pr.ts, col=2)
  lines(uci.ts, col=3)
  lines(lci.ts, col=3)
  abline(h = 0)
  
  # MSE: full forecast ts
  mse_full[model_name] = mean((ts_future - pr)^2, na.rm=T)
  # MSE: months specified
  mse_short[model_name] = mean((ts_future[1:short_h] - pr[1:short_h])^2, na.rm=T)
}

# MSE df
mse_df = data.frame(
  Model = names(models),
  MSE_full = round(mse_full, 3),
  MSE_n_months = round(mse_short, 3),
  row.names = NULL
)

# look at forecast error to decide which model is better (can't compare AIC or BIC due to different d/D)
print(mse_df)
# Lowest forecast error should be best, however, it only applies for a short period of time.
