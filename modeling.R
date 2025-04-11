# Time-Series Analysis and Forecasting of Unemployment Rate in the US

library(forecast)
library(TSA)

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

unrate.2019 = window(unrate, end=c(2019, 12))

unrate.1999.2005 = window(unrate, start=c(1999, 1), end=c(2005, 12))
unrate.2003.2006 = window(unrate, start=c(2003, 1), end=c(2006, 12))


# ##################################################
# Plotting Time Series
# ##################################################
#    full ts
plot(unrate, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 1948-2024")

#    ts pre-covid -- will be used for fitting
plot(unrate.2019, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 1948-2019")

#    seasonal demonstration
plot(unrate.1999.2005, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 1999-2005")
grid()
plot(unrate.2003.2006, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 2003-2006")
grid()
# There is a seasonal pattern within each year (freq could be 12)

# histogram
hist(unrate.2019)

# ##################################################
# Seasonal Decomposition (STL)
# ##################################################
sdecomp = stl(unrate, s.window="periodic")
plot(sdecomp)
sdecomp.2019 = stl(unrate.2019, s.window="periodic")
plot(sdecomp.2019)

# ##################################################
# Deterministinc Trend (probably not)
# ##################################################
# seasonal means model
month = season(unrate.2019)
model.sm = lm(unrate.2019~month)
summary(model.sm)

model.sm.resid=ts(resid(model.sm),freq=12)
plot(model.sm.resid, main="Residuals of seasonal means model\n1948-2019")
abline(h=mean(model.sm.resid)) # mean is basically zero


plot(rstandard(model.cos))
abline(h=0,col=2)

# normal distribution check
hist(model.sm.resid)
qqnorm(model.sm.resid)
qqline(model.sm.resid)
# It is not normally distributed

#     fitting model
#plot(unrate.2019)
#model.sm.f = ts(fitted(model.sm)
#                 ,start = start(unrate.2019)
#                 )#,freq = frequency(unrate.2019)) # 12
#lines(model.sm.f,col=2,lty=2)

# cosine trend model
unrate.2019.harmonic = harmonic(unrate.2019, m=1) # 1/12
model.cos = lm(unrate.2019~unrate.2019.harmonic)
summary(model.cos)

model.cos.resid = resid(model.cos)
plot(model.cos.resid, type="l", main="Residuals of cosine trend model\n1948-2019")
abline(h=mean(model.cos.resid)) # mean is basically zero

#     normal distribution check
hist(model.cos.resid)
qqnorm(model.cos.resid)
qqline(model.cos.resid)
# It is not normally distributed

#     fitting model
#plot(unrate.2019)
#model.cos.f = ts(fitted(model.cos)
#                  ,start = start(unrate.2019)
#                  )#,freq = frequency(unrate.2019)) # 12
#lines(model.cos.f,col=2,lty=2)

plot(model.cos.resid)
abline(h=0,col=2)

# Both model's residuals have zero mean,
#   but they may lack homoscedasticity and normal distribution.


# polynomial trend
model.poly = lm(unrate.2019~month+time(unrate.2019))

model.poly.resid = resid(model.poly)
plot(model.poly.resid, type="l", main="Residuals of poly trend model\n1948-2019")
abline(h=mean(model.poly.resid)) # mean is basically zero

#     normal distribution check
hist(model.poly.resid)
qqnorm(model.poly.resid)
qqline(model.poly.resid)
# It is not normally distributed

#plot(unrate.2019)
#model.poly.f = ts(fitted(model.poly)
#                  ,start = start(unrate.2019)
#                  )#,freq = frequency(unrate.2019)) # 12
#lines(model.poly.f,col=2,lty=2)

plot(rstandard(model.poly))
abline(h=0,col=2)
acf(rstandard(model.poly))


# ##################################################
# Stochastic Trend and Seasonal Transformation
# ##################################################
# seasonal differencing
s.diff.2019 = diff(unrate.2019,lag=12)
plot(s.diff.2019, type="l", ylab="Seasonally Differenced Unemployment Rate", main="Seasonally Differenced\nUS Unemployment Rate 1948-2019")

model.sdiff = Arima(unrate.2019, order=c(0,0,0), seasonal=c(0,1,0))
sdiff.resid = resid(model.sdiff)
plot(sdiff.resid, type="l", main="Residuals of seasonal difference\n1948-2019")
abline(h=mean(sdiff.resid)) # mean is basically zero

#     normal distribution check
hist(sdiff.resid)
qqnorm(sdiff.resid)
qqline(sdiff.resid)
# not a satisfactory result. There may be a stochastic trend.

# first difference (d=1)
diff.2019 = diff(unrate.2019)
plot(diff.2019, type="l", ylab="Differenced Unemployment Rate", main="Differenced\nUS Unemployment Rate 1948-2019")

model.diff = Arima(unrate.2019, order=c(0,1,0))
diff.resid = resid(model.diff)
plot(diff.resid, type="l", main="Residuals of first difference\n1948-2019")
abline(h=mean(diff.resid)) # mean is basically zero

#     normal distribution check
hist(diff.resid)
qqnorm(diff.resid)
qqline(diff.resid)


# double difference (first and seasonal) (d=1, D=1)
diff2.2019 = diff(diff(unrate.2019),lag=12)
plot(diff2.2019, type="l", ylab="Double Differenced Unemployment Rate", main="Double Differenced\nUS Unemployment Rate 1948-2019")

model.diff2 = Arima(unrate.2019, order=c(0,1,0), seasonal=c(0,1,0))
diff2.resid = resid(model.diff2)
#     Residuals Assumptions
#     residuals (zero mean and homoscedasticity)
plot(diff2.resid, type="l", main="Residuals of first and seasonal difference\n1948-2019")
abline(h=mean(diff2.resid)) # mean is basically zero
#     normal distribution check
hist(diff2.resid) # heavy tails
qqnorm(diff2.resid) # heavy tails
qqline(diff2.resid)
#     Shapiro-Wilk test (normality)
shapiro.test(diff2.2019) # violated p-value = 1.255e-11

#     runs test and ACF plot (independence)
runs(diff2.2019)
acf(diff2.2019)

#     Stationarity
adf.test(diff2.2019) # rejected -> stationary
pp.test(diff2.2019) # rejected -> stationary
kpss.test(diff2.2019) # rejected -> NOT stationary


# ##################################################
# Choosing Model parameters
# ##################################################
# p & q
plot(diff2.2019, type="l")
acf(diff2.2019,lag.max=80) # MA(1)
pacf(diff2.2019,lag.max=80) # AR(3)
eacf(diff2.2019) # ARMA(2,1), ARMA(2,2)
auto.arima(diff2.2019) # SARIMA(2,0,1)(2,0,2)
auto.arima(unrate.2019) # SARIMA(3,0,1)(2,1,2)

# seasonal P & Q
acf(diff2.2019,lag.max=700) # MA(0)/MA(1)/MA(2)/MA(3)   ????????????????????????????????????????????
pacf(diff2.2019,lag.max=700) # AR(0)                    ????????????????????????????????????????????


# ##################################################
# Candidate Models
# ##################################################  SEASONAL COMPONENT  ????????????????????????????????????????????
sari1 =         Arima(diff2.2019,order=c(0,0,1),seasonal=c(2,0,2),include.mean=F)
sari3 =         Arima(diff2.2019,order=c(3,0,0),seasonal=c(2,0,2),include.mean=F)
sarima21 =      Arima(diff2.2019,order=c(2,0,1),seasonal=c(2,0,2),include.mean=F)
sarima22 =      Arima(diff2.2019,order=c(2,0,2),seasonal=c(2,0,2),include.mean=F)
#sarima201.202 = Arima(diff2.2019,order=c(2,0,1),seasonal=c(2,0,2),include.mean=F)
sarima301.212 = Arima(unrate.2019,order=c(3,0,1),seasonal=c(2,1,2),include.mean=F)
sarima212.212 = Arima(unrate.2019,order=c(2,1,2),seasonal=c(2,1,2),include.mean=F)

sari1
sari3
sarima21
sarima22
sarima201.202
sarima301.212 # best model
sarima212.212

res = armasubsets(y=diff2.2019,nar=14,nma=14,y.name='test',ar.method='ols')
plot(res)	# default is BIC
# AR(1) model is chosen based on BIC
plot(res,scale='AIC')

# Model diagnostics
tsdiag(sarima301.212)
tsdiag(sarima212.212)


