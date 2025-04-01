# Time-Series Analysis and Forecasting of Unemployment Rate in the US

library(forecast)

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

unrate.ts = ts(unrate.df$UNRATENSA
                ,start=c(start.year, start.month)
                ,frequency = 12)

unrate.ts.2019 = window(unrate.ts, end=c(2019, 12))

unrate.ts.1999.2005 = window(unrate.ts, start=c(1999, 1), end=c(2005, 12))
unrate.ts.2003.2006 = window(unrate.ts, start=c(2003, 1), end=c(2006, 12))


# ##################################################
# Analysis
# ##################################################

# plot time series
#    full ts
plot(unrate.ts, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 1948-2024")
#    ts pre-covid
plot(unrate.ts.2019, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 1948-2019")
#    seasonal demonstration
plot(unrate.ts.1999.2005, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 1999-2005")
grid()
plot(unrate.ts.2003.2006, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 2003-2006")
grid()





# ##################################################
# ##################################################
# test / exploration
# ##################################################

# seasonal differencing
diff.2019 = diff(unrate.ts.2019,lag=12)

plot(diff.2019, type="l")
auto.arima(diff.2019)
acf(diff.2019,lag.max=50)
pacf(diff.2019)
eacf(diff.2019)


# 2nd diff
diff2.2019 = diff(diff(unrate.ts.2019),lag=12) # maybe lag=30?

plot(diff2.2019, type="l")
auto.arima(unrate.ts.2019)
acf(diff2.2019,lag.max=50)
pacf(diff2.2019)

#    least squares for seasonal means
lm.2019 = lm(unrate.ts.2019 ~ season(unrate.ts.2019))
plot(rstudent(ls.2019), type="l")

summary(lm.2019)
#    use hac for seasonal means differences
coeftest(lm.2019, vcov=vcovHAC(lm.2019))
#    make inferences

# linear model with seasonal differencing 

diff.lm.2019 = diff(resid(lm.2019), lag=12)
plot(diff.lm.2019, type="l")

auto.arima(diff.lm.2019)
acf(diff.lm.2019)
pacf(diff.lm.2019)
eacf(diff.lm.2019)



#    least squares for seasonal means + linear trend    --- is about the same as above
lm_lin_2019 = lm(unrate.ts.2019 ~ cycle(unrate.ts.2019) + time(unrate.ts.2019)) 
plot(rstudent(lm_lin_2019), type="l", main="Linear trend model 1948-2019")

diff.lm.lin.2019 = diff(rstudent(lm_lin_2019))
plot(diff.lm.lin.2019, type="l")
acf(diff.lm.lin.2019)

library(TSA)

#    least squares to fit a cosine trend with fundamental frequency 1/12
frequency(unrate.ts.2019) # frequency = 12
unrate.ts.2019_harmonic = harmonic(unrate.ts.2019, m=1) # 1/12
unrate.ts.2019_cos = lm(unrate.ts.2019 ~ unrate.ts.2019_harmonic)
summary(unrate.ts.2019_cos)
plot(rstudent(unrate.ts.2019_cos), type="l", main="Cosine trend model 1948-2019")

#    differenced
diff.2019 = diff(unrate.ts.2019)
plot(diff.2019)
acf(diff.2019)



plot(log(diff.2019))
