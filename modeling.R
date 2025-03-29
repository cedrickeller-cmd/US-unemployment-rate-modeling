# Time-Series Analysis and Forecasting of Unemployment Rate in the US

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

unrate.ts <- ts(unrate.df$UNRATENSA
                ,start=c(start.year, start.month)
                ,frequency = 12)

unrate.ts.2019 <- window(unrate.ts, end = c(2019, 12))

# ##################################################
# Analysis
# ##################################################
plot(unrate.ts, type="l", grid=True, ylab="Unemployment Rate (%)", main="US Unemployment Rate 1948-2024")

plot(unrate.ts.2019, type="l", ylab="Unemployment Rate (%)", main="US Unemployment Rate 1948-2019")
