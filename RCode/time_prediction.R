library(readr)
library(forecast)
stats3 <- read_csv("../data/stats3.csv")
# fecting 1st and 5th column, 5th column is for US, raw timeseries generation
statstimeseries = ts(stats3[, c(0, 5)], frequency = 12, start = c(2006, 1))
plot.ts(statstimeseries)
exports_ts <- log(statstimeseries)
plot.ts(exports_ts)
exports_decoposed <- decompose(exports_ts)
plot(exports_decoposed)
auto.arima(exports_ts)
fit <- arima(exports_ts, order = c(2, 0, 0))
accuracy(fit)
forecasted = forecast(fit, 5)
plot(forecasted)
# removing seasonalily
export_without_sesonality <- exports_ts - exports_decoposed$seasonal
plot(export_without_sesonality)
stats_data <- diff(export_without_sesonality, differences = 1)
plot.ts(stats_data)
par(mfrow = c(2, 1))
acf(stats_data)
pacf(stats_data)

