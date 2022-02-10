############################################################
# DES: Motor crashes analysis
# BY: Tiernan Barry
############################################################

library("aws.s3")
library("utils")
library("plotly")
library("rpivotTable")
library("tseries")
library("fUnitRoots")
library("forecast")

############################################################
# Get data
############################################################

ts_data_sum <- readRDS("/home/rstudio/motor_crashes/Output/ts_data_sum.rds")
ts_data_sum$DATE <- as.Date(ts_data_sum$DATE, format = "%m/%d/%Y")
ts_data_sum <- ts_data_sum[order(ts_data_sum$DATE, decreasing = F), ]
ts_data_sum_ts <- ts(ts_data_sum$COUNT, frequency = 365.25)
dec_ts_data_sum = decompose(ts_data_sum_ts)

plot(dec_ts_data_sum)

# run auto arima:
forecast::auto.arima(ts_data_sum_ts[3000:length(ts_data_sum_ts)], trace=TRUE)

arima_model <- arima(ts_data_sum_ts[3000:length(ts_data_sum_ts)], 
                     order=c(0, 1, 4), 
                     method="ML")

preds <- forecast(arima_model, h = 30, level=c(99))

plot(preds)

# run auto arima:
forecast::auto.arima(ts_data_sum_ts, trace=TRUE)

arima_model <- forecast::Arima(ts_data_sum_ts,
                              order=c(5, 1, 2),
                              include.drift = T)

preds <- forecast(arima_model, h = 300, level=c(99.6))
plot(preds)


alpha_test <- 0.05
adf_res <- adf.test(ts_data_stny, alternative = "stationary")
adf_res

diff_order <- 1
ar_order <- 0
ma_order <- 3

# ARIMA
acf(ts_data_stny)
pacf(ts_data_stny)

arima_model <- arima(ts_data_sum_ts, 
                     order=c(ar_order, diff_order, ma_order), 
                     #seasonal = list(order = c(1,0,0), period = 365.25),
                     method="ML")

pred <- predict(arima_model, n.ahead = 20)

futurVal <- forecast.Arima(arima_model,h=10, level=c(99.5))
plot.forecast(futurVal)
