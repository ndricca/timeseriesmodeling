########### opts and packages ########### 
Sys.setlocale("LC_TIME","C")
set.seed(123)
library(graphics)
library(ggplot2)
library(ggforce)
library(rpart)
library(party)
library(data.table)
library(forecast)
library(tsoutliers)
library(plotly)
library(tidyverse)
library(padr)
library(grid)
library(animation)
library(astsa)
library(mgcv)
library(Rssa)


########### data import and preparation ########### 

df <- read.csv("C://Users//andrea.ricca//Downloads//series1_count_errors.csv",
               sep = ",",header = T)
df$date = as.POSIXct(df$date, format = "%Y/%m/%d %H")
df <- df %>% pad %>% fill_by_value(value = 0)
levels(df$root) <- "series_one"
df[is.na(df$root),"root"] <- "series_one"
df$date_hour <- as.factor(df$date_hour)
df$date_wday <- as.factor(weekdays(df$date))
df$day <- format(df$date,"%Y/%m/%d")

# remove first and last day from df because they are not complete days
df <- df[!df$day %in% c(unique(df$day)[1],unique(df$day)[length(unique(df$day))]),]

# set parameters
n_date <- unique(df$day)
n_days_tot <- floor(length(n_date))
n_rows <- dim(df)[1]
period <- 24
window <- n_rows / period 
days_ahead <- 1

i<-11
data_train <- df[df$day %in% n_date[(i+1):(i+7*3)],]
test_data <- df[df$day %in% n_date[22+i], ]

data_ts <- ts(data_train$sum_error,frequency = period * 7)

four_train <- fourier(data_ts,K=2)
four_test <- fourier(data_ts,K=2,h=24)
arima_fourier_model <- auto.arima(data_ts, xreg = four_train,seasonal = TRUE)
fcst_arima_fourier <- forecast(arima_fourier_model,xreg=four_test,h=24,level=95)

forecast_arima_fourier <- function(df, set_of_date, period = 24){
  
  data_train <- df[df$day %in% set_of_date,]
  data_ts <- ts(data_train$sum_error, freq = period*7)
  four_train <- fourier(data_ts,K=2)
  four_test <- fourier(data_ts,K=2,h=24)
  
  arima_fourier_model <- auto.arima(data_ts, xreg = four_train,seasonal = TRUE)
  
  fcst <- forecast(arima_fourier_model,xreg=four_test,h=24,level = 95)
  fcst_mean <- fcst$mean
  fcst_lower <- fcst$lower
  fcst_upper <- fcst$upper
  
  return(list(forecast = as.vector(fcst_mean), lower = fcst_lower, upper = fcst_upper,
              real = data_train$sum_error))
}


##### oneshot model ######
i<-11
forecast_arima_fourier_list <- forecast_arima_fourier(df, n_date[(i+1):(i+7*3)])

test_data <- df[df$day %in% n_date[22+i], ]
n_test_data <- nrow(test_data)
if (length(forecast_arima_fourier_list$forecast) > n_test_data) {
  forecast_arima_fourier_list$forecast <- forecast_arima_fourier_list$forecast[1:n_test_data]
  forecast_arima_fourier_list$lower <- forecast_arima_fourier_list$lower[1:n_test_data]
  forecast_arima_fourier_list$upper <- forecast_arima_fourier_list$upper[1:n_test_data]
}

outliers <- rep(NA,nrow(test_data))
outliers[test_data$sum_error > forecast_arima_fourier_list$upper] <- test_data[test_data$sum_error  > forecast_arima_fourier_list$upper,"sum_error"]

data_for <- data.table(Load = c(forecast_arima_fourier_list$real, test_data$sum_error,
                                forecast_arima_fourier_list$forecast, forecast_arima_fourier_list$lower, forecast_arima_fourier_list$upper),
                       Date = c(df[df$day %in% n_date[(i+1):(i+7*3)], "date"],
                                rep(test_data$date, 4)),
                       Type = c(rep("Train data", length(forecast_arima_fourier_list$real)),
                                rep("Test data", length(forecast_arima_fourier_list$forecast)),
                                rep("Fcst mean", length(forecast_arima_fourier_list$forecast)),
                                rep("Fcst lower bound", length(forecast_arima_fourier_list$forecast)),
                                rep("Fcst upper bound", length(forecast_arima_fourier_list$forecast))))

outdata <- data.table(Load = outliers,
                      Date = test_data$date,
                      Type = rep("outliers",nrow(test_data)))

gg1 <- ggplot(data_for, aes(Date, Load, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75)
if (!all(is.na(outdata$Load))) gg1 <- gg1 + geom_point(data=outdata,aes(Date,Load,color = Type),color = "red",shape = "x",size=2.5,na.rm = FALSE)
gg1 <- gg1 + facet_zoom(xy = Date %in% test_data$date, zoom.size = 1.2, horizontal = FALSE) +
  labs(x= NULL, title =  paste("Forecast from forecast arima fourier; ", "day number: ", i+21, sep = ""))
gg1
