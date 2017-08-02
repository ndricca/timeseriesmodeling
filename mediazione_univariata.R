set.seed(123)
library(ggplot2)
library(TTR)
library(gbm)
library(dplyr)
library(stringr)
library(tseries)
library(ggplot2)
library(forecast)
library(xts)
library(reshape2)

 
data <- read.csv("C://Users//andrea.ricca//Downloads//DA_GTEL_SRVGestorePreventivo_count_errors.csv",
                 sep = ",",header = T)
data$date = as.POSIXct(data$date, format = "%Y/%m/%d %H:%M:%S")

time_index <- seq(from = as.POSIXct("2017-04-15"), 
                  to = as.POSIXct(data$date[nrow(data)]), by = "hour")

time_index <- as.xts(time_index)["2017-04-15T18:00/"]
na.fill(time_index,fill = 0)
df <- data.frame(date=time_index)

errordata <- as.matrix(data[,6])

count <- xts(errordata, order.by = as.POSIXct(data[,1]), frequency =24*7)
errorcount <- merge(count,time_index)[,1]
errorcount <- na.fill(errorcount,fill = 0)
attributes(errorcount)$frequency <- 24*7
tsdisplay(diff(diff(errorcount),24))

a <- Arima(errorcount, order = c(3,0,3), seasonal = c(1,0,0))
tsdisplay(residuals(a))

fit <- auto.arima(errorcount)
plot(fit)
plot(stl(errorcount,s.window = 24))
#test_error <- new_df[2221:nrow(new_df),c(1,6)]

train_error$days <- as.factor(weekdays(as.Date(train_error$date)))
train_error$weekend <- as.factor(ifelse(train_error$days == "sabato" | train_error$days == "domenica", "Weekend", "Weekday"))
train_error$month <- as.factor(format(as.Date(train_error$date), "%m"))
train_error$weeknum <- as.POSIXlt(train_error$date)
train_error$weeknum <- as.factor(strftime(train_error$weeknum,format="%W"))

str(train_error)
#ggplot(data=train_error) + geom_point(aes(x=days,y=sum_error,color = month))

train_error$ma <- EMA(train_error$sum_error, 24)
train1 <- train_error[-(1:24),]
str(train1)

train <- train1[(1:2182),]
val <- train1[(2183:2206),]
errors <- val$sum_error
val$sum_error <- NULL
Dates <- as.character(val$date)


formula <- sum_error~(days + weekend + weeknum + month)*ma

fit <- gbm(formula, data = train, n.trees = 100000)

model <- predict(fit, newdata = val, n.trees = 100000)
summary(fit)
df <- data.frame(Dates, errors, (floor(model)))
colnames(df)[3] <- "model"
df
df_m <- melt(df,id.vars = "Dates")
ggplot(data=df_m,
       aes(x=Dates, y=value, colour=variable,group = 1)) +
  geom_point()

ggplot(data = df, aes(x=seq_along(date)) + geom_line(aes(y=model,colour = "model")) + geom_line(aes(y=errors,colour = "errors"))

train_error$weekend <- ifelse(train_error$date_wday == "saturday" | train_error$date_wday == "sunday","weekend","weekday")
train_error$date_wday <- as.factor(train_error$date_wday)
train_error$weekend <- as.factor(train_error$weekend)
