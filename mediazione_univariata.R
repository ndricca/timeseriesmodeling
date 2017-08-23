Sys.setlocale("LC_TIME","C")
set.seed(123)
library(ggplot2)
library(TTR)
library(ggjoy)
library(gbm)
library(tidyverse)
library(stringr)
library(tseries)
library(ggplot2)
library(forecast)
library(xts)
library(reshape2)
library(padr)


df <- read.csv("C://Users//andrea.ricca//Downloads//series1_count_errors.csv",
               sep = ",",header = T)
df$date = as.POSIXct(df$date, format = "%Y/%m/%d %H")
df <- df %>% pad %>% fill_by_value(value = 0)
levels(df$root) <- "series_one"
df[is.na(df$root),"root"] <- "series_one"
df$date_hour <- as.factor(df$date_hour)
df$date_wday <- as.factor(weekdays(df$date))

summary(df)

df %>% 
  select(sum_error,date_wday) %>%
  group_by(date_wday) %>%
  do(ggplot2:::compute_density(.$sum_error, NULL)) %>%
  rename(sum_error = x) -> df_density_wday

ggplot(df_density_wday, aes(x = sum_error, y = date_wday, height = density)) + 
  geom_joy(stat = "identity")

ggplot(df_density_wday, aes(x = sum_error, y = date_wday, height = ..density..)) + 
  geom_joy(stat = "binline",bins = 10)


ggplot(df,aes(x = sum_error)) + 
  geom_histogram(aes(y=..density..), binwidth=100,position="identity") + 
  stat_function(geom = "line", fun = dpois, arg = list(lambda = mean(df$sum_error)), colour = "red", fill = NA, n = 9)

mean(df$sum_error)
round(max(df$sum_error),-3)
summary(errordata)

errordata <- xts(x = df[,5], order.by = df[,1])
attr(errordata,"frequency ") <- 24
str(coredata(errordata))
ggplot(errordata,aes(x = errordata[,1])) + geom_density()

plot(decompose(as.ts(errordata)))

plot(ets(as.ts(errordata)))
error.ts <- ts(errordata,start = start(errordata), frequency = 24*7)
index(error.ts) <- as.POSIXct(as.numeric(as.character(index(error.ts))),origin="1970-01-01",tz="GMT")
attributes(errordata)$frequency <- 24*7
plot(decompose(as.ts(errordata)))
plot(dec_err)
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
