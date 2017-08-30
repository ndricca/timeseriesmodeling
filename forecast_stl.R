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
library(reshape2)

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

########### set parameters ########### 

n_date <- unique(df$day)
n_days_tot <- floor(length(n_date))
n_rows <- dim(df)[1]
period <- 24
days_ahead <- 2
########### forecast.stl model ########### 
# set_of_date <- n_date[1:(n_days_tot-days_ahead)]
# data_train <- df[df$day %in% set_of_date,]
# data_ts <- ts(data_train$sum_error, freq = period * 7)
# decomp_ts <- stl(data_ts,s.window = "periodic",robust = TRUE)
# fcst <- forecast(decomp_ts,h=period*days_ahead)
# fcst_mean <- fcst$mean
# fcst_lower <- fcst$lower[,1]
# fcst_upper <- fcst$upper[,1]
# 
# set_of_date_test <- n_date[(n_days_tot-days_ahead+1):n_days_tot]
# data_test <- df[df$day %in% set_of_date_test,]
# 
# fcst_mean <- fcst_mean[1:nrow(data_test)]
# fcst_lower <- fcst_lower[1:nrow(data_test)]
# fcst_upper <- fcst_upper[1:nrow(data_test)]
# 
# data_pred <- data_test[,c("date","sum_error","day")]
# data_pred$pred <- fcst_mean
# data_pred$pred.low <- fcst_mean + fcst_lower
# data_pred$pred.upp <- fcst_mean + fcst_upper
# 
# data_test[data_test$sum_error > fcst_mean+25,]
# outliers <- rep(NA,nrow(data_test))
# outliers[data_test$sum_error > fcst_mean+25] <- data_test[data_test$sum_error > fcst_mean+25,"sum_error"]
# datas <- data.table(Load = c(data_test$sum_error,fcst_mean,fcst_mean + fcst_lower,fcst_mean + fcst_upper),
#                     Date = rep(data_test$date,4),
#                     Type = c(rep("test",nrow(data_test)),rep("fcst_mean",nrow(data_test)),
#                              rep("fcst_lower",nrow(data_test)),rep("fcst_upper",nrow(data_test))))
# outdata <- data.table(Load = outliers,
#                        Date = data_test$date,
#                        Type = rep("outliers",nrow(data_test)))
# 
# ggplot(datas,aes(Date,Load,color = Type)) + geom_line() +
#   geom_point(data=outdata,aes(Date,Load,color = Type),na.rm = FALSE)
# 
# 
# head(datas)
# head(fcst_mean)
# head(outdata)

########### forecast.stl model function ###########
CtreeTrend <- function(df, set_of_date, period = 24){
  
  data_train <- df[df$day %in% set_of_date,]
  N <- nrow(data_train)
  data_ts <- ts(data_train$sum_error, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  
  fcst <- forecast(decomp_ts,h=24)
  fcst_mean <- fcst$mean
  fcst_lower <- fcst$lower[,1]
  fcst_upper <- fcst$upper[,1]
  
  return(list(forecast = as.vector(fcst_mean), lower = fcst_lower, upper = fcst_upper,
              real = data_train$sum_error))
}


############# outlier oneshot ############# 
# ctree_list <- CtreeTrend(df, n_date[(1):(7*3)])
# summary(ctree_list)
# test_data <- df[df$day %in% n_date[22], ]
# 
# outliers <- rep(NA,nrow(test_data))
# outliers[test_data$sum_error > ctree_list$forecast + ctree_list$upper] <- test_data[test_data$sum_error  > ctree_list$forecast + ctree_list$upper,"sum_error"]
# 
# data_for <- data.table(Load = c(ctree_list$real, test_data$sum_error, ctree_list$forecast, ctree_list$lower, ctree_list$upper),
#                        Date = c(df[df$day %in% n_date[(i+1):(i+7*3)], "date"],
#                                 rep(test_data$date, 4)),
#                        Type = c(rep("Train data", length(ctree_list$real)),
#                                 rep("Test data", length(ctree_list$forecast)),
#                                 rep("Fcst mean", length(ctree_list$forecast)),
#                                 rep("Fcst lower bound", length(ctree_list$forecast)),
#                                 rep("Fcst upper bound", length(ctree_list$forecast))))
# 
# ctree_list <- CtreeTrend(df, n_date[(1):(7*3)])
# # summary(ctree_list)
# test_data <- df[df$day %in% n_date[22], ]
# tail(test_data)
# outliers <- rep(NA,nrow(test_data))
# outliers[test_data$sum_error > ctree_list$forecast + ctree_list$upper] <- test_data[test_data$sum_error  > ctree_list$forecast + ctree_list$upper,"sum_error"]
# outliers[5] <- 200
# 
# data_for <- data.table(Load = c(ctree_list$real, test_data$sum_error, ctree_list$forecast, 
#                                 ctree_list$lower, ctree_list$upper),
#                        Date = c(df[df$day %in% n_date[(1):(7*3)], "date"],
#                                 rep(test_data$date, 4)),
#                        Type = c(rep("Train data", length(ctree_list$real)),
#                                 rep("Test data", length(ctree_list$forecast)),
#                                 rep("Fcst mean", length(ctree_list$forecast)),
#                                 rep("Fcst lower bound", length(ctree_list$forecast)),
#                                 rep("Fcst upper bound", length(ctree_list$forecast))))
# outdata <- data.table(Load = outliers,
#                       Date = test_data$date,
#                       Type = rep("outliers",nrow(test_data)))
# 
# 
# gg1 <- ggplot(data_for, aes(Date, Load, color = Type)) +
#   geom_line(size = 0.8, alpha = 0.75)
# if (!all(is.na(outliers))) gg1 <- gg1 + geom_point(data=outdata,aes(Date,Load),color = "red",shape = "x",size=2.5,na.rm = FALSE)
# gg1 <- gg1 + facet_zoom(xy = Date %in% test_data$date, zoom.size = 1.2, horizontal = FALSE) +
#   labs(x= NULL, title =  paste("Forecast from forecast.stl; ", "day number: ", i+21, sep = ""))
# gg1
# 
# 
# 
# 
# outdata <- data.table(Load = outliers,
#                       Date = test_data$date,
#                       Type = rep("outliers",nrow(test_data)))
# 
# gg1 <- ggplot(data_for, aes(Date, Load, color = Type)) +
#   geom_line(size = 0.8, alpha = 0.75) 
# if (!all(is.na(outliers))) gg1 <- gg1 + geom_point(data=outdata,aes(Date,Load,color = Type),na.rm = FALSE)
# gg1 <- gg1 + facet_zoom(xy = Date %in% test_data$date, zoom.size = 1.2, horizontal = FALSE) +
#   labs(x= NULL, title =  paste("Forecast from forecast.stl; ", "day number: ", i+21, sep = ""))



########### animation ###########
define_region <- function(row, col){
    viewport(layout.pos.row = row, layout.pos.col = col)
  }
total = n_days-1

pb <- txtProgressBar(min = 0, max = total, style = 3)
ani.record(reset=TRUE)
n_days <- floor(length(n_date)) - 21


for(i in 0:(n_days-1)){

  ctree_list <- CtreeTrend(df, n_date[(i+1):(i+7*3)])

  test_data <- df[df$day %in% n_date[22+i], ]
  n_test_data <- nrow(test_data)
  if (length(ctree_list$forecast) > n_test_data) {
    ctree_list$forecast <- ctree_list$forecast[1:n_test_data]
    ctree_list$lower <- ctree_list$lower[1:n_test_data]
    ctree_list$upper <- ctree_list$upper[1:n_test_data]
  }


  outliers <- rep(NA,nrow(test_data))
  outliers[test_data$sum_error > ctree_list$forecast + ctree_list$upper] <- test_data[test_data$sum_error  > ctree_list$forecast + ctree_list$upper,"sum_error"]

  data_for <- data.table(Load = c(ctree_list$real, test_data$sum_error, ctree_list$forecast, ctree_list$lower, ctree_list$upper),
                         Date = c(df[df$day %in% n_date[(i+1):(i+7*3)], "date"],
                                  rep(test_data$date, 4)),
                         Type = c(rep("Train data", length(ctree_list$real)),
                                  rep("Test data", length(ctree_list$forecast)),
                                  rep("Fcst mean", length(ctree_list$forecast)),
                                  rep("Fcst lower bound", length(ctree_list$forecast)),
                                  rep("Fcst upper bound", length(ctree_list$forecast))))

  outdata <- data.table(Load = outliers,
                        Date = test_data$date,
                        Type = rep("outliers",nrow(test_data)))

                        gg1 <- ggplot(data_for, aes(Date, Load, color = Type)) +
                          geom_line(size = 0.8, alpha = 0.75)
                        if (!all(is.na(outdata$Load))) gg1 <- gg1 + geom_point(data=outdata,aes(Date,Load,color = Type),color = "red",shape = "x",size=2.5,na.rm = FALSE)
                        gg1 <- gg1 + facet_zoom(xy = Date %in% test_data$date, zoom.size = 1.2, horizontal = FALSE) +
                           labs(x= NULL, title =  paste("Forecast from forecast.stl; ", "day number: ", i+21, sep = ""))
                    #      labs(x= NULL, title =  paste("Forecast from CTREE; ", "day: ", i+21, "; MAPE: ",
                    #                          round(ctree_err_mape, 2), "%", sep = "")) +
                    #      theme_ts

                    #    trend_data <- data.table(Load = c(ctree_list$trend, ctree_list$trendFor),
                    #                             Date = c(DT[date %in% n_date[(i+1):(i+7*3)], date_time],
                    #                                      test_data$date_time),
                    #                             Type = c(rep("Real", length(ctree_list$trend)),
                    #                                      rep("Forecast", length(ctree_list$trendFor))))
                    #
                    #    gg2 <- ggplot(trend_data, aes(Date, Load, color = Type)) +
                    #      geom_line(size = 1.2) +
                    #      labs(title = paste("Trend Forecasting with ", ctree_list$trendFit)) +
                    #      theme_ts

                    grid.newpage()
                    # Create layout : nrow = 2, ncol = 1
                    pushViewport(viewport(layout = grid.layout(1, 1)))
                    # Arrange the plots
                    print(gg1, vp = define_region(1, 1))
                    #    print(gg2, vp = define_region(2, 1))
                    setTxtProgressBar(pb, i)
                    ani.record()
                    dev.off()
}
close(pb,i)
htmlfilename <- paste0("html_animated_plot_",format(Sys.time(),"%Y%m%d%H%M%S"),".html")
saveHTML(ani.replay(), img.name = "recorded_plot", htmlfile = htmlfilename, outdir = getwd())