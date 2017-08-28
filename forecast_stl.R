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
  
  df <- read.csv("C://Users//andrea.ricca//Downloads//series1_count_errors.csv",
                 sep = ",",header = T)
  df$date = as.POSIXct(df$date, format = "%Y/%m/%d %H")
  df <- df %>% pad %>% fill_by_value(value = 0)
  levels(df$root) <- "series_one"
  df[is.na(df$root),"root"] <- "series_one"
  df$date_hour <- as.factor(df$date_hour)
  df$date_wday <- as.factor(weekdays(df$date))
  df$day <- format(df$date,"%Y/%m/%d")
  
  n_date <- unique(df$day)
  n_days <- floor(length(n_date)) - 21
  n_rows <- length(n_date)
  period <- 24
  
  CtreeTrend <- function(df, set_of_date, K = 2, period = 24){
    
    # subsetting the dataset by dates
    data_train <- df[df$day %in% set_of_date,]
    
    N <- nrow(data_train)
    window <- (N / period) - 1
    
    #data_ts <- msts(data_train$value, seasonal.periods = c(period, period*7))
    
    #fuur <- fourier(data_ts, K = c(K, K))
    #fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
    
    data_ts <- ts(data_train$sum_error, freq = period*7)
    decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
    
    #new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
    #trend_part <- ts(decomp_ts$time.series[,2])
    
    #trend_fit <- auto.arima(trend_part)
    #trend_for <- as.vector(forecast(trend_fit, period)$mean)
    
    #lag_seas <- decomp_ts$time.series[1:(period*window), 1]
    
    #matrix_train <- data.table(Load = tail(new_load, window*period),
    #                           fuur[(period+1):N,],
    #                           Lag = lag_seas)
    
    #tree_2 <- party::ctree(Load ~ ., data = matrix_train,
    #                       controls = party::ctree_control(teststat = "quad",
    #                                                       testtype = "Teststatistic",
    #                                                       mincriterion = 0.925,
    #                                                       minsplit = 1,
    #                                                       minbucket = 1))
    
    #test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
    
    #matrix_test <- data.table(fuur_test,
    #                          Lag = test_lag)
    
    #pred_tree <- predict(tree_2, matrix_test) + trend_for
    
    ########## my model ##########
    fcst <- forecast(decomp_ts,h=24)
    fcst_mean <- fcst$mean
    fcst_lower <- fcst$lower[,1]
    fcst_upper <- fcst$upper[,1]
    
    return(list(forecast = as.vector(fcst_mean), lower = fcst_lower, upper = fcst_upper,
                real = data_train$sum_error))
  }
  
  define_region <- function(row, col){
    viewport(layout.pos.row = row, layout.pos.col = col)
  }
total = n_days-1  
pb <- txtProgressBar(min = 0, max = total, style = 3)
ani.record(reset=TRUE)
for(i in 0:(n_days-1)){
  
  ctree_list <- CtreeTrend(df, n_date[(i+1):(i+7*3)])
  
  test_data <- df[df$day %in% n_date[22+i], ]
  #    ctree_err_mape <- mape(test_data$value,
  #                           ctree_list$forecast)
  
  # 1. plot of forecasts
  
  data_for <- data.table(Load = c(ctree_list$real, test_data$sum_error, ctree_list$forecast, ctree_list$lower, ctree_list$upper),
                         Date = c(df[df$day %in% n_date[(i+1):(i+7*3)], "date"],
                                  rep(test_data$date, 4)),
                         Type = c(rep("Train data", length(ctree_list$real)),
                                  rep("Test data", length(ctree_list$forecast)),
                                  rep("Fcst mean", length(ctree_list$forecast)),
                                  rep("Fcst lower bound", length(ctree_list$forecast)),
                                  rep("Fcst upper bound", length(ctree_list$forecast))))
                         
                         gg1 <- ggplot(data_for, aes(Date, Load, color = Type)) +
                           geom_line(size = 0.8, alpha = 0.75) +
                           facet_zoom(xy = Date %in% test_data$date, zoom.size = 1.2, horizontal = FALSE) +
                                      labs(x= NULL, title =  paste("Forecast from CTREE; ", "day: ", i+21, sep = ""))
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
saveHTML(ani.replay(), img.name = "record_plot",outdir = getwd())
