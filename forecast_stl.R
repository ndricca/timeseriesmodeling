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

########### set parameters ########### 

n_date <- unique(df$day)
n_days_tot <- floor(length(n_date))
n_rows <- dim(df)[1]
period <- 24
days_ahead <- 2

########### forecast.stl model function ###########
forecast_stl <- function(df, set_of_date, period = 24){
  
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


############# forecast stl oneshot ############# 
i<-11
forecast_stl_list <- forecast_stl(df, n_date[(i+1):(i+7*3)])

test_data <- df[df$day %in% n_date[22+i], ]
n_test_data <- nrow(test_data)
if (length(forecast_stl_list$forecast) > n_test_data) {
  forecast_stl_list$forecast <- forecast_stl_list$forecast[1:n_test_data]
  forecast_stl_list$lower <- forecast_stl_list$lower[1:n_test_data]
  forecast_stl_list$upper <- forecast_stl_list$upper[1:n_test_data]
}

outliers <- rep(NA,nrow(test_data))
outliers[test_data$sum_error > forecast_stl_list$upper] <- test_data[test_data$sum_error  > forecast_stl_list$upper,"sum_error"]

data_for <- data.table(Load = c(forecast_stl_list$real, test_data$sum_error,
                                forecast_stl_list$forecast, forecast_stl_list$lower, forecast_stl_list$upper),
                       Date = c(df[df$day %in% n_date[(i+1):(i+7*3)], "date"],
                                rep(test_data$date, 4)),
                       Type = c(rep("Train data", length(forecast_stl_list$real)),
                                rep("Test data", length(forecast_stl_list$forecast)),
                                rep("Fcst mean", length(forecast_stl_list$forecast)),
                                rep("Fcst lower bound", length(forecast_stl_list$forecast)),
                                rep("Fcst upper bound", length(forecast_stl_list$forecast))))

outdata <- data.table(Load = outliers,
                      Date = test_data$date,
                      Type = rep("outliers",nrow(test_data)))

gg1 <- ggplot(data_for, aes(Date, Load, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75)
if (!all(is.na(outdata$Load))) gg1 <- gg1 + geom_point(data=outdata,aes(Date,Load,color = Type),color = "red",shape = "x",size=2.5,na.rm = FALSE)
gg1 <- gg1 + facet_zoom(xy = Date %in% test_data$date, zoom.size = 1.2, horizontal = FALSE) +
  labs(x= NULL, title =  paste("Forecast from forecast.tbats; ", "day number: ", i+21, sep = ""))
gg1



########### animation ###########
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
}
total = n_days-1

pb <- txtProgressBar(min = 0, max = total, style = 3)
ani.record(reset=TRUE)
training_days <- 35
n_days <- floor(length(n_date)) - training_days


for(i in 0:(n_days-1)){
  
  forecast_stl_list <- forecast_stl(df, n_date[(i+1):(i+training_days)])
  
  test_data <- df[df$day %in% n_date[training_days+1+i], ]
  n_test_data <- nrow(test_data)
  if (length(forecast_stl_list$forecast) > n_test_data) {
    forecast_stl_list$forecast <- forecast_stl_list$forecast[1:n_test_data]
    forecast_stl_list$lower <- forecast_stl_list$lower[1:n_test_data]
    forecast_stl_list$upper <- forecast_stl_list$upper[1:n_test_data]
  }
  
  outliers <- rep(NA,nrow(test_data))
  outliers[test_data$sum_error > forecast_stl_list$upper] <- test_data[test_data$sum_error  > forecast_stl_list$upper,"sum_error"]
  
  data_for <- data.table(Load = c(forecast_stl_list$real, test_data$sum_error, forecast_stl_list$forecast, forecast_stl_list$lower, forecast_stl_list$upper),
                         Date = c(df[df$day %in% n_date[(i+1):(i+training_days)], "date"],
                                  rep(test_data$date, 4)),
                         Type = c(rep("Train data", length(forecast_stl_list$real)),
                                  rep("Test data", length(forecast_stl_list$forecast)),
                                  rep("Fcst mean", length(forecast_stl_list$forecast)),
                                  rep("Fcst lower bound", length(forecast_stl_list$forecast)),
                                  rep("Fcst upper bound", length(forecast_stl_list$forecast))))
  
  outdata <- data.table(Load = outliers,
                        Date = test_data$date,
                        Type = rep("outliers",nrow(test_data)))
  
  gg1 <- ggplot(data_for, aes(Date, Load, color = Type)) +
    geom_line(size = 0.8, alpha = 0.75)
  if (!all(is.na(outdata$Load))) gg1 <- gg1 + geom_point(data=outdata,aes(Date,Load,color = Type),color = "red",shape = "x",size=2.5,na.rm = FALSE)
  gg1 <- gg1 + facet_zoom(xy = Date %in% test_data$date, zoom.size = 1.2, horizontal = FALSE) +
    labs(x= NULL, title =  paste("Forecast from forecast.stl; ", "day number: ", i+training_days, sep = ""))
  
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

imgname = paste0("recorded_plot_",format(Sys.time(),"%Y%m%d%H%M%S"))
htmlfilename <- paste0("stl_html_animated_plot_",format(Sys.time(),"%Y%m%d%H%M%S"),".html")
saveHTML(ani.replay(), img.name = imgname, htmlfile = htmlfilename, outdir = getwd())