---
title: "prova"
author: "ndricca"
date: "27 luglio 2017"
output: github_document 
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Time series analysis

Univariate and multivariate models for anomaly detection.
First attempt of concrete use of GitHub repo.

Setup the environment loading all the necessary packages
```{r packages, include=FALSE}
set.seed(123)
library(ggplot2)
library(ggjoy)
library(dplyr)
library(forcats)
library(reshape2)
library(tidyr)
```
# Data import

Load data and fill all NAs with 0. Then, see data structure and summary
```{r load_data,echo=FALSE}
df <- read.csv("C://Users//andrea.ricca//Downloads//DA_GTEL_SRVGestorePreventivo_count_errors.csv",
         sep = ",",header = T)
df$date = as.POSIXct(df$date, format = "%Y/%m/%d %H")
df[is.na(df)] <- 0
levels(df$root) <- "TSVariable"
str(df)
summary(df)

```

# Visualisation

Joyplot per weekday and hour:
Reshape the dataframe in order to visualize data as expected from ggplot.

Distribution per weekday:

```{r ggjoy_weekday}
df2 <- df %>% 
  gather(count, sum_error, key = "Variable", value = "Data") %>%
  arrange(date_wday,date)

df2%>% 
  ggplot(aes(y = as.factor(date_wday) %>% fct_rev())) +
  geom_joy(aes(x = Data, fill = Variable, alpha = 0.8)) +
  scale_fill_manual(values = c("Red", "Blue")) +
  labs(x = "Density",
       y = "Weekday",
       title = "Count of Volumes and Errors: different distribution by weekday",
       subtitle = "Volumes (Red) and Errors (Blue)")

```

Distribuzion per hour:

```{r ggjoy_hour}
df3 <- df %>% 
  gather(count, sum_error, key = "Variable", value = "Data") %>%
  arrange(date_hour,date) 
df3 %>% 
  ggplot(aes(y = as.factor(date_hour) %>% fct_rev())) +
  geom_joy(aes(x = Data, fill = Variable, alpha = 0.8)) +
  scale_fill_manual(values = c("Red", "Blue")) +
  labs(x = "Density",
       y = "Hour",
       title = "Count of Volumes and Errors: different distribution by hour",
       subtitle = "Volumes (Red) and Errors (Blue)")
```

