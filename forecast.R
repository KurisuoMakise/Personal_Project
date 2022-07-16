getwd()
setwd("D:/NCTU_NOT_NYCU/Personal_Project/2011-2022data_process")
#https://medium.com/r-%E8%AA%9E%E8%A8%80%E8%87%AA%E5%AD%B8%E7%B3%BB%E5%88%97/r%E8%AA%9E%E8%A8%80%E8%87%AA%E5%AD%B8%E7%B3%BB%E5%88%97-4-%E8%B3%87%E6%96%99%E6%B8%85%E6%B4%97%E8%88%87%E6%8E%A2%E7%B4%A2%E6%80%A7%E8%B3%87%E6%96%99%E5%88%86%E6%9E%90-68a2f75770ed

library(tidyverse)
library(tseries)
library(forecast)#arima
library(dplyr)
library(lubridate) #提取月份
library(xts) #as.xts

data <- readRDS("data.rds")
df <- as.data.frame(data)

for(i in 1:1461){
  if(is.nan(df$Usage[i])){
    a <- 1
    while(is.nan(df$Usage[i+a])){
      a <- a + 1
    }
    print(df$Usage[i])
    df$Usage[i] <- (df$Usage[i-1] + df$Usage[i+a])/2
    print(df$Usage[i])
  }
}

