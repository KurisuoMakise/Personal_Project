library(forecast)
library(tseries)
library(dplyr)
library(lubridate) #提取月份
library(xts) #as.xts

getwd()
setwd("D:/NCTU_NOT_NYCU/Personal_Project/2011-2022data_process")

#data處理#####################################
#data <- readRDS("data_20162021_hourly.rds")

#data <- data %>%
  #filter(year(datetime) >= "2017") %>%
  #filter(year(datetime) <= "2020")

#data <- xts(x = data$工程二館,order.by=data$datetime)

#colnames(data) <- c("load")

#data <- apply.daily(data,colSums,na.rm = TRUE)
#saveRDS(data,"data.rds")
#data <- readRDS("data.rds")

#data_time <- seq(as.Date("2017-01-01"),length = 365*4+1,by = "days")
#datas <- as.numeric(data$load)
#data_time

#data <- as.xts(datas,order.by=data_time)
#colnames(data) <- c("Usage")
#saveRDS(data,"data.rds")
##############################################

#資料呼叫 and 分類############################
data <- readRDS("data.rds")

control <- subset(data,year(index(data)) >= "2019")

test <- subset(data,year(index(data)) < "2019")

#https://stackoverflow.com/questions/33128865/starting-a-daily-time-series-in-r 
test <- ts(test,frequency = 365)
test
##############################################


####初步觀察資料##############################
part <- decompose(test)
plot(part)
##############################################


#檢查是否穩定#################################
#單根檢定 (要小於0.05)
#adf.test(test)
#lamda_transfer <- BoxCox(test,lambda = "auto")
#BoxCox.lambda(lamda_transfer)
#adf.test(lamda_transfer)

#轉換還是不穩定，只能差分
adf.test(test)
#difference <- diff(test,differences = 1)
#adf.test(difference)
##############################################

#建模#########################################
#沒轉換就擺原資料，差分是之後會要加別的參數
auto.arima(test,stepwise = F,trace = T,stationary = T,ic=c("aic"))

fit <- arima(test,order = c(1,0,0),include.mean = FALSE)
#order(ar,i,ma)
##############################################

#檢查模型#####################################
tsdisplay(residuals(fit),lag.max = 50,main = "殘差")

shapiro.test(fit$residuals)

Box.test(fit$residuals,lag = 10,type = "Ljung-Box")
##############################################




