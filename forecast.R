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
 
data_ex <- as.data.frame(subset(data$Usage,index(data) < "2019-01-01"))
data_control <- as.data.frame(subset(data$Usage,index(data) >= "2019-01-01"))


data_ts1 <- ts(data_ex$Usage,start = min(index(data_ex),end = max(index(data_ex))),frequency = 7)


#拆解 <- decompose(data_ts1)
#plot(拆解)

adf.test(data_ts1) 
#因為非stationary，故要進行lambda轉換(轉換後還是不穩定，後來使用轉換又差分的方式)
lambda轉換 <- BoxCox(data_ts1,lambda = "auto")
#轉換值
BoxCox.lambda(lambda轉換) #0.7053499
#差分且轉換就過了(p-value < 0.05)
#有足夠的證據去推翻不穩定的假設
差分且轉換 <- diff(BoxCox(data_ts1,lambda = "auto"),differences = 1)
adf.test(差分且轉換)


#建模
#order(AR,I,MA)(S)
#SARI模型
auto.arima(lambda轉換,stepwise = F,trace = T,stationary = T,ic = c("aic"))
fit <- arima(lambda轉換,order = c(1,1,0),seasonal = list(order = c(2,0,0),period = 7),include.mean = FALSE) #SARMA


#模型檢查
tsdisplay(residuals(fit),lag.max = 50,main = '殘差')
#不符合常態分布
#https://officeguide.cc/r-normality-test-tutorial/
shapiro.test(fit$residuals)
#顯著性值少於 0.05 表示殘差誤不是隨機的，暗示在觀察數列中有模型未說明的結構。
#https://www.ibm.com/docs/zh-tw/spss-modeler/SaaS?topic=node-examining-model
Box.test(fit$residuals,lag = 14,type = "Ljung-Box")


#預測誤差及討論空間
#lambda填上lambda轉換次數+1
p <- forecast(fit,100,lambda = 0.7053499)
p
plot(p)

預測 <- as.data.frame(p)
評估 <- cbind(p,data_control[-c(425),])
colnames(評估) <- c("test","Usage")
評估 <- 評估 %>%
  mutate(mae = abs(Usage - 評估$`Point Forecast`)) %>%
  mutate(mape = abs(Usage - 評估$`Point Forecast`)/Usage)
mean(評估)  
