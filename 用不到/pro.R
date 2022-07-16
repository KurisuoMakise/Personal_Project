getwd()
setwd("D:/NCTU_NOT_NYCU/Personal_Project/2011-2022data_process")


library(tidyverse)
library(forecast) #AUTO-ARIMA
library(tseries) #adf kpss test
library(dplyr) # mutate %>%
library(xts) 
library(mice) #找缺失值(條件設0，因為耗電不應該為0)
library(zoo) #插值
 
Sys.setenv(TZ="Asia/Taipei")                                 #設定時間
par(family="Noto Sans CJK TC Black")                         #設定中文字型
par(family="Consolas")                                           #設定英文字型

#讀取資料
#xts_2011_to_2015 <- read_rds("data_20112015_hourly.rds")
#sapply(xts_2011_to_2015,function(x)sum(x == 0))
#xts_2016_to_2021 <- read_rds("data_20162021_hourly.rds")
#sapply(xts_2016_to_2021,function(x)sum(x == 0))


####################################################################
#實驗組and對照組
#########################################################################################################start1
xts_EB2 <- read_rds("xts_EB2.rds")
#####################實驗組處理資料
#for(i in 1:length(xts_EB2$Usage)){
#  if(xts_EB2$Usage[i] < 20){
#    xts_EB2$Usage[i] = mean(xts_EB2$Usage)
#  }
#}
#dygraphs::dygraph(xts_EB2)
#plot(na.approx(xts_EB2$Usage))
#saveRDS(object = xts_EB2, file = "xts_EB2.rds")
#####################


EB2_monthly <- apply.monthly(xts_EB2,colSums, na.rm=TRUE)
EB2_monthly_ts <- ts(EB2_monthly,frequency = 12,start = c(2011,01))
##########
core <- decompose(EB2_monthly_ts)
plot(core)
##########
#########################################################################################################start2
xts_EB2_2016_to_2021_control_group <- read_rds("xts_EB2_2016_to_2021_control_group.rds")
#dygraphs::dygraph(xts_EB2_2016_to_2021_control_group)
##########對照組處理
#for(i in 1:length(xts_EB2_2016_to_2021_control_group$Usage)){
#  if(xts_EB2_2016_to_2021_control_group$Usage[i] < 0 || xts_EB2_2016_to_2021_control_group$Usage[i] > 150){
#    xts_EB2_2016_to_2021_control_group$Usage[i] = mean(xts_EB2_2016_to_2021_control_group$Usage)
#  }
#}
#for(i in 1:length(xts_EB2_2016_to_2021_control_group$Usage)){
#  if(xts_EB2_2016_to_2021_control_group$Usage[i] == 0 || xts_EB2_2016_to_2021_control_group$Usage[i] > 60){
#    xts_EB2_2016_to_2021_control_group$Usage[i] = NA
#  }
#}
#saveRDS(object = xts_EB2_2016_to_2021_control_group, file = "xts_EB2_2016_to_2021_control_group.rds")
#####################


EB2_monthly_control <- apply.monthly(xts_EB2_2016_to_2021_control_group,colSums, na.rm=TRUE)
EB2_monthly_ts_control <- ts(EB2_monthly_control,frequency = 12,start = c(2016,01))
####################################################################





####################################################################

#################################################################################
#檢測資料符不符合模型使用的前提
#SARIMA
#穩定性:均數和方差在一定範圍下的移動
#檢定方法:ADF test 單根檢定

#對立假設放穩定:p-value若沒有小於0.05，則意思是沒有足夠證據去推翻不穩定的假設
#兩種做法變穩定:差分，轉換(優先選擇轉換)
adf.test(EB2_monthly_ts)


#################################################################################



#############################
####建模
#############################

#幫我們找模型參數
#ic 比對模型好壞的指標
#trace 把尋找的過程列出來
auto.arima(EB2_monthly_ts,stepwise = F,trace = T,stationary = T,ic = c("aic"))
#建立模型
#oreder 第二個參數表示差分幾次
#SARIMA = AR模型 + 差分 + MA模型 + 季節性
#"ARMA"
fit <- arima(EB2_monthly_ts,order=c(2,0,1),include.mean = TRUE)


#############################
####模型檢查
#############################

#畫圖看一下 
#main = 圖的標題 lag.max看幾筆資料
tsdisplay(residuals(fit),lag.max = 50,main = "殘差")

#殘差是否常態
#shapiro看的是殘差是否常態(p-value要大於0.05)
#p-value要大於0.05(這裡常態檢定沒過)
shapiro.test(fit$residuals)

#殘差之間是否獨立
#box.test 看的是殘差之間是否獨立(p-value要大於0.05)
#p-value要大於0.05(這裡獨不獨立檢定有過)
#box.test裡面的lag如果是非季節性寫10，若有的   話寫週期*2
Box.test(fit$residuals,lag = 12*2,type = "Ljung-Box")




#預測及檢討空間
#沒有經過lambda轉換，lambda填1
#中間的參數是指想要預測幾期
p <- forecast(fit,3,lambda = 1)
#point Forcast指的是我們預測的值(80%信心水準下的區間值和95%信心水準下的區間值)
p
plot(p)



預測 <- as.data.frame(p)
評估 <- cbind(預測,EB2_monthly_ts_control)


# mae
# mape
評估 <- 評估 %>%
  mutate(mae=abs(Usage - 評估$`Point Forecast`)) %>%
  mutate(mape=abs(Usage - 評估$`Point Forecast`)/Usage)
mean(評估$mape)
