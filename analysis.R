library(xts)
library(forecast)
library(ggplot2)

install.packages("imputeTS")

library(imputeTS)

library(xlsx)

new_data<-read.xlsx("orig_data_test.xlsx",sheetIndex = 1)

head(new_data)

getwd()



new_data

new_data$OrderQty<-na.kalman(new_data$OrderQty)

setwd("X:/Data for Forecasting/New")
library(xlsx)

install.packages("smooth")
library(smooth)

data_1<-read.xlsx("Dat4.xlsx",sheetIndex = 1)
data_2<-read.xlsx("Dat5.xlsx",sheetIndex = 1)
data_3<-read.xlsx("Dat6.xlsx",sheetIndex = 1)
data_4<-read.xlsx("Dat7.xlsx",sheetIndex = 1)
data_5<-read.xlsx("Dat8.xlsx",sheetIndex = 1)
data_6<-read.xlsx("Dat9.xlsx",sheetIndex = 1)

data_anast$Amount<-as.numeric(data_anast$Amount)


head(data_4)
tail(data_4)
ts <- ts(data_anast$Amount, as.Date(data_anast$Date, "%m/%d/%Y"))

head(data_6)
tail(data_6)

ts_o<- ts(data_5$OrderQty,start=c(2014,1,28),end=c(2017,11,21),frequency=365)

model_hw<-HoltWinters(ts_o)
fore<-forecast(model_hw,h=365)
write.csv(fore,file="dat9_forecast.csv")


plot(ts_o)


date <- as.Date(data_4$OrderDate, "%m/%d/%Y")
alldates <- seq(min(date), max(date), 1)
dates0 <- alldates[!(alldates %in% date)]

data<-data_4$OrderQty

mydata0 <<- data.frame(date = dates0, data = NA_real_)
mydata_final <<- rbind(cbind(date,data), mydata0)
mydata_final <- mydata_final[order(mydata_final$date),]
ts_o<- ts(mydata_final$data,start=c(2013,10,22),end=c(2017,11,22),frequency=365)


#ts_mydata <- ts(mydata_final$data, start = start.date, end = end.date, frequency = freq)
ts_mydata <- tsclean(ts_o)

tail(data_4$OrderDate)


model_1<-auto.arima(ts_o)
model_2<-arima(ts_o,order = c(0,1,20))
model_3<-holt(ts_o)
model_4<-spline(ts_o)
model_5<-splinef(ts_o,h=365)
model_6<-es(ts_o)
model_7<-HoltWinters(ts_o)
plot(model_5)
plot(forecast(model_7,h=365))





ts_o
ts_int<-na.interp(ts)

ts

plot(ts_m)

ts_q <- xts(data_anast$Quantity, as.Date(data_anast$Date, "%m/%d/%Y"))

data_anast
# convert daily data
ts_w = apply.weekly(ts,sum)
ts_m = apply.monthly(ts, sum)
ts_y = apply.yearly(ts, FUN)
ts_q = apply.quarterly(ts, FUN)

model_ma<-ma(ts_m,order = 10)

model_ar<-arima(ts,order = c(0,0,10))

plot(forecast(model_ar,h=10))

ts_m_q = apply.monthly(ts_q, sum)

plot(decompose(ts_m_q))

model_ar_q<-auto.arima(ts_m)

model_h<-holt(ts_m_q)

autoplot(forecast(model_ar,h=5))

model_ar<-auto.arima(ts_m)



model_ar_w<-auto.arima(ts_w)



#model_spline<-spline(ts_m)

#model_ses<-ses(ts_m)

model_holt<-holt(ts_m)

autoplot(forecast(model_ar_w,h=5))

autoplot(forecast(model_ar,h=5))

autoplot(forecast(model_ar_m,h=5))

autoplot(forecast(model_holt,h=5))

final_result<-forecast(model_ar,h=5)

final_result


library(xlsx)

data_loloi_comp<-read.csv("Loloi_Shipments_v2.csv")
head(data_loloi_comp)

data_loloi_comp$Amount<-as.numeric(as.character(data_loloi_comp$Amount))

ts_l <- xts(data_loloi_comp$Amount, as.Date(data_loloi_comp$Date, "%m/%d/%Y"))

ts_l_q <- xts(data_loloi_comp$Quantity, as.Date(data_loloi_comp$Date, "%m/%d/%Y"))
ts_m<-apply.monthly(ts_l_q,sum)
ts_m_q<-apply.monthly(ts_l_q,sum)

ts_q = apply.quarterly(ts_l, sum)

autoplot(ts_m)

plot(ts_m)

plot(ts_m_q)

model_am<-ses(ts_m)
autoplot(forecast(model_am,h=10))


comb_data<-read.csv("combined_data.csv")

comb_data$Amount<-as.numeric(as.character(comb_data$Amount))

ts_a <- xts(comb_data$Amount, as.Date(comb_data$Date, "%m/%d/%Y"))
ts_q <- xts(comb_data$Quantity, as.Date(comb_data$Date, "%m/%d/%Y"))

ts_m_c = apply.monthly(ts_a, sum)



plot(ts_m_c)

ts_m<-apply.monthly(ts_l,sum)
ts_m_q<-apply.monthly(ts_l_q,sum)

model_new<-arima(ts_m_new,order=c(1,1,0))

autoplot(forecast(model_new,h=6))

setwd("X:/Data for Forecasting")
mom_data<-read.csv("momeni_shipment_v2.csv")

ts_q <- xts(mom_data$Qty, as.Date(mom_data$Date, "%m/%d/%Y"))
ts_a<-xts(mom_data$Amount, as.Date(mom_data$Date, "%m/%d/%Y"))

ts_m<-apply.monthly(ts_a,sum)

plot(ts_m)

model_mom<-rwf(ts_m)

plot(model_mom)

plot(forecast(model_mom,h=10))


mom_po<-read.csv("momeni-PO.csv")

str(mom_po)
ts_q <- xts(mom_po$Quantity, as.Date(mom_po$Date, "%m/%d/%Y"))
ts_m_2<-apply.monthly(ts_q,sum)
plot(ts_m_2)

model_mom_q<-ma(ts_m_2,order=12,centre=FALSE)
model_mom_2<-holt(ts_m_2)
x11()

plot(forecast(model_mom_q,h=10))


loloi_po<-read.csv("Loloi-PO.csv")

head(loloi_po)
str(loloi_po)


#somewhat accurate
ts_q_l <- xts(loloi_po$Quantity, as.Date(loloi_po$Date, "%m/%d/%Y"))

ts_a_l <- xts(loloi_po$Amount, as.Date(loloi_po$Date, "%m/%d/%Y"))

ts_m<-apply.monthly(ts_q_l,sum)
ts_a_m<-apply.monthly(ts_a_l,sum)

plot(ts_m)

plot(ts_a_m)

model_loloi_po<-auto.arima(ts_m)
model_loloi_po_am<-rwf(ts_a_m)

s<-stlf(ts_m)

plot(forecast(model_loloi_po,h=10))
plot(forecast(model_loloi_po_am,h=10))

data_saf<-read.csv("safaviah-PO.csv")

ts_q_s <- xts(data_saf$Quantity, as.Date(data_saf$Date, "%m/%d/%Y"))

ts_m_s<-apply.monthly(ts_q_s,sum)
plot(ts_m_s)

model_s<-auto.arima(ts_m_s)

plot(forecast(model_s,h=10))

sales_data<-read.csv("sample_data_3.csv")
head(sales_data)

str(sales_data)

class(sales_data$OrderDate)


head(as.Date(sales_data$OrderDate,"%Y-%m-%d"))

sales_data$InvoiceDate<-as.Date(sales_data$InvoiceDate,"%Y-%m-%d")


data_fore<-data.frame(Invoice_Date=sales_data$InvoiceDate,Total_Amount=sales_data$TotalAmount)

nrow(data_fore)


sort(data_fore$Order_Date)

library(forecast)

head(data_fore)

ts_s <- xts(sales_data$TotalAmount, as.Date(sales_data$Date, "%m/%d/%Y"))

plot(ts_m)

ts_m<-apply.monthly(ts_s,sum)

ts_y<-apply.yearly(ts_s,sum)

ts_q<-apply.quarterly(ts_s,sum)

plot(ts_q)

model_yearly<-arima(ts_m,order=c(0,1,4))

plot(forecast(model_yearly,h=10))

model_lol_sales<-auto.arima(fore)


plot(forecast(model_lol_sales,h=11))

data_fore[]
