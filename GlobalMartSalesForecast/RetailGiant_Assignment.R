### Loading necessary packages################


required_packages <- c("dplyr","tidyr","ggplot2","stringr","forecast","rstudioapi")
lapply(required_packages, require, character.only = TRUE)

####setting source directory as working directory#####

sourcedir <- getActiveDocumentContext()$path 
setwd(dirname(sourcedir))

####Reading the file and viewing the data #####

superstore_data <- read.csv('Global Superstore.csv')
View(superstore_data)

#####EDA and Data Cleaning##############
str(superstore_data)
summary(superstore_data)

##No Duplicate data#####
sum(duplicated(superstore_data)) 

### Finding NA- Only Postal code has NA

na_count <- data.frame(sapply(superstore_data,function(x) sum(length(which(is.na(x))))))
na_count$column <- row.names(na_count)
colnames(na_count) <- c('count','colname')
na_count <- na_count[which(na_count$count >0),]
View(na_count)



###Converting Date to "%d-%m-%Y" format

superstore_data$Order.Date <- as.Date(superstore_data$Order.Date, "%d-%m-%Y")
superstore_data$Ship.Date <- as.Date(superstore_data$Ship.Date, "%d-%m-%Y")

#Checking Order Dates duration

summary(superstore_data$Order.Date)


###################################Data Preparation ###################

#Step 1:Deleting Postal.Code as it contain NA values and its unnecessary for forcasting

superstore_data <- subset(superstore_data,TRUE,-c(Postal.Code))

#Step 2:Merging multiple columns to one

superstore_data$Market_Segment <- paste(superstore_data$Market, superstore_data$Segment, sep = "_")
superstore_data$Year_Month <- format(as.Date(superstore_data$Order.Date), "%Y-%m")

#Step 3: Subsetting data into buckets

invisible(lapply(split(superstore_data, superstore_data$Market_Segment), function(x) {assign(paste(x$Market_Segment[1]), x, pos = .GlobalEnv)}))

#Step 4: Aggregating market buckets  order date by sales, quantity & profit
#Aggregating data by Sales, Profit & Quantity & CV

data_aggregated <- superstore_data %>%  group_by(Market_Segment) %>% 
                  summarise(.,sum(Sales),sum(Profit),sum(Quantity),sd(Profit)*100/mean(Profit))  
colnames(data_aggregated) <- c("Market_Segment","Total_Sales","Total_Profit","Total Quantity","CV")
View(data_aggregated)

#monthly aggregation on each atribute

data_aggregated_monthly <- superstore_data %>% group_by(Market_Segment, Year_Month) %>% summarise(.,sum(Sales),sum(Profit),sum(Quantity),sd(Profit)*100/mean(Profit))
colnames(data_aggregated_monthly) <- c("Market_Segment","Order_Date","Total_Sales","Total_Profit","Total Quantity","CV")
View(data_aggregated_monthly)

# Plotting the data 

sales_profit <- superstore_data[,c("Profit","Sales","Market","Segment","Quantity")] %>% group_by(Market,Segment) %>% dplyr::summarise(., sum(Sales),sum(Profit),sd(Profit)*100/mean(Profit))
colnames(sales_profit) = c("Market","Segment","Sales","Profit","CV")
View(sales_profit)


ggplot(sales_profit, aes(Segment, Sales, fill=Market)) + geom_bar(position = "dodge",stat = "identity") 
ggplot(sales_profit, aes(Segment, Profit, fill=Market)) + geom_bar(position = "dodge",stat = "identity")
ggplot(sales_profit, aes(Segment, CV, fill=Market)) + geom_bar(position = "dodge",stat = "identity")


#Based on CV & Profits APAC_Consumer & EU_Consumer are 2 markets with highest Profit & lowest CV 

#---------------------------------------------------------Model Building------------------------------------------------------------------

#Selecting subset for EU_Consumer and APAC_Consumer

best_two <- subset(data_aggregated_monthly, Market_Segment == "EU_Consumer" | Market_Segment == "APAC_Consumer")
str(best_two)

#Changing column names

names(best_two) <- c("Market_Segment", "Order_Month", "Sales", "Profit", "Quantity", "CV")
View(best_two)

#Getting Top 2 Market Segments

invisible(lapply(split(best_two, best_two$Market_Segment), function(x) {assign(paste0("Top_", x$Market_Segment[1]), x, pos = .GlobalEnv)}))

APAC_Consumer_Agg <- Top_APAC_Consumer[,c(2:5)]
EU_Consumer_Agg <- Top_EU_Consumer[,c(2:5)]


###-------------------------------------------------- Forecasting APAC Consumer Sales-------------------------------------------

#Creating APAC sales timeseries

APAC_Consumer_Sales_TS <- ts(APAC_Consumer_Agg$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_Consumer_Sales_TS)

#Testing Holt winters for Smoothing

plot(APAC_Consumer_Sales_TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(APAC_Consumer_Sales_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)
smoothedseries <- HoltWinters(APAC_Consumer_Sales_TS, alpha=.3,beta=FALSE, gamma=FALSE)
plot(smoothedseries)


#Smoothing time series using Moving Average method

w <- 4
APAC_Consumer_Sales_Smoothed <- stats::filter(APAC_Consumer_Sales_TS,filter=rep(1/w,w,method='convolution',sides=2)) 
#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- APAC_Consumer_Sales_Smoothed[w+2] - APAC_Consumer_Sales_Smoothed[w+1]
View(diff)
for (i in seq(w,1,-1)) {
  APAC_Consumer_Sales_Smoothed[i] <- APAC_Consumer_Sales_Smoothed[i+1] - diff
}

## Right end smoothing

n <- length(APAC_Consumer_Sales_Smoothed)
diff <- APAC_Consumer_Sales_Smoothed[n-w] - APAC_Consumer_Sales_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_Consumer_Sales_Smoothed[i] <- APAC_Consumer_Sales_Smoothed[i-1] + diff
}

View(APAC_Consumer_Sales_Smoothed)

#Plotting smoothed series

plot(APAC_Consumer_Sales_TS)
lines(APAC_Consumer_Sales_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='red',lwd=2)

# Above plots indicate Moving average result in better smoothing and more matching plot than the Holt and Winters 
#Converting smoothed series to data frame with Order_Month & renaming columns 

APAC_Consumer_Sales_df <- data.frame(cbind(APAC_Consumer_Agg$Order_Month,APAC_Consumer_Sales_Smoothed))
colnames(APAC_Consumer_Sales_df) <- c("Month","Sales")
View(APAC_Consumer_Sales_df)

#Changing Sales type

APAC_Consumer_Sales_df$Sales <- as.numeric(as.character((APAC_Consumer_Sales_df$Sales)))

#Creating time series from dataframe

APAC_Consumer_Sales_TS <- ts(APAC_Consumer_Sales_df$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_Consumer_Sales_TS)

#Creating train & validation Data

testwindow <- 6
nTrain <- length(APAC_Consumer_Sales_TS)-testwindow
train_ts <- window(APAC_Consumer_Sales_TS,start = c(2011,1),end=c(2011,nTrain))
valid_ts <- window(APAC_Consumer_Sales_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+testwindow))
test_ts <- ts(APAC_Consumer_Sales_df$Sales,frequency=12,start=c(2015,1),end=c(2015,6))

#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)

train_lm <- tslm(train_ts~trend+I(sin(2*pi*trend/12))+season)
summary(train_lm)

train_lm_forecast <- forecast(train_lm,h=testwindow,level=0)
plot(APAC_Consumer_Sales_Smoothed,ylab="Sales",xlab="Time",bty='l',xaxt='n',col="red",title=c("APAC Consumer Sales"))  
lines(train_lm_forecast$fitted,lwd=2,col="black",lty=3)
lines(train_lm_forecast$lower,col="blue")

#Checking MAPE for our linear model

APAC_consumer_accuracy <- accuracy(train_lm_forecast$mean,valid_ts)  

APAC_consumer_accuracy #MAPE=13.54141

#ACF and PACF plots 

acf(train_lm_forecast$residuals,lag.max = 12)
pacf(train_lm_forecast$residuals,lag.max = 12)


#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train_ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=testwindow)
autoarima_acc <- accuracy(autoarima_forecast,valid_ts)
plot(autoarima_forecast)
autoarima_acc

#MAPE=5.52011 for Training set & 14.48387 for Validation set for Auto ARIMA

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model

autoarima_ts <- auto.arima(APAC_Consumer_Sales_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=testwindow)
plot(autoarima_forecast)


###--------------------------------------------------APAC Consumer Quantity Forecast--------------------------------------------

#Creating APAC Quantity timeseries

APAC_Consumer_Quantity_TS <- ts(APAC_Consumer_Agg$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_Consumer_Quantity_TS)

#Testing Holt winters for Smoothing

plot(APAC_Consumer_Quantity_TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")

for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(APAC_Consumer_Quantity_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#Smoothing time series using Moving Average method

w <- 4
APAC_Consumer_Quantity_Smoothed <- stats::filter(APAC_Consumer_Quantity_TS,filter=rep(1/w,w,method='convolution',sides=2)) 

#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series

diff <- APAC_Consumer_Quantity_Smoothed[w+2] - APAC_Consumer_Quantity_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  APAC_Consumer_Quantity_Smoothed[i] <- APAC_Consumer_Quantity_Smoothed[i+1] - diff
}

#Smoothing right end of the time series
n <- length(APAC_Consumer_Quantity_Smoothed)
diff <- APAC_Consumer_Quantity_Smoothed[n-w] - APAC_Consumer_Quantity_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_Consumer_Quantity_Smoothed[i] <- APAC_Consumer_Quantity_Smoothed[i-1] + diff
}

#Converting smoothed series to data frame with Order_Month & renaming columns
#Using moving average smoothed time series here

APAC_Consumer_Quantity_df <- data.frame(cbind(APAC_Consumer_Agg$Order_Month,APAC_Consumer_Quantity_Smoothed))
colnames(APAC_Consumer_Quantity_df) <- c("Month","Quantity")

#Changing Quantity type

APAC_Consumer_Quantity_df$Quantity <- as.numeric(as.character((APAC_Consumer_Quantity_df$Quantity)))

#Plotting smoothed series
plot(APAC_Consumer_Quantity_TS)
lines(APAC_Consumer_Quantity_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='red',lwd=2)

#Again creating time series from dataframe

APAC_Consumer_Quantity_TS <- ts(APAC_Consumer_Quantity_df$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))

#Creating train & validation sets

ntest <- 6
nTrain <- length(APAC_Consumer_Quantity_TS)-ntest
train_ts <- window(APAC_Consumer_Quantity_TS,start = c(2011,1),end=c(2011,nTrain))
valid_ts <- window(APAC_Consumer_Quantity_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test_ts <- ts(APAC_Consumer_Quantity_df$Quantity,frequency=12,start=c(2015,1),end=c(2015,6))

#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)

train_lm <- tslm(train_ts~trend+I(sin(2*pi*trend/12))+season)
summary(train_lm)
train_lm_forecast <- forecast(train_lm,h=ntest,level=0)
plot(APAC_Consumer_Quantity_Smoothed,ylab="Quantity",xlab="Time",bty='l',xaxt='n',col="red")  
lines(train_lm_forecast$fitted,lwd=2,col="black",lty=3)
lines(train_lm_forecast$lower,col="blue")

#Checking MAPE for our linear model

APAC_consumer_accuracy <- accuracy(train_lm_forecast$mean,valid_ts)  
APAC_consumer_accuracy #MAPE=9.580049

#ACF and PACF plots 

acf(train_lm_forecast$residuals,lag.max = 12)
pacf(train_lm_forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model

autoarima_ts <- auto.arima(train_ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid_ts)
plot(autoarima_forecast)
autoarima_acc

#MAPE=4.80180 for Training set & 10.36941 for Validation set for Auto ARIMA

#Forecast for months 49 to 54(Jan 2015 to June 2015) using ARIMA model
autoarima_ts <- auto.arima(APAC_Consumer_Quantity_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)


###--------------------------------------------------EU Consumer Sales Forecast--------------------------------------------
#Creating EU Sales timeseries
EU_Consumer_Sales_TS <- ts(EU_Consumer_Agg$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Consumer_Sales_TS)

#Testing Holt winters for Smoothing

plot(EU_Consumer_Sales_TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(EU_Consumer_Sales_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#Smoothing time series using Moving Average method

w <- 5
EU_Consumer_Sales_Smoothed <- stats::filter(EU_Consumer_Sales_TS,filter=rep(1/w,w,method='convolution',sides=2)) 
#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- EU_Consumer_Sales_Smoothed[w+2] - EU_Consumer_Sales_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  EU_Consumer_Sales_Smoothed[i] <- EU_Consumer_Sales_Smoothed[i+1] - diff
}
#Smoothing right end of the time series

n <- length(EU_Consumer_Sales_Smoothed)
diff <- EU_Consumer_Sales_Smoothed[n-w] - EU_Consumer_Sales_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_Consumer_Sales_Smoothed[i] <- EU_Consumer_Sales_Smoothed[i-1] + diff
}

#Converting smoothed series to data frame with Order_Month & renaming columns
#Using moving average smoothed time series here
EU_Consumer_Sales_df <- data.frame(cbind(EU_Consumer_Agg$Order_Month,EU_Consumer_Sales_Smoothed))
colnames(EU_Consumer_Sales_df) <- c("Month","Sales")

#Changing Sales type
EU_Consumer_Sales_df$Sales <- as.numeric(as.character((EU_Consumer_Sales_df$Sales)))

#Plotting smoothed series
plot(EU_Consumer_Sales_TS)
lines(EU_Consumer_Sales_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='red',lwd=2)

#Again creating time series from dataframe
EU_Consumer_Sales_TS <- ts(EU_Consumer_Sales_df$Sales,frequency=12,start=c(2011,1),end=c(2014,12))

#Creating train & validation sets
ntest <- 6
nTrain <- length(EU_Consumer_Sales_TS)-ntest
train.ts <- window(EU_Consumer_Sales_TS,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_Consumer_Sales_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(EU_Consumer_Sales_df$Sales,frequency=12,start=c(2015,1),end=c(2015,6))

#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)

train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(EU_Consumer_Sales_Smoothed,ylab="Sales",xlab="Time",bty='l',xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")

#Checking MAPE for our linear model
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_consumer_accuracy #MAPE=13.80817

#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc

#MAPE=5.098584 for Training set & 3.343440 for Validation set for Auto ARIMA

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(EU_Consumer_Sales_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)


###--------------------------------------------------EU Consumer Quantity Forecast--------------------------------------------
#Creating EU Quantity timeseries
EU_Consumer_Quantity_TS <- ts(EU_Consumer_Agg$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Consumer_Quantity_TS)

#Testing Holt winters for Smoothing
plot(EU_Consumer_Quantity_TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(EU_Consumer_Quantity_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#Smoothing time series using Moving Average method

w <- 3
EU_Consumer_Quantity_Smoothed <- stats::filter(EU_Consumer_Quantity_TS,filter=rep(1/w,w,method='convolution',sides=2)) 
#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- EU_Consumer_Quantity_Smoothed[w+2] - EU_Consumer_Quantity_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  EU_Consumer_Quantity_Smoothed[i] <- EU_Consumer_Quantity_Smoothed[i+1] - diff
}
#Smoothing right end of the time series
n <- length(EU_Consumer_Quantity_Smoothed)
diff <- EU_Consumer_Quantity_Smoothed[n-w] - EU_Consumer_Quantity_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_Consumer_Quantity_Smoothed[i] <- EU_Consumer_Quantity_Smoothed[i-1] + diff
}

#Converting smoothed series to data frame with Order_Month & renaming columns
#Using moving average smoothed time series here
EU_Consumer_Quantity_df <- data.frame(cbind(EU_Consumer_Agg$Order_Month,EU_Consumer_Quantity_Smoothed))
colnames(EU_Consumer_Quantity_df) <- c("Month","Quantity")

#Changing Quantity type
EU_Consumer_Quantity_df$Quantity <- as.numeric(as.character((EU_Consumer_Quantity_df$Quantity)))

#Plotting smoothed series
plot(EU_Consumer_Quantity_TS)
lines(EU_Consumer_Quantity_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='red',lwd=2)

#Again creating time series from dataframe
EU_Consumer_Quantity_TS <- ts(EU_Consumer_Quantity_df$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))

#Creating train & validation sets
ntest <- 6
nTrain <- length(EU_Consumer_Quantity_TS)-ntest
train.ts <- window(EU_Consumer_Quantity_TS,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_Consumer_Quantity_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(EU_Consumer_Quantity_df$Quantity,frequency=12,start=c(2015,1),end=c(2015,6))

#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(EU_Consumer_Quantity_Smoothed,ylab="Quantity",xlab="Time",bty='l',xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")

#Checking MAPE for our linear model
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_consumer_accuracy #MAPE=13.72469

#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model

autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc

#MAPE=6.244317 for Training set & 19.685536 for Validation set for Auto ARIMA

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(EU_Consumer_Quantity_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto Regression model
train.lm.model <- tslm(EU_Consumer_Quantity_TS~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=0)
train.lm.total.forecast
plot(train.lm.total.forecast,col="black")
