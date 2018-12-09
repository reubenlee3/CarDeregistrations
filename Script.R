#Cat B analysis for now
setwd("~/Desktop/COE dereg/New set")

library(data.table)
library(forecast)
library(tseries)
library(Metrics)
library(ggplot2)
library(caret)
library(e1071)
library(randomForest)

#load the dataset and create a column for dates
dereg <- fread("car_dereg.csv")
dereg$date <- seq(as.Date("1990/05/01"),by = "month",length.out = nrow(dereg))

#split the dateset to take only the relevant details and visually inspect the time series
dereg_ts <- dereg[,c(4,5)]

#plot the initial time series for visual inspection
ggplot(dereg_ts,aes(x = date,y = Catb_clean)) + geom_line() + scale_x_date("month") + ylab("Cat B vehicle Deregistration") + xlab("") + geom_vline(xintercept = as.Date("2018-04-01"),linetype = "dashed",colour="purple", size = 1)

#first Augmented Dicky Fuller test, acf and pacf plot 
par(mfrow = c(2,1))
acf(dereg_ts$Catb_clean,lag.max = 50,main ="")
pacf(dereg_ts$Catb_clean,lag.max = 50,main="")
adf.test(dereg_ts$Catb_clean)

#splitting into train and test set for ARIMA model
train_ts_arima <- dereg_ts[date < ymd("2018-04-01"),]
test_ts_arima <- dereg_ts[date >= ymd("2018-04-01"),]

#Augmented Dicky Fuller test, acf and pacf plot after first differencing
train_ts_arima$Catb_diff_1 <- diff(train_ts_arima$Catb_clean,differences = 1)
acf(train_ts_arima$Catb_diff_1,lag.max = 30,main ="")
pacf(train_ts_arima$Catb_diff_1,lag.max = 30,main="")
adf.test(train_ts_arima$Catb_diff_1)

#Fitting an ARIMA model
model_arima <- auto.arima(train_ts_arima$Catb_clean,stepwise = FALSE,approximation = FALSE,max.p = 15,max.q = 15)

#display residuals of fitted ARIMA
tsdisplay(residuals(model_arima))

#Forecast values for out-sample (April to July 2018)
forecast_arima <- forecast(model_arima,h=4)

#Forecast values for April to July 2018 with dates 
forecast_arima_plot <- data.frame(date = seq(as.Date("2018-04-01"),by = "month",length.out = 4),Catb_clean = forecast_arima$mean[1:4])

#Fitted values on in-sample
fitted_arima <- data.frame(date = train_ts_arima$date, Catb_clean =fitted(model_arima,start = as.Date("1990-05-01")))

#plotting of results
ggplot(test_ts_arima,aes(x = date,y = Catb_clean)) + 
  geom_line() + 
  scale_x_date("month") + 
  ylab("Cat B vehicle Deregistration") + 
  xlab("") +
  geom_line(data = forecast_arima_plot,colour = "blue") 

ggplot(dereg_ts,aes(x = date,y = Catb_clean)) + 
  geom_line() + 
  scale_x_date("month") + 
  ylab("Cat B vehicle Deregistration") + 
  xlab("") +
  geom_line(data = fitted_arima,colour = "red") +
  geom_line(data = forecast_arima_plot,colour = "blue",size = 1)

#This is the part where I manually looped through different combinations of (p,d,q) to find the one with lowest AIC
#catbfinal_aic <- 1000000
#catbfinal_order <- c(0,0,0)
#for (p in 0:5) for (d in 0:2) for (q in 0:5) {
#  catbtemp_aic <- AIC(arima(dereg_ts$Catb_clean,order = c(p,d,q)))
#    if (catbtemp_aic < catbfinal_aic) {
#      catbfinal_aic <- catbtemp_aic
#      catbfinal_order <- c(p,d,q)
#      catbfinal_arima <- arima(dereg_ts$Catb_clean,order = catbfinal_order)
#    }
#  checkresiduals(catbfinal_arima)
#  autoplot(forecast(catbfinal_arima))
#}

#---------------------------------------------------------------------------------------------------
#randomforest model

#lets create our additional variables!
bidding <- fread("bidding results.csv")
gdp <- fread("gdp.csv")
pop <- fread("population.csv")
unemp <- fread("unemployment.csv")

#creating date variables
bidding$date <- seq(as.Date("2002/02/01"),by = "month",length.out = nrow(bidding))
gdp$date <- seq(as.Date("1990/01/01"),by = "quarter",length.out = nrow(gdp))
unemp$date <- seq(as.Date("1992/01/01"),by = "quarter",length.out = nrow(unemp))

#use cubic splines to extrapolate between quarterly data for GDP data and Unemployment data
func_spline <- function(x,y,ori_dataset){
  func <- splinefun(x=x,y=y,method="fmm",ties = mean)
  a <- func(seq(from = x[1],to = x[length(x)],by="month"))
  b <- data.frame(date = seq(from = x[1],to = x[length(x)],by="month"),xyz=a)
}

gdp_monthly <- func_spline(gdp$date,gdp$`Gross Domestic Product At Current Market Prices`,gdp)
names(gdp_monthly)[ncol(gdp_monthly)] <- "gdp_spline"

unemp_monthly <- func_spline(unemp$date,unemp$`Total Unemployment Rate`,unemp)
names(unemp_monthly)[ncol(unemp_monthly)] <- "unemp_spline"


#Create a new dataframe incorporating all the new predictors
dataset_oth <- Reduce(function(x,y) merge(x,y,by="date",all=TRUE),list(gdp_monthly,unemp_monthly,bidding))
dataset_temp <- merge(dataset_oth,dereg,by="date",all.x = TRUE)
dataset_rf <- dataset_temp[-c(1:4,344:347),c(1:3,5,6,10)]

#create lag variables
dataset_rf$catb_lag1 <- shift(dataset_rf$CatB_dereg,n = 1)
dataset_rf$catb_lag2 <- shift(dataset_rf$CatB_dereg,n = 2)
dataset_rf$catb_lag3 <- shift(dataset_rf$CatB_dereg,n = 3)
dataset_rf$catb_lag4 <- shift(dataset_rf$CatB_dereg,n = 4)
dataset_rf$catb_lag5 <- shift(dataset_rf$CatB_dereg,n = 5)
dataset_rf$catb_lag6 <- shift(dataset_rf$CatB_dereg,n = 6)
dataset_rf$catb_lag7 <- shift(dataset_rf$CatB_dereg,n = 7)
dataset_rf$catb_lag8 <- shift(dataset_rf$CatB_dereg,n = 8)
dataset_rf$catb_lag9 <- shift(dataset_rf$CatB_dereg,n = 9)
dataset_rf$catb_lag10 <- shift(dataset_rf$CatB_dereg,n = 10)
dataset_rf$catb_lag11 <- shift(dataset_rf$CatB_dereg,n = 11)
dataset_rf$catb_lag12 <- shift(dataset_rf$CatB_dereg,n = 12)

dataset_rf$gdp_lag1 <- shift(dataset_rf$gdp_spline,n = 1)
dataset_rf$gdp_lag2 <- shift(dataset_rf$gdp_spline,n = 2)
dataset_rf$gdp_lag3 <- shift(dataset_rf$gdp_spline,n = 3)
dataset_rf$gdp_lag4 <- shift(dataset_rf$gdp_spline,n = 4)
dataset_rf$gdp_lag5 <- shift(dataset_rf$gdp_spline,n = 5)

dataset_rf$unemp_lag1 <- shift(dataset_rf$unemp_spline,n = 1)
dataset_rf$unemp_lag2 <- shift(dataset_rf$unemp_spline,n = 2)
dataset_rf$unemp_lag3 <- shift(dataset_rf$unemp_spline,n = 3)
dataset_rf$unemp_lag4 <- shift(dataset_rf$unemp_spline,n = 4)
dataset_rf$unemp_lag5 <- shift(dataset_rf$unemp_spline,n = 5)

dataset_rf$bid_round1_lag1 <- shift(dataset_rf$bid_round1,n = 1)
dataset_rf$bid_round1_lag2 <- shift(dataset_rf$bid_round1,n = 2)
dataset_rf$bid_round1_lag3 <- shift(dataset_rf$bid_round1,n = 3)
dataset_rf$bid_round1_lag4 <- shift(dataset_rf$bid_round1,n = 4)
dataset_rf$bid_round1_lag5 <- shift(dataset_rf$bid_round1,n = 5)

dataset_rf$bid_round2_lag1 <- shift(dataset_rf$bid_round2,n = 1)
dataset_rf$bid_round2_lag2 <- shift(dataset_rf$bid_round2,n = 2)
dataset_rf$bid_round2_lag3 <- shift(dataset_rf$bid_round2,n = 3)
dataset_rf$bid_round2_lag4 <- shift(dataset_rf$bid_round2,n = 4)
dataset_rf$bid_round2_lag5 <- shift(dataset_rf$bid_round2,n = 5)

#create the test and train set 
train_rf <- dataset_rf[dataset_rf$date >= ymd("2002-02-01") & dataset_rf$date < ymd("2018-04-01"),]
test_rf <- dataset_rf[dataset_rf$date >= ymd("2018-04-01"),]

#CARET TIME
set.seed(123)

rf.fitted <- train(CatB_dereg ~.-date, 
                   data = train_rf, 
                   method="rf",
                   na.action = na.omit,
                   tuneGrid = data.frame(mtry=1:10),
                   trControl = trainControl("cv",number = 10),
                   importance = TRUE)

plot(varImp(rf.fitted,scale = FALSE))
importance(rf.fitted)

predict_train <- data.frame(date = seq(as.Date("2002-07-01"),by="month",length.out = nrow(train_rf)-5),
                                       CatB_dereg = predict(rf.fitted,train_rf))

predict_test <- data.frame(date = seq(as.Date("2018-04-01"),by="month",length.out = nrow(test_rf)),
                           CatB_dereg = predict(rf.fitted,test_rf))

ggplot(train_rf,aes(x = date,y = CatB_dereg)) + 
  geom_line() + 
  scale_x_date("month") + 
  ylab("Act vs Pred")+ xlab(" ") +
  geom_line(data=predict_train,colour="blue") + 
  geom_line(data=test_rf,colour="black") +
  geom_line(data=predict_test,colour = "red")

ggplot(test_rf,aes(x = date,y = CatB_dereg)) + 
  geom_line() + 
  scale_x_date("month") + 
  ylab("Act vs Pred")+ xlab(" ") +
  geom_line(data=predict_test,colour="blue")  
  
