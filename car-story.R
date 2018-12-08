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
ggplot(dereg_ts,aes(x = date_format,y = CatB_dereg)) + geom_line() + scale_x_date("month")
       + ylab("Cat B vehicle Deregistration")+ xlab()

#---------------------------------------------------------------------------------------------------
#this is the log series
#create a log series of the Cat B deregistrations to reduce the error 
dereg_ts$log_catb <- log(dereg_ts$CatB_dereg)
dereg_ts$log_catb <- tsclean(dereg_ts$log_catb)
ggplot(dereg_ts,aes(x = date_format,y = log_catb)) + geom_line() + scale_x_date("month")
                    + ylab("Cat B vehicle Deregistration")+ xlab()

#split train and test set (wanna test for apr - jul)
train_ts_arima <- dereg_ts[date_format < ymd("2018-04-01"),]
test_ts_arima <- dereg_ts[date_format >= ymd("2018-04-01"),]

#---------------------------------------------------------------------------------------------------
#train an arima model on the train data set
adf.test(train_ts_arima$log_catb)
acf(train_ts_arima$log_catb,main = "ACF plot")
pacf(train_ts_arima$log_catb,main="PACF plot")

log_catb_diff_train <- diff(train_ts_arima$log_catb,differences = 1)
plot.ts(log_catb_diff_train)
adf.test(log_catb_diff_train)

acf(log_catb_diff_train)
pacf(log_catb_diff_train)

model1 <- auto.arima(train_ts_arima$log_catb,seasonal = FALSE, approximation = FALSE)
checkresiduals(model1)
autoplot(forecast(model1))

model1_forecast <- forecast(model1,h=4)
plot(model1_forecast,main="")
lines(ts(dereg_ts$log_catb))

rmse(exp(model1_forecast$mean[1:4]),exp(test_ts_arima$log_catb))

plot(exp(test_ts_arima$log_catb),
     type = "b",col="red",
     legend("topleft",c("actual","predicted"),fill=c("red","blue")))
lines(exp(model1_forecast$mean[1:4]),type="b",col="blue")

#temporary code to iterate through possibilities of arima models to find the one with minimum AIC
#catbfinal_aic <- 1000000
#catbfinal_order <- c(0,0,0)
#for (p in 0:5) for (d in 0:2) for (q in 0:5) {
#  catbtemp_aic <- AIC(arima(dereg_ts$log_catb,order = c(p,d,q)))
#    if (catbtemp_aic < catbfinal_aic) {
#      catbfinal_aic <- catbtemp_aic
#      catbfinal_order <- c(p,d,q)
#      catbfinal_arima <- arima(dereg_ts$log_catb,order = catbfinal_order)
#    }
#  checkresiduals(catbfinal_arima)
#  autoplot(forecast(catbfinal_arima))
#}

#---------------------------------------------------------------------------------------------------
#this will be the randomforest model

#lets create our additional variables!

bidding <- fread("bidding results.csv")
gdp <- fread("gdp.csv")
pop <- fread("population.csv")
unemp <- fread("unemployment.csv")

bidding$date <- seq(as.Date("2002/02/01"),by = "month",length.out = nrow(bidding))
gdp$date <- seq(as.Date("1990/01/01"),by = "quarter",length.out = nrow(gdp))
unemp$date <- seq(as.Date("1992/01/01"),by = "quarter",length.out = nrow(unemp))

#date_seq <- seq(from = as.Date("1990/01/01"),to = as.Date("2018/07/01"),by = "month")

#use cubic splines to extrapolate between quarterly data

func_spline <- function(x,y,ori_dataset){
  func <- splinefun(x=x,y=y,method="fmm",ties = mean)
  a <- func(seq(from = x[1],to = x[length(x)],by="month"))
  b <- data.frame(date = seq(from = x[1],to = x[length(x)],by="month"),xyz=a)
}

gdp_monthly <- func_spline(gdp$date,gdp$`Gross Domestic Product At Current Market Prices`,gdp)
names(gdp_monthly)[ncol(gdp_monthly)] <- "gdp_spline"

unemp_monthly <- func_spline(unemp$date,unemp$`Total Unemployment Rate`,unemp)
names(unemp_monthly)[ncol(unemp_monthly)] <- "unemp_spline"


#now i want to create a new dataframe incorporating all these
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
  
