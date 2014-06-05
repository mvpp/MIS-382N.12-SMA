#Strategy 1 - ARIMA
setwd("/Users/yunlugao/Documents/Social Media Analytics/Case/Bitcoin Strategy")
{
library(forecast)
price <- read.csv("price.csv")

d <- 7  		# days
n <- 24 * 60	# 1440 minutes per day
nrow <- nrow(price)		# d * n = 10080

ts <- ts(price[,2], start = c(1, 1), end = c(d, n), frequency = n)

pdf("arima.pdf", width = 8, height = 5)
plot(ts, main = "Time Series", ylab = "Price", xlim = c(1,9), ylim = c(400,500))
abline(v = c(2:7), lty = 2, col = 'blue')

arima <- auto.arima(ts)		# ARIMA(2,1,2)
#Forecast the price
forecast <- forecast(arima)

plot(forecast, xlab = "Time", ylab = "Price", xlim = c(1,9), ylim = c(400, 500))
abline(v = c(2:7), lty = 2, col = 'blue')
dev.off()
# It needs sometime to save this pdf

write.csv(forecast, "arima.csv")

residuals <- arima$residuals
mse <- sum(residuals^2)
rmse <- sqrt(mse)
#RMSE
cat(paste0("RMSE: ", round(rmse, 2), "\n"))

#residual = actual price - predicted price
#predicted = actual - residual
#prediction of price from April 1 to 7 
price.arima <- price[,2] - residuals
write.csv(price.arima, "price.arima.csv")
}

#Strategy 2 - Random Forest
#weight the sentiment by log(following + 1), log(followers + 1), and log(updates + 1)
#Slide 7  
{
  library(dplyr)
  raw<-read.csv("1_raw tweets with sentiments.csv")
  data<-raw[,c(5,13:15,17:18)]
  names(data)<-c("PUBLISH_DATE","FOLLOWING","FOLLOWERS","UPDATES","POSITIVE","NEGATIVE")
  data$PUBLISH_DATE<-format(data$PUBLISH_DATE)
  data$PUBLISH_DATE<-strptime(data$PUBLISH_DATE,  "%m/%d/%Y %H:%M",tz="GMT")
  
#log FOLLOWING, FOLLOWER, and UPDATES
#+1 is to avoid log(0) = -inf
  data$log_FOLLOWING<-log(data$FOLLOWING + 1)
  data$log_FOLLOWERS<-log(data$FOLLOWERS + 1)
  data$log_UPDATES<-log(data$UPDATES + 1)
  data$FOLLOWING<-NULL
  data$FOLLOWERS<-NULL
  data$UPDATES<-NULL
  
#aggregate by minute
  data$time<-as.numeric(data$PUBLISH_DATE)
  data$PUBLISH_DATE<-NULL
  
  n<-summarise(group_by(data,time), count=n())
  middata<-merge(data,n,by.x="time",by.y="time")
  
#calculate the score for the sentiment (positive and negative) weighted by log(folliwing),
#log(follower), and log(updates)
#Aggregate from second to minute
  result<-summarise(group_by(middata,time),
                    folllowing_pos_score=sum(POSITIVE * log_FOLLOWING) / mean(count),
                    folllowing_neg_score=sum(NEGATIVE * log_FOLLOWING) / mean(count),
                    folllowers_pos_score=sum(POSITIVE * log_FOLLOWERS) /mean(count),
                    folllowers_neg_score=sum(NEGATIVE * log_FOLLOWERS) /mean(count),
                    updates_pos_score=sum(POSITIVE * log_UPDATES) / mean(count),
                    updates_neg_score=sum(NEGATIVE * log_UPDATES) / mean(count),
                    AVG_POSITIVE=mean(POSITIVE),
                    AVG_NEGATIVE=mean(NEGATIVE),
                    count=mean(count))

  result$PUBLISH_DATE<-as.POSIXct(result$time,"GMT","1970-01-01")
  result$time<-NULL  
}

#merge price and result (result is the file telling about the sentiment scores)
{
rice$Time<-format(price$Time)
price$Time<-strptime(price$Time,"%m/%d/%y %H:%M",tz="GMT")

result$time<-as.numeric(result$PUBLISH_DATE)
price$time<-as.numeric(price$Time)

all<-merge(result,price,x.name="time", y.name="time")
all$time<-NULL
all$PUBLISH_DATE<-NULL
}

# model - Random Forest
{
  library(randomForest)
  all$LagPrice[2:10075]<-all$Price[1:10074]
  all$change<-all$Price-all$LagPrice
  all$updownNext<-as.numeric(all$change>0)
  all<-all[-1,]

  #Random Forest
  #training and test dataset
  set.seed(731)
  n <- dim(all)[1]
  random.idx <- sample(1:n, n)
  # the first 7000 records belong to the training set and the left belong to the test set
  train.idx <- random.idx[1:7000]
  test.idx <- random.idx[7001:n]

  data.train <- all[1:7000, ]
  data.test <- all[7001:n,]

  #random forest on train
  rf.train <- randomForest(updownNext ~ folllowing_pos_score + folllowing_neg_score + 
                             folllowers_pos_score + folllowers_neg_score + updates_pos_score
                           + updates_neg_score + AVG_POSITIVE + AVG_NEGATIVE+ count,
                           data=data.train,importance=T, ntree=6)
  pre.train <- predict(rf.train,data.train)

  pre.test.prob <- predict(rf.train,data.test)

  #get the probability that make a profitable sell
  sell <- ifelse(pre.test.prob-(1-pre.test.prob)>0.1,0,NA)
  sum(data.test$updownNext ==0 & sell==0, na.rm=T)/sum(sell==0,na.rm=T)
  #get the probability that make a profitable buy
  buy <- ifelse(pre.test.prob-(1-pre.test.prob)<0.2,0,NA)
  sum(data.test$updownNext ==0 & buy==0, na.rm=T)/sum(buy==0,na.rm=T)
}

#Strategy 3 - A Combination of Social Predictors with Autocorrelation
{
 #detrend
  price <- read.csv("price.csv")
  counts <- as.vector(c(1:10080))
  trend <- lm(price$Price ~ counts)
  
  detrended <- data.frame(x=trend$residuals)
  a<-as.POSIXct(price[,1])
  library(lubridate)
  agghour <- floor_date(a,"hour")
  detrended <- cbind(detrended,agghour)

  library(dplyr)
  HourPrice<-summarise(group_by(detrended,agghour),
                 hrprice = mean(x))
  #deseasonality (24 hours per day)
  c<- rep(c(1:24),times = nrow(HourPrice)/24)
  HourPrice <- cbind(HourPrice,c)
  season<-summarise(group_by(HourPrice,c),
                    seasonprice = mean(hrprice))
  
  d<-c()
  for (i in 1:24){
    e<-data.frame(rep(season[i,2],times = 60))
    d <- rbind(d,e)
  }

  #seasonal effect
  d<- as.vector(as.matrix(d))
  f<- data.frame(rep(d,7))

  #get the price which is detrended and deseansoned
  detrended <- cbind(detrended,f)
  detrended$dtdsprice <- detrended$x - detrended[,3]
  names(detrended)[3] <- "seasonality"
  
  #Use a linear regression to predict the detrended price
  #merge the detrended price with the twitter information
  dtds<-data.frame(detrended, price$Time)
  dtds<-dtds[,c(-2)]
  names(dtds)[4]<-"Time"
  dtds$Time<-format(dtds$Time)
  dtds$Time<-strptime(dtds$Time,"%m/%d/%y %H:%M",tz="GMT")
  
  #merge by time
  all$time<-as.numeric(all$Time)
  dtds$time<-as.numeric(dtds$Time)
  All<-merge(dtds,all,x.name="time", y.name="time")
  All$time<-NULL
  
  All$lag_dtdsprice[2:10074]<-All$dtdsprice[1:10073]
  All<-All[-1,]
  model.lm<-lm(dtdsprice~folllowing_pos_score+folllowing_neg_score
               +folllowers_pos_score+folllowers_neg_score+
                 updates_pos_score+updates_neg_score+
                 AVG_POSITIVE+AVG_NEGATIVE+

  All$fitted_dtdsprice<-predict(model.lm,All)
  
  #final predicted price
  count <- as.vector(c(1:10073))
  trend <- lm(All$Price ~ count)
  finalPrice<-trend$fitted.values + All$fitted_dtdsprice + All$seasonality
  
  ######Back to price######
  predicted.move <- ifelse(finalPrice>All$LagPrice, 1, 0)
  #whether buy or sell is based on whether the predicted price exceeds the current price
  #get the probability that make a profitable sell
  buy <- ifelse((finalPrice - All$LagPrice) > 0.3,1,0)
  sum(buy==1 & as.numeric(All$updownNext)==1) / sum(buy==1)
  #get the probability that make a profitable buy
  sell <- ifelse((finalPrice - All$LagPrice) < -0.1,1,0)
  sum(sell==1 & as.numeric(All$updownNext==0)) / sum(sell==1)
}
