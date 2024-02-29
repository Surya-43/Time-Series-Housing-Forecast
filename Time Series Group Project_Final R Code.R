#setting up the required libraries
library(forecast)
library(zoo)

#setting up data source
old.data <-  read.csv("C:/Users/surya/OneDrive/Desktop/BAN 673/Project/BAN 673 Project Data Source.csv")
old.data
cost <- gsub(",",'', old.data$Value)
cost

house.data <- cbind(old.data, cost)
house.data 

house.data<-transform(house.data,cost = as.numeric(house.data$cost)) 



head(house.data)
tail(house.data)
new.house.data<- house.data[,!(names(house.data) %in% c("cost"))]
new.house.data

housing.ts<-ts(house.data$cost,
               start = c(1980,1), end = c(2022, 12), freq = 12)


housing.ts


housing.stl <- stl(housing.ts, s.window = "periodic")
autoplot(housing.stl, main = "House Sales Time Series Components")

autocor <- Acf(housing.ts, lag.max = 12, 
               main = "Autocorrelation for Housing Sales")

plot(housing.ts, 
     xlab = "Timeline", ylab = "Housing Sales over time", 
     ylim = c(200, 1500), xaxt = 'n',
     main = "Housing Sales Over time")

axis(1, at = seq(1980, 2022, 1), labels = format(seq(1980, 2022, 1)))


#testing of the hypothesis
house.ar1<- Arima(housing.ts, order = c(1,0,0))
summary(house.ar1)

ar1 <- 0.9740
s.e. <- 0.0093
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

#first differencing lag
diff.house.ts <- diff(housing.ts, lag = 1)
diff.house.ts
#first differencing acf
Acf(diff.house.ts, lag.max = 12, 
    main = "Autocorrelation for Housing Sales Data")
#ACF CHART
Acf(housing.ts, lag.max = 12, 
    main = "Autocorrelation for Amtrak Ridership")

#partioning of the data
length(housing.ts)
#total records - 516
#validation period -96
nValid <- 96
nTrain <- length(housing.ts) - nValid
nTrain
#training period - 420
train.ts <- window(housing.ts, start = c(1980, 1), end = c(1980, nTrain))
train.ts
valid.ts <- window(housing.ts, start = c(1980, nTrain + 1), 
                   end = c(1980, nTrain + nValid))
valid.ts
train.ts


#calculating the window width of moving average

ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_7 <- rollmean(train.ts, k = 7, align = "right")
ma.trailing_10 <- rollmean(train.ts, k = 10, align = "right")

#forecasting of respective window width

ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_4.pred
ma.trail_7.pred <- forecast(ma.trailing_7, h = nValid, level = 0)
ma.trail_7.pred
ma.trail_10.pred <- forecast(ma.trailing_10, h = nValid, level = 0)
ma.trail_10.pred

#using accuracy function to choose the best window width
round(accuracy(ma.trail_4.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_7.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_10.pred$mean, valid.ts), 3)

#using of window width 4

# Plot original data and centered MA for window widths of k= 4 and 12. 
plot(housing.ts, 
     xlab = "Time", ylab = "Houses sold (annual rate)", xaxt = "n",
     ylim = c(250, 1500), bty = "l",
     xlim = c(1980, 2022), main = "Centered Moving Average") 
axis(1, at = seq(1980, 2022, 1), labels = format(seq(1980, 2022, 1)))
lines(ma.trailing_4, col = "green", lwd = 2)
lines(ma.trailing_7, col = "brown", lwd = 2)
lines(ma.trailing_10, col = "blue", lwd = 2)
legend(1980,1500, legend = c("values", "Centered MA, k=4","Centered MA, k=7",
                             "Centered MA, k=12"), 
       col = c("black","green","brown" , "blue"), 
       lty = c(1, 1, 1), lwd =c(4, 4, 4), bty = "n")





# Use tslm() function to create linear trend and seasonal model.
train.linear.trend.seasonality <- tslm(train.ts ~ trend + season)
train.linear.trend.seasonality
# See summary of linear trend and seasonality model and associated parameters.
summary(train.linear.trend.seasonality)
round(accuracy(train.linear.trend.seasonality$fitted, train.ts),3)
# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.linear.trend.seasonality.pred <- forecast(train.linear.trend.seasonality, h = nValid, level = 0)
train.linear.trend.seasonality.pred




## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY
train.quadratic.trend.seasonality <- tslm(train.ts ~ trend + I(trend^2) + season)
train.quadratic.trend.seasonality
summary(train.quadratic.trend.seasonality)
round(accuracy(train.quadratic.trend.seasonality$fitted, train.ts),3)
train.quadratic.trend.seasonality.pred <- forecast(train.quadratic.trend.seasonality, h = nValid, level = 0)
train.quadratic.trend.seasonality.pred
summary(train.quadratic.trend.seasonality.pred)

round(accuracy(train.linear.trend.seasonality.pred$mean, valid.ts),3)
round(accuracy(train.quadratic.trend.seasonality.pred$mean, valid.ts),3)




#entire dataset
linear.trend.seasonality <- tslm(housing.ts ~ trend + season)
linear.trend.seasonality
summary(linear.trend.seasonality)
linear.trend.seasonality.pred <- forecast(linear.trend.seasonality, h = 12, level = 0)
linear.trend.seasonality.pred


#quad
quadratic.trend.seasonality <- tslm(housing.ts ~ trend + I(trend^2) + season)
quadratic.trend.seasonality
summary(quadratic.trend.seasonality)
quadratic.trend.seasonality.pred <- forecast(quadratic.trend.seasonality, h=12, level =0)
quadratic.trend.seasonality.pred

round(accuracy(linear.trend.seasonality.pred$fitted, housing.ts), 3)
round(accuracy(quadratic.trend.seasonality.pred$fitted, housing.ts),3)



# Fit a regression model with linear trend and seasonality for training dataset. 
train.linear.trend.seasonality <- tslm(train.ts ~ trend + season)
summary(train.linear.trend.seasonality)

train.linear.trend.seasonality.pred <- forecast(train.linear.trend.seasonality, h = nValid, level = 0)
train.linear.trend.seasonality.pred

# Identify and display regression residuals for training
# partition (differences between actual and regression values 
# in the same periods).
train.linear.trend.seasonality.res<- train.linear.trend.seasonality$residuals
train.linear.trend.seasonality.res

# Apply trailing MA for residuals with window width k = 4
# for training partition.
ma.trail.res <- rollmean(train.linear.trend.seasonality.res, k = 4, align = "right")
ma.trail.res

# Regression residuals in validation period.
train.linear.trend.seasonality.res.valid <- valid.ts - train.linear.trend.seasonality.pred$mean
train.linear.trend.seasonality.res.valid

# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.

fst.2level <- train.linear.trend.seasonality.pred$mean + ma.trail.res.pred$mean
fst.2level

round(accuracy(fst.2level, valid.ts), 3)


# Fit a regression model with linear trend and seasonality for
# entire data set.
linear.trend.seasonality <- tslm(housing.ts ~ trend  + season)
summary(linear.trend.seasonality)


# Create regression forecast for future 12 periods.
linear.trend.seasonality.pred <- forecast(linear.trend.seasonality, h = 12, level = 0)
linear.trend.seasonality.pred

# Identify and display regression residuals for entire data set.
linear.seasonality.res <- linear.trend.seasonality$residuals
linear.seasonality.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(linear.seasonality.res, k = 4, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- linear.trend.seasonality.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

round(accuracy(linear.trend.seasonality.pred$fitted + tot.ma.trail.res.pred$fitted, housing.ts), 3)



# Plot original Ridership time series data and regression model.
plot(housing.ts, 
     xlab = "Time", ylab = "Houses sold", ylim = c(200, 2000), 
     bty = "l", xlim = c(1980, 2025), lwd =1, xaxt = "n",
     main = "Housing Data and Regression with Trend and Seasonality") 
axis(1, at = seq(1980, 2025, 1), labels = format(seq(1980, 2025, 1)))
lines(linear.trend.seasonality$fitted, col = "blue", lwd = 2)
lines(linear.trend.seasonality.pred$mean, col = "green", lty =5, lwd = 2)
legend(1980,1900, legend = c("Historcial Data", "Regression",
                             "Regression Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "green"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2023, 2023), c(0, 1900))
text(2010, 1900, "Data Set")
text(2023, 1900, "Future")
arrows(1980, 1800, 2023, 1800, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 1800, 2025, 1800, code = 3, length = 0.1,
       lwd = 1, angle = 30)





#quadratic
train.quadratic.seasonality.res <- train.quadratic.trend.seasonality$residuals
train.quadratic.seasonality.res

# Apply trailing MA for residuals with window width k = 4
# for training partition.
quad.ma.trail.res <- rollmean(train.quadratic.seasonality.res, k = 4, align = "right")
quad.ma.trail.res

# Create residuals forecast for validation period.
quad.ma.trail.res.pred <- forecast(quad.ma.trail.res, h = nValid, level = 0)
quad.ma.trail.res.pred

quad.fst.2level.train <- train.quadratic.trend.seasonality$fitted.values + quad.ma.trail.res


quad.fst.2level <- train.quadratic.trend.seasonality.pred$mean + quad.ma.trail.res.pred$mean
quad.fst.2level
round(accuracy(train.quadratic.trend.seasonality.pred$mean + quad.ma.trail.res.pred$mean, valid.ts), 3)



# Plot original Ridership time series data and regression model.
plot(train.quadratic.seasonality.res, 
     xlab = "Time", ylab = "Houses sold", ylim = c(-1000, 2000), 
     bty = "l", xlim = c(1980, 2025), lwd =1, xaxt = "n",
     main = "Housing Data and Regression with Trend and Seasonality") 
axis(1, at = seq(1980, 2025, 1), labels = format(seq(1980, 2025, 1)))
lines(quad.ma.trail.res, col = "blue", lwd = 2)
lines(quad.ma.trail.res.pred$mean, col = "green", lty =5, lwd = 2)
legend(1980,1900, legend = c("residuals ", "trailing ma for residuals",
                             "forecast of trailing ma residuals"), 
       col = c("black", "blue" , "green"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2023, 2023), c(-1500, 1900))
text(2010, 1900, "Data Set")
text(2024, 1900, "Future")
arrows(1980, 1800, 2023, 1800, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 1800, 2025, 1800, code = 3, length = 0.1,
       lwd = 1, angle = 30)




# Identify and display regression residuals for entire data set.
quadratic.trend.seasonality.res <- quadratic.trend.seasonality$residuals
quadratic.trend.seasonality.res

# Use trailing MA to forecast residuals for entire data set.
quad.tot.ma.trail.res <- rollmean(quadratic.trend.seasonality.res, k = 4, align = "right")
quad.tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
quad.tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
quad.tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
quad.tot.fst.2level <- quadratic.trend.seasonality.pred$mean + quad.tot.ma.trail.res.pred$mean
quad.tot.fst.2level

round(accuracy(quadratic.trend.seasonality.pred$fitted + quad.tot.ma.trail.res.pred$fitted, housing.ts), 3)



# Plot original Ridership time series data and regression model.
plot(quadratic.trend.seasonality.res, 
     xlab = "Time", ylab = "Houses sold", ylim = c(-500, 2000), 
     bty = "l", xlim = c(1980, 2025), lwd =1, xaxt = "n",
     main = "Housing Data and Regression with Trend and Seasonality") 
axis(1, at = seq(1980, 2025, 1), labels = format(seq(1980, 2025, 1)))
lines(quad.tot.ma.trail.res, col = "blue", lwd = 2)
lines(quad.tot.ma.trail.res.pred$mean, col = "green", lty =5, lwd = 2)
legend(1980,1900, legend = c("residuals ", "trailing ma for residuals",
                             "forecast of trailing ma residuals"), 
       col = c("black", "blue" , "green"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2023, 2023), c(-600, 1900))
text(2010, 1900, "Data Set")
text(2023, 1900, "Future")
arrows(1980, 1800, 2023, 1800, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 1800, 2025, 1800, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#ar1 and ar2 for linear trend
res.ar1 <- Arima(train.linear.trend.seasonality$residuals, order = c(1,0,0))
res.ar1$fitted

res.ar2 <- Arima(train.linear.trend.seasonality$residuals, order = c(2,0,0))
res.ar2$fitted

Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for AR1 for regression Residuals of Residuals")

Acf(res.ar2$residuals, lag.max = 12, 
    main = "Autocorrelation for AR2 for regression Residuals of Residuals")

two.level.train <- train.linear.trend.seasonality$fitted + res.ar2$fitted
two.level.train

#entire dataset
residual.ar2 <- Arima(linear.trend.seasonality$residuals, order = c(2,0,0))
residual.ar2
residual.ar2.pred <- forecast(residual.ar2, h = 12, level = 0)
residual.ar2.pred
Acf(residual.ar2$residuals, lag.max = 12, 
    main = "Autocorrelation for Exports Training Residuals of Residuals")


two.level.ar2 <- linear.trend.seasonality.pred$mean + residual.ar2.pred$mean
two.level.ar2

round(accuracy(linear.trend.seasonality.pred$fitted + residual.ar2.pred$fitted, housing.ts), 3)



#quadratic
quad.res.ar2 <- Arima(train.quadratic.trend.seasonality$residuals, order = c(2,0,0))
quad.res.ar2
summary(quad.res.ar2)
quad.res.ar2.pred <- forecast(quad.res.ar2, h = nValid, level = 0)
quad.res.ar2.pred


round(accuracy(train.quadratic.trend.seasonality.pred$mean + quad.res.ar2.pred$mean, valid.ts), 3)
quad.valid.two.level <- train.quadratic.trend.seasonality.pred$mean + quad.res.ar2.pred$mean
quad.valid.two.level
#entire data set
quad.residual.ar2 <- Arima(quadratic.trend.seasonality$residuals, order = c(2,0,0))
quad.residual.ar2
quad.residual.ar2.pred <- forecast(quad.residual.ar2, h=12, level = 0)
quad.residual.ar2.pred


quad.two.level.ar2 <- quadratic.trend.seasonality.pred$mean + quad.residual.ar2.pred$mean
quad.two.level.ar2

round(accuracy(quadratic.trend.seasonality.pred$fitted + quad.residual.ar2.pred$fitted, housing.ts),3)


# Plot original Ridership time series data and regression model.
plot(housing.ts, 
     xlab = "Time", ylab = "Houses sold", ylim = c(200, 2000), 
     bty = "l", xlim = c(1980, 2025), lwd =1, xaxt = "n",
     main = "Housing Data and Regression with Trend and Seasonality") 
axis(1, at = seq(1980, 2025, 1), labels = format(seq(1980, 2025, 1)))
lines(quadratic.trend.seasonality$fitted, col = "blue", lwd = 2)
lines(quadratic.trend.seasonality.pred$mean, col = "green", lty =5, lwd = 2)
legend(1980,1900, legend = c("Historical Data", "Regression",
                             "Regression Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "green"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2023, 2023), c(0, 1900))
text(2010, 1900, "Data Set")
text(2023, 1900, "Future")
arrows(1980, 1800, 2023, 1800, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 1800, 2025, 1800, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#accuracy Measures
#linear trend and Seasonality
round(accuracy(linear.trend.seasonality.pred$fitted, housing.ts), 3)
#quadratic trend and seasonality
round(accuracy(quadratic.trend.seasonality.pred$fitted, housing.ts),3)
#two level model linear trend and seasonality with trailing ma
round(accuracy(linear.trend.seasonality.pred$fitted + tot.ma.trail.res.pred$fitted, housing.ts), 3)
#two level model quadratic trend and seasonality with trailing ma
round(accuracy(quadratic.trend.seasonality.pred$fitted + quad.tot.ma.trail.res.pred$fitted, housing.ts), 3)
#two level model linear trend and seasonality with ar2
round(accuracy(linear.trend.seasonality.pred$fitted + residual.ar2.pred$fitted, housing.ts), 3)
#two level model quadratic trend and seasonality with ar2
round(accuracy(quadratic.trend.seasonality.pred$fitted + quad.residual.ar2.pred$fitted, housing.ts),3)

#holtwinter
hw.ZZZ.train <- ets(train.ts, model = "ZZZ")
hw.ZZZ.train
summary(hw.ZZZ.train)
hw.ZZZ.train.pred <- forecast(hw.ZZZ.train, h = nValid , level = 0)
hw.ZZZ.train.pred

#total data set
HW.ZZZ <- ets(housing.ts, model = "ZZZ")
HW.ZZZ
HW.ZZZ.pred <- forecast(HW.ZZZ,h=12, level = 0)
HW.ZZZ.pred

plot(housing.ts, 
     xlab = "Time", ylab = "Houses sold", ylim = c(0, 1500), 
     bty = "l", xlim = c(1980, 2025), xaxt = "n",
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     col = "black", lwd = 2) 
axis(1, at = seq(1980, 2025, 1), labels = format(seq(1980, 2025, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue",lty=1, lwd = 2)
lines(HW.ZZZ.pred$mean,col = "green", lwd = 2)
legend(1980,1500, 
       legend = c("Historical Data", 
                  "HW for Entire Data Set",
                  "HW Future 12 Periods"), 
       col = c("black", "blue" , "green"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2023, 2023), c(0, 1500))
text(2010, 1300, "Data Set")
text(2025, 1300, "Future")
arrows(1980, 1400, 2023, 1400, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 1400, 2025, 1400, code = 3, length = 0.1,
       lwd = 1, angle = 30)




#ar1 model HW MODEL
hw.res.ar1 <- Arima(hw.ZZZ.train$residuals, order = c(1,0,0))
summary(hw.res.ar1)
hw.res.ar1$fitted

hw.res.ar1.pred <- forecast(hw.res.ar1, h = nValid, level = 0)
hw.res.ar1.pred

train.df <- round(data.frame(train.ts, hw.res.ar1$fitted, 
                             hw.res.ar1$residuals, hw.res.ar1$fitted, hw.res.ar1$residuals), 3)
names(train.df) <- c("Ridership", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df


Acf(hw.res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Amtrak Training Residuals of Residuals")


hw.valid.two.level.pred <- hw.ZZZ.train.pred$mean + hw.res.ar1.pred$mean
accuracy(hw.ZZZ.train.pred$mean + hw.res.ar1.pred$mean, valid.ts)


hw.total.res.1 <- Arima(HW.ZZZ$residuals, order = c(1,0,0))
hw.total.res.1.pred <- forecast(hw.total.res.1, h=12, level = 0)
hw.total.res.1.pred

hw.ar1.pred <- HW.ZZZ.pred$mean + hw.total.res.1.pred$mean
hw.ar1.pred
round(accuracy(HW.ZZZ.pred$fitted + hw.total.res.1.pred$fitted, housing.ts), 3)

#moving averages hw
# Identify and display regression residuals for training
# partition (differences between actual and regression values 
# in the same periods).
hw.ZZZ.train.res<- hw.ZZZ.train$residuals
hw.ZZZ.train.res

# Apply trailing MA for residuals with window width k = 7
# for training partition.
hw.ma.trail.res <- rollmean(hw.ZZZ.train.res, k = 7, align = "right")
hw.ma.trail.res

# Regression residuals in validation period.
hw.ma.trail.res.valid <- valid.ts - hw.ZZZ.train.pred$mean
hw.ma.trail.res.valid

# Create residuals forecast for validation period.
hw.ma.trail.res.pred <- forecast(hw.ma.trail.res, h = nValid, level = 0)
hw.ma.trail.res.pred

# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level.train <- hw.ZZZ.train$fitted + hw.ma.trail.res
fst.2level.train

fst.2level <- hw.ZZZ.train.pred$mean + hw.ma.trail.res.pred$mean
fst.2level

round(accuracy(hw.ZZZ.train.pred$mean + hw.ma.trail.res.pred$mean, valid.ts), 3)

#entire data
total.hw.ZZZ.train.res<- HW.ZZZ$residuals
total.hw.ZZZ.train.res

# Apply trailing MA for residuals with window width k = 7
# for training partition.
total.hw.ma.trail.res <- rollmean(total.hw.ZZZ.train.res, k = 7, align = "right")
total.hw.ma.trail.res

# Regression residuals in validation period.
total.hw.ma.trail.res.valid <- valid.ts - HW.ZZZ.pred$fitted
total.hw.ma.trail.res.valid

# Create residuals forecast for validation period.
total.hw.ma.trail.res.pred <- forecast(total.hw.ma.trail.res, h = 12, level = 0)
total.hw.ma.trail.res.pred

# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
toatl.hwww.fst.2level <- HW.ZZZ.pred$mean + total.hw.ma.trail.res.pred$mean
toatl.hwww.fst.2level

fst.2level <- hw.ZZZ.train.pred$mean + hw.ma.trail.res.pred$mean
fst.2level

round(accuracy(HW.ZZZ.pred$fitted + total.hw.ma.trail.res.pred$fitted, housing.ts), 3)

plot(housing.ts, 
     xlab = "Time", ylab = "Houses sold", ylim = c(0, 1500), 
     bty = "l", xlim = c(1980, 2025), xaxt = "n",
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     col = "black", lwd = 2) 
axis(1, at = seq(1980, 2025, 1), labels = format(seq(1980, 2025, 1)))
lines(toatl.hwww.fst.2level, col = "green", lwd = 2)
legend(1980,1500, 
       legend = c("Historical Data", 
                  "HW 2-level forcast for Future 12 Periods"), 
       col = c("black" , "green"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2023, 2023), c(0, 1500))
text(2010, 1300, "Data Set")
text(2025, 1300, "Future")
arrows(1980, 1400, 2023, 1400, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 1400, 2025, 1400, code = 3, length = 0.1,
       lwd = 1, angle = 30)



#hypothesis
house.ar1<- Arima(housing.ts, order = c(1,0,0))
summary(house.ar1)

ar1 <- 0.9740
s.e. <- 0.0093
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

diff.house.ts <- diff(housing.ts, lag = 1)
diff.house.ts

Acf(diff.house.ts, lag.max = 12, 
    main = "Autocorrelation for Housing Sales Data")
Acf(housing.ts, lag.max = 12, 
    main = "Autocorrelation for Amtrak Ridership")

#ARIMA
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred
#plot
accuracy(train.auto.arima.pred$mean, valid.ts)

auto.arima <- auto.arima(housing.ts)
summary(auto.arima)

auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred
round(accuracy(auto.arima.pred$fitted, housing.ts), 3)


# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 12 future periods.
plot(housing.ts, 
     xlab = "Time", ylab = "Houses sold (annual rate)", 
     ylim = c(250, 2000), xaxt = "n", 
     bty = "l", xlim = c(1980, 2025), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset") 
axis(1, at = seq(1980, 2025, 1), labels = format(seq(1980, 2025, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "green", lty = 5, lwd = 2)
legend(1980,1700, legend = c("Houses sold Time Series", 
                             "Auto ARIMA Forecast", 
                             "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "green"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
lines(c(2023, 2023), c(0, 1500))
text(2010, 1300, "Data Set")
text(2025, 1300, "Future")
arrows(1980, 1400, 2023, 1400, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 1400, 2025, 1400, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# two level forecast for linear trend and seasonality with ma
round(accuracy(linear.trend.seasonality.pred$fitted + tot.ma.trail.res.pred$fitted, housing.ts), 3)

#linear trend and seasonality 2 level model with ar1
round(accuracy(linear.trend.seasonality.pred$fitted + residual.ar2.pred$fitted, housing.ts), 3)

#quadratics trend 2 level model with trailing ma
round(accuracy(quadratic.trend.seasonality.pred$fitted + quad.tot.ma.trail.res.pred$fitted, housing.ts), 3)
#quadratic trend 2 level model with ar2
round(accuracy(quadratic.trend.seasonality.pred$fitted + quad.residual.ar2.pred$fitted, housing.ts),3)

#2-level holt's winter and ar model
round(accuracy(HW.ZZZ.pred$fitted + hw.total.res.1.pred$fitted, housing.ts), 3)

#2-level holt's winter and trailing ma
round(accuracy(HW.ZZZ.pred$fitted + total.hw.ma.trail.res.pred$fitted, housing.ts), 3)
#auto arima
round(accuracy(auto.arima.pred$fitted, housing.ts), 3)

