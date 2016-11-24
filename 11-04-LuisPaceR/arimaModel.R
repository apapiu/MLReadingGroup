library("forecast"); library("caret")

setwd("/Users/belcelou/Dropbox/Pace Predictor")
data <- read.csv(file = "dataFFF.csv")
data$X <- NULL
times <- data$moving_time
speed <- data$average_speed

# trash variables i decided to keep from raw data
# but did not end up using
data$moving_time <- NULL
data$elapsed_time <- NULL
data$months <-NULL
data$days <- NULL

trainData <- data[1:90,]
testData <- data[91:100,]

# function that performs leave one out cross valiadation
# for time series data. INPUTS:
# order: 3-vector specifying ARIMA parameters
# drift.Bool: T/F for drift term in moden
# trainingD/testD: training/test datasets
LOOCV <- function(order, drift.Bool, trainingD, testD){
  # setting the time series variable
  trainY <- trainingD$average_speed;
  testY <- testD$average_speed
  
  # the outer regressors
  trainReg <- trainingD[,2:5]
  testReg <- testD[,2:5]
  
  # number of iterations we need and a vector to store the errors
  M <- length(testY)
  errors <- c(1:M)
  
  for (i in c(1:M)){
    # our initial model. at every iteration the training 
    # data matrix will be augmented
    m <- Arima(trainY, order = order, xreg = trainReg,
               include.drift = drift.Bool)
    
    # predicts the next value of the test set
    f <- forecast(m, h = 1 ,xreg=testReg[1,])
    prediction <- f$mean
    
    # record the error made
    errors[i] = RMSE(prediction, testY[1])^2
    
    # remove the test data we predicted and add it to
    # the training set
    trainY <- append(trainY, testY[1])
    testY <- testY[-1]
    
    trainReg <- rbind(trainReg, testReg[1,])
    testReg <- testReg[-1,]
  }
  
  return(mean(errors))
}

# try a bunch of models and pick the one with the lowest
# leave one out cross validation error
model_01_CV <- LOOCV(c(2,1,2), drift = F, trainingD = trainData,
                     testD = testData)
model_02_CV <- LOOCV(c(2,1,2), drift = T, trainingD = trainData,
                     testD = testData)
model_03_CV <- LOOCV(c(2,1,1), drift = F, trainingD = trainData,
                     testD = testData)
model_04_CV <- LOOCV(c(2,1,1), drift = T, trainingD = trainData,
                     testD = testData)
model_05_CV <- LOOCV(c(2,0,1), drift = F, trainingD = trainData,
                     testD = testData)
model_06_CV <- LOOCV(c(2,0,1), drift = T, trainingD = trainData,
                     testD = testData)
model_07_CV <- LOOCV(c(1,0,1), drift = F, trainingD = trainData,
                     testD = testData)
model_08_CV <- LOOCV(c(1,0,1), drift = T, trainingD = trainData,
                     testD = testData)
# from the models above, moden_08 has the lowest LOOCV
# let's see what the test predictions look like
tFit <- Arima(trainData[,1], order = c(1,0,1),
                 xreg = trainData[,2:5], include.drift = T)
tForecast <- forecast(tFit, h = 10, xreg = testData[,2:5])
plot(tForecast)
tPredictions <- tForecast$mean
sum(residuals(tForecast)^2)/90 # training error
RMSE(tPredictions, testData[,1])^2 # test error


# we now fit the model using the entire dataset
LOOCV_fit <- Arima(speed, order = c(1,0,1),
                   xreg = data[,2:5],
                   include.drift = T)
summary(LOOCV_fit)





# there is also the 'auto.arima' function which picks picks
# the best model by minimizing the Akaike Information Critera (AIC)
# see documentation for more details. 
autoModel <- auto.arima(speed, xreg = data[,2:5]);
summary(autoModel)

# what is the LOOCV of the auto fit??
autoLOOCV <- LOOCV(c(1,1,3), drift = T, trainingD = trainData,
                   testD = testData)
# LOOCV_fit has smaller cv error, but this one is the next best
                

# on sunday i will run 21km. the forecast for sunday morning
# is 71deg and 55% hum. assume 100ft elev gain
# let's see how these two models predict
x <- data.frame(distance=21,
          total_elevation_gain=100,
          temp=71, hum=55)

p1 <- forecast(LOOCV_fit, h = 1 ,xreg=x)
summary(p1); plot(p1)
# (plot is kind of pointless since we only have 1 point)

p2 <- forecast(autoModel, h = 1 ,xreg=x)
summary(p2); plot(p2)

# divide 60 by the speed to get the pace (units km/hr)
prediction01 <- 60/(p1$mean); prediction02 <- 60/(p2$mean);

# OLS predictor gives speed estimate 11.6km/hr, which translates
# to a pace of 5.17min/km. The Riegel estimate time is 
# 1hr and 40mins, which means a speed of 13.125km/hr
# and a pace of 4.57min/km. 

# final predictions
paces <- c(4.57, 5.17, prediction01, prediction02)
paces

# UPDATE: actual pace 4:27min/km (4.45min/km)
# Activity link: https://www.strava.com/activities/767782242
# Actual elevation gain was 113ft. Actual weather was 45deg
# and 86% hum. Updating prediction gives
x <- data.frame(21.5, 113, 45, 86)

p1 <- forecast(LOOCV_fit, h = 1 ,xreg=x)
p2 <- forecast(autoModel, h = 1 ,xreg=x)

prediction01 <- 60/(p1$mean); prediction02 <- 60/(p2$mean);
paces <- c(4.94, 5.17, prediction01, prediction02)
paces

# Moral: Use auto.arima. LOOCV is no good, haha
# I plan to expand on the results further (maybe during
# winter break)

