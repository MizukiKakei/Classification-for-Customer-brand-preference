######################################################################################
# Brand Preference Prediction                                                        #
#                                                                                    #
# Mizuki Kakei                                                                       #
#                                                                                    #
# Version 1.0                                                                        #     
# Date : 09.05.2019                                                                  #     
# We have customers survey information with tehir age, salary, car, zipcode,         #
# education level, credit and brand.                                                 #
# Some of their brand preference information is missing.                             #     
# Therefore, we make a prediction about brand preference based on existing data.     #
#                                                                                    #
# This description uses C5.0 and Randomforest model                                  #
######################################################################################

# Uploading library ####
library(readr)                                                                       # Reading data set 
library(caret)                                                                       # Prediction
library(C50)                                                                         # C5.0 algorythm
library(inum)                                                                        # Representation of vectors and intervals
library(plyr)                                                                        # Breaking big problems down into managable pieces and bringing back together
library(ggplot2)                                                                     # Visualization

# C5 ####
# Reading Complete data ####
Cust.Complete.rawdata <- read.csv("CompleteResponses.csv", TRUE, sep =",")
Cust.Complete.C5 <- Cust.Complete.rawdata

# Preprocessing the data ####
# Changing datatypes
Cust.Complete.C5$car <- as.factor(Cust.Complete.C5$car)
Cust.Complete.C5$zipcode <- as.factor(Cust.Complete.C5$zipcode)
Cust.Complete.C5$brand <- as.factor(Cust.Complete.C5$brand)

# Excluding attributes based on variance importance 
Cust.Complete.C5$credit <- NULL
Cust.Complete.C5$elevel <- NULL

# Modeling ####
# Sampling data
set.seed(123)
Cust.Sample <- Cust.Complete.C5[sample(1:nrow(Cust.Complete.C5), 
                                             5000, 
                                             replace=FALSE
                                       ),
                               ]

# Splitting data
inTraining <- createDataPartition(Cust.Sample$brand, p = .75, list = FALSE )
training <- Cust.Sample[inTraining, ]
testing <- Cust.Sample[-inTraining, ]

# Modeling ####
# Cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
rfGridC5 <- expand.grid(trials = c(20), 
                        model = c("rules"), 
                        winnow = c(TRUE)
                       )

# training
C50model <- train(brand~., 
                  data = training, 
                  method = "C5.0", 
                  trControl = fitControl, 
                  tuneGrid = rfGridC5, 
                  tuneLength = 2)
C50model

varImp(C50model)

# predicting
predictionComplete <- predict(C50model,testing)
postResample(predictionComplete,testing$brand)

# RandomForest ####
# read data
Cust.Complete.RF <- read.csv("CompleteResponses.csv",TRUE,sep =",")

# Change datatype
Cust.Complete.RF$car <- as.factor(Cust.Complete.RF$car)
Cust.Complete.RF$zipcode <- as.factor(Cust.Complete.RF$zipcode)
Cust.Complete.RF$brand <- as.factor(Cust.Complete.RF$brand)

# Exclude attributes
Cust.Complete.RF$zipcode <- NULL
Cust.Complete.RF$car <- NULL

# sampling data
set.seed(123)
Cust.Sample.Complete <- Cust.Complete.RF[sample(1:nrow(Cust.Complete.RF), 
                                                5000, 
                                                replace = FALSE),
                                         ]

# splitting data
inTraining <- createDataPartition(Cust.Sample.Complete$brand, p = .75, list = FALSE)
training <- Cust.Sample.Complete[inTraining,]
testing <- Cust.Sample.Complete[-inTraining,]

# Modelling ####
# Cross validation and parameter tuning
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
rfGrid <- expand.grid(mtry=c(2))
rfFitm1 <- train(brand~ ., 
                 data = training, 
                 method = "rf", 
                 trControl=fitControl, 
                 tuneGrid=rfGrid
                )

# Checking the important variables
varImp(rfFitm1) 

# predicting
predictionComplete <- predict(rfFitm1,testing)
postResample(predictionComplete,testing$brand)

# Read data
Cust.Incomplete.rawdata <- read.csv("SurveyIncomplete.csv", TRUE, sep =",")
Cust.Incomplete.Prediction <- Cust.Incomplete.rawdata

# Change datatype
Cust.Incomplete.Prediction$car <- as.factor(Cust.Incomplete.Prediction$car)
Cust.Incomplete.Prediction$zipcode <- as.factor(Cust.Incomplete.Prediction$zipcode)
Cust.Incomplete.Prediction$brand <- as.factor(Cust.Incomplete.Prediction$brand)

# Excluding Attributes as C5 Model
Cust.Incomplete.Prediction$credit <- NULL
Cust.Incomplete.Prediction$elevel <- NULL

# prediction
myPrediction <- predict(C50model, Cust.Incomplete.Prediction)
myPrediction

#changing the appearance
Cust.Incomplete.Prediction$brand <- myPrediction

# Visualization ####
# Preprocessing the data
Cust.Incomplete.Prediction <- as.matrix(Cust.Incomplete.Prediction)
Cust.Complete.C5 <- as.matrix(Cust.Complete.C5)
Combineddata <- rbind(Cust.Incomplete.Prediction, Cust.Complete.C5)
Combineddata <- as.data.frame(Combineddata)

# 12. creating a graph for visualization of the predictions ####
bargraphBrand <- ggplot(Combineddata, aes(x = brand, 
                                          fill = brand, 
                                          color = brand )
                        ) + geom_bar()
bargraphBrand

# changing name of observations from"0, 1" - to"Acer, Sony" and backward
Combineddata$brand = mapvalues(Combineddata$brand, 
                               from = c("0", "1"),
                               to = c("Acer", "Sony")
                              )

# Complete visualization ####
myplot <- ggplot(Combineddata, aes(x = age, y = salary, fill = brand, color = brand)) + geom_point()
myplot + scale_fill_manual(values = c("blue","red")) 
