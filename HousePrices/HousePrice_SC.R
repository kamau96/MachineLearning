rm(list=ls())

# Load necessary libraries
library(data.table)
library(Metrics)
library(caret)
set.seed(12)

# Read data to be used in building the model
data <- data.table(fread("./SydneyHousePrices.csv"))
data$suburb <- as.factor(data$suburb)
data$propType <- as.factor(data$propType)

# Random sampling of the data
train <- data[sample(.N, 150000)]
test <- data[sample(.N, 49504)]
actual_sellPrice <- test$sellPrice
test <- subset(test, select = -c(sellPrice))

# Using significant predictors to build the model
lm_model <- lm(sellPrice ~ Date+postalCode+bath+car, data = train)
summary(lm_model)

# obtaining the predictions using the linear model
predicted_sellPrice<-predict(lm_model, newdata = test)
predicted_sellPrice <- as.numeric(predicted_sellPrice)
actual_sellPrice <- as.numeric(actual_sellPrice)
submit<-test[,.(Id, predicted_sellPrice)]

# writing out a submission
fwrite(submit,"./submit_lm.csv")
