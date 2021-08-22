rm(list=ls())

# Load necessary libraries
library(data.table)
library(Metrics)
library(caret)

# Read data to be used in building the model
data <- fread("./SydneyHousePrices.csv")
train <- data[1:150000,]
test <- data[150001:199504,]
train_saleprice <- train$sellPrice
test_saleprice <- test$sellPrice


# Changing from categorial to numerical
dummies <- dummyVars(sellPrice ~ ., data = train)
train <- predict(dummies, newdata = train)

dummies2 <- dummyVars(sellPrice~., data = test)
test <- predict(dummies2, newdata = test)


train <- data.table(train)
test <- data.table(test)
train$sellPrice <- train_saleprice


lm_model <- lm(sellPrice ~ ., data = train)

# obtaining the predictions using the linear
sellPrice<-predict(lm_model, newdata = test)
submit<-test[,.(Id,SalePrice)]
submit$SalePrice[which(is.na(submit$SalePrice))]<-mean(submit$SalePrice[which(!is.na(submit$SalePrice))])

# writing out a submission
fwrite(submit,"./submit_lm.csv")
