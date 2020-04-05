library(data.table)                                                                                     # Call the library function to use it functionality
library(reshape2)
library(randomForest)
library(party)
library(ROCR)


setwd("E:\\Anand Teknow\\output")                                                                                             # Set work directory
dat <- read.csv("data.csv")   # Import data from csv to R by creating data variable 



View(dat)


temp <- dat[,-19]
x <- colnames(temp)
x <- paste(x, collapse = " + ")
equation <- as.formula(paste("WORKING.STATUS ~ ",x))
equation

library(caret)
set.seed(25)
intrain <- createDataPartition(dat$WORKING.STATUS,p=0.7,list=FALSE)

training.data <- dat[intrain,]
testing.data <- dat[-intrain,]


fit=ctree(equation,data=training.data)     
plot(fit)    

fit

predictions <- predict(fit,testing.data)

predictions

table(predictions,testing.data$WORKING.STATUS)

confusionMatrix(testing.data$WORKING.STATUS,predictions)





testing.data$predClass = predict(fit, newdata=testing.data, type="response")   
testing.data$predProb = sapply(predict(fit, newdata=testing.data,type="prob"),'[[',2)  
testing.data$predNode = predict(fit, newdata=testing.data, type="node")   

roc_pred <- prediction(testing.data$predProb, testing.data$WORKING.STATUS)
perf <- performance(roc_pred, "tpr", "fpr")
plot(perf, col="red")
abline(0,1,col="grey")

# get area under the curve
performance(roc_pred,"auc")@y.values

result <- data.frame("employee.code" = testing.data$EMP..CODE, "predictions" = predictions)

write.csv(result,"Result.csv")

