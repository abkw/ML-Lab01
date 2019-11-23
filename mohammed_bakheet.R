library(readxl)
library(kknn)
# importing xlsx files
data <- read_excel("D:/Desktop/Machine Learning/Machine Learning/lab01/spambase.xlsx")
names(data)

#Deviding data into dataset and training
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

# 2- Logistics Regression 

logiModel <- glm(Spam ~ . , family = binomial, data = train)
summary(logiModel)

# Prediction for testing data
glm.probs <- predict(logiModel, newData = test, type = 'response')

#making prediction
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

#Calculating the factor
# y_pred <- factor(glm.pred, levels=c(0, 1))
# y_act <- test$Spam

#The confusion matrices

table(glm.pred,train$Spam)
table(glm.pred,test$Spam)
#When using the confusion matrix it appears that 473 emails were classified as spam, whereas 1254 emails were not spam
#computing missclassification
testMisClassification <- (1 - mean(glm.pred == test$Spam)) * 100
trainMisClassification <- (1 - mean(glm.pred == train$Spam)) * 100
# 3- Classifying test data more than 0.8
glmPredtest <- ifelse(glm.probs > 0.8, 1, 0)

#Confustion matrix
table(glmPredtest,train$Spam)
table(glmPredtest,test$Spam)
#computing missclassification

testMisClassification08 <- (1 - mean(glmPredtest == test$Spam)) * 100
trainMisClassification08 <- (1 - mean(glmPredtest == train$Spam)) *100
#The mean, or the accuracy of the model in when P(Y = 1|X) > 0.5, otherwirse Y^ = 0, is 0.8394161. 
#Whereas the model accuracy when P(Y = 1|X) > 0.8, otherwirse Y^ = 0, is 0.749635
#When P(Y = 1|X) > 0.5, 346 emails were correctly classified as spam, and 127 emails were misclassified
#When P(Y = 1|X) > 0.8, 106 emails were correctly classified as spam, and 10 emails were misclassified

#4: standard classifier kknn() with k 30
kknnResult <- kknn(Spam ~ ., train = train, test = test, k=30, distance = 2, kernel = "optimal")
kknnPredict <- ifelse(kknnResult$fitted.values > 0.5, 1, 0)
kknnTrain <- table(kknnPredict, train$Spam)
kknnTest <- table(kknnPredict, test$Spam)
#Calculating misclassification when k = 30
#Misclassification for test data
miscTrain <- (1 - mean(kknnPredict == train$Spam)) * 100
miscTest <- (1 - mean(kknnPredict == test$Spam)) * 100

#When K = 1
kknnResult1 <- kknn(Spam ~ ., train = train, test = test, k=1, distance = 2, kernel = "optimal")
kknnPredict1 <- ifelse(kknnResult1$fitted.values > 0.5, 1, 0)
kknnTrain1 <- table(kknnPredict1, train$Spam)
kknnTest1 <- table(kknnPredict1, test$Spam)
#Calculating misclassification when k = 30
#Misclassification for test data
miscTrain1 <- (1 - mean(kknnPredict1 == train$Spam)) * 100
miscTest1 <- (1 - mean(kknnPredict1 == test$Spam)) * 100
#When using k = 1 the misclassification rate increases


