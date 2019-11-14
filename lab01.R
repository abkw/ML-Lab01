  library("readxl")
  library(kknn)
  # importing xlsx files
  data <- read_excel("/home/mohammed/Desktop/Machine Learning/Machine Learning/lab01/spambase.xlsx")
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
  
  