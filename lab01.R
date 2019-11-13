  library("readxl")
  # importing xlsx files
  data <- read_excel("/home/mohammed/Desktop/Machine Learning/Machine Learning/lab01/spambase.xlsx")
  names(data)
  
  #Deviding data into dataset and training
  n=dim(data)[1]
  set.seed(12345)
  id=sample(1:n, floor(n*0.5))
  train=data[id,]
  test=data[-id,]
  # Logistics Regression 
  
  logiModel <- glm(Spam ~ . , family = "binomial", data = train)
  summary(logiModel)
  
  # Prediction for testing data
  glm.probs <- predict(logiModel, newData = test, type = "response")
  glm.probs
  #making prediction
  glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
  
  #Calculating the factor
  y_pred <- factor(glm.pred, levels=c(0, 1))
  y_act <- test$Spam
  
  #The confusion matrices
  table(glm.pred,Spam)
  
  #computing missclassification
  mean(y_pred == y_act)
