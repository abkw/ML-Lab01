library(ggplot2)
library(readxl)

tecator = read_excel("D:/Desktop/Machine Learning/Machine Learning/lab01/tecator.xlsx")

plotMoistureVsProtein = function() {
  regressor = lm(formula = Protein ~ Moisture, data = tecator)
  y_predict = predict(regressor, newdata = tecator)
  
  
  ggplot(tecator, aes(x = tecator$Moisture, y = tecator$Protein),) +
    geom_point(color = 'red') +
    #geom_line(aes(x = tecator$Moisture, y = predict(regressor, newdata = tecator)),
    #          color = 'yellow') +
    geom_smooth(method = lm,
                color = 'blue') +
    ggtitle('Protein vs Moisture') +
    xlab('Protein') +
    ylab('Moisture')
}


# -----------------------------------------------

plotPloynomialModels = function() {
  n = dim(tecator)[1]
  set.seed(12345)
  id = sample(1:n, floor(n * 0.5))
  trainingSet = tecator[id,]
  validationSet = tecator[-id,]
  
  poly_num = c()
  train_MSE = c()
  valid_MSE = c()
  
  for (index in 1:6) {
    MSE_data = getMSE_data(trainingSet, validationSet,index)
    
    poly_num = c(poly_num, index)
    train_MSE = c(train_MSE, MSE_data[['train']])
    valid_MSE = c(valid_MSE, MSE_data[['valid']])
  }
  
  MSE_dataframe = data.frame(
    polyCount = poly_num,
    train_MSE = train_MSE,
    valid_MSE = valid_MSE
  )
  #print(MSE_dataframe)
  
  ggplot(MSE_dataframe) +
    geom_line(aes(x = MSE_dataframe$polyCount, y = MSE_dataframe$train_MSE, color = 'Training Data')) +
    geom_point(aes(x = MSE_dataframe$polyCount, y = MSE_dataframe$train_MSE, color = 'Training Data')) +
    geom_line(aes(x = MSE_dataframe$polyCount, y = MSE_dataframe$valid_MSE, color = 'Validation Data')) +
    geom_point(aes(x = MSE_dataframe$polyCount, y = MSE_dataframe$valid_MSE, color = 'Validation Data')) +
    scale_color_discrete(name = 'Datset') +
    ggtitle('Mean vs Polynomial Degree') +
    xlab('Polynomial Degree') +
    ylab('MSE')
}



getMSE_data = function(trainSet, validationSet, degree) {
  regressor = lm(formula = Moisture ~ poly(Protein, degree = degree, raw = TRUE), data = trainSet)
  SSE = as.numeric(t(residuals(regressor)) %*% residuals(regressor))
  print(SSE)
  MSE = SSE / nrow(trainSet)
  
  validFittedValue = predict(regressor, validationSet)
  validationResidulas = validationSet$Moisture - validFittedValue
  validationSSE = as.numeric(t(validationResidulas) %*% validationResidulas)
  validationMSE = validationSSE / nrow(validationSet)
  
  MSE_data = list()
  MSE_data[['train']] = MSE
  MSE_data[['valid']] = validationMSE
  
  return(MSE_data)
}


plotMoistureVsProtein()
plotPloynomialModels()