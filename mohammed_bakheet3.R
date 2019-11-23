library(MASS)
library(glmnet)
#1) importing xlsx files for the data and plotting the moisture versus protein
data <- read_excel("D:/Desktop/Machine Learning/Machine Learning/lab01/tecator.xlsx")
#Plotting Moisture versus Protein
plot(data$Moisture,data$Protein)

#From the plot, the data could be described very well in a linear model, and it appears
#that moisture and protein have a positive relation

#2) 𝑀i is our model ~ N(M,Sigma^2)
#Mi = B0 + B1X + B2X^2 + .... + BiX^i
#it is appropriate to use MSE criterion when fitting this model because the moisture is
#normally distributed and and the expected Moisture is a polynomial function of Protein.

# lmModel <- lm(formula = Protein ~ Moisture, data = tecator)
# summary(lmModel)
# aicValue <- AIC(object = lmModel,k = 6)
#Get AICs function
getAic <- function(train, test){
  aicVector <- vector()
  trainingMse <- vector()
  testingMse <- vector()
  for (i in 1:6){
  linearModel <- lm(formula = Moisture ~ poly(Protein, degree = i, raw = TRUE), data = train)
  aicVector <- c(aicVector,AIC(linearModel))
  #Calculating the training MSE
  trainingMse <- c(trainingMse, mean(sum(linearModel$residuals^2)))
  #Calculating the testing MSE
  testingMse <- c(testingMse, mean((test$Moisture - predict.lm(linearModel, test)) ^ 2))
  }
  result <- data.frame(trainingMse,testingMse,c(1:6))
  return(result)
}
#3)Dividing the data into training and testing, and fitting the models
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

#Sending the data to get AICs for different models

mses <- getAic(train,test)
ggplot(mses)+
  geom_line(aes(x = mses$c.1.6., y = mses$trainingMse, color = 'Training MSE')) +
  geom_point(aes(x = mses$c.1.6., y = mses$trainingMse, color = 'Training MSE')) +
  geom_line(aes(x = mses$c.1.6., y = mses$testingMse, color = 'Testing MSE')) +
  geom_point(aes(x = mses$c.1.6., y = mses$testingMse, color = 'Testing MSE')) +
  scale_color_discrete(name = 'Datset') +
  ggtitle('Training MSE & Testing MSE') +
  xlab('Degree (Model Complexity') +
  ylab('MSE')

#The best model is when degree = 3 with AIC of 658.3725, and MSE for testing of 39.88347
# The more we increase the degree (power) of the model the bigget the error gets for  the
#testing data, however, for the training data the mean square erros decreases as the model
#degree increases. The model for training has a high bias and low variance when the model
#was simple, and when the model gets highly complicated, the error increases also. The best
#model therefore, is when the model is its medium complexity with 3 degrees.

#4) Variable selection of a linear model
# modelData <- data[,Channel1:Channel100]
# modelData %>% select(Channel1:Channel100)
modelData <- data[,-c(ncol(data),ncol(data)-1)]
linearModelAic <- lm(formula = Fat ~ . , data = modelData)
aicValue <- stepAIC(linearModelAic,direction = "backward")
#63 variable are selected as a final model by stepAIC of the total number of channels,
#meaning AIC is smallest when using these 63 variables and excluding the rest.


#5) Fitting ridge regression
y <- modelData$Fat
x <- model.matrix(Fat~., modelData)[,-1]
lambdas <- 10^seq(3, -2, by = -.1)

fit <- glmnet(x, y, alpha = 0, lambda = lambdas)
summary(fit)
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
plot(cv_fit)
# coeffcientsValue <- predict(cv_fit,type = "coefficients")

#Lambda minimum value
clambda_best_ridge <- v_fit$lambda.min
#The best lambda value is 0.01
#The lowest point in the curve indicates the optimal lambda which is 0.01 in our case,
#and it's the log value of lambda that best minimised the error in cross-validation

#Building the best model
best_fit <- cv_fit$glmnet.fit
head(best_fit)
# Rebuilding the model with optimal lambda value
best_ridge <- glmnet(x, y, alpha = 0, lambda = 0lambda_best_ridge
#Ridge when lambda = 0
best_ridge_lambda0 <- glmnet(x, y, alpha = 0, lambda = 0)

#Plotting the coefficients for the best mridge odel
plot(coef(best_ridge))

ridgeLambda1 <- glmnet(x, y, alpha = 0, lambda = 1)
plot(coef(ridgeLambda1))




#6) Fitting lasso regression
yLasso <- modelData$Fat
xLasso <- model.matrix(Fat~., modelData)[,-1]
lambdasLasso <- 10^seq(3, -2, by = -.1)

fitLasso <- glmnet(xLasso, yLasso, alpha = 1, lambda = lambdasLasso)
summary(fitLasso)
cv_fit_laso <- cv.glmnet(xLasso, yLasso, alpha = 1, lambda = lambdasLasso)
plot(cv_fit_laso)
#Lambda minimum value
bestLambda <- cv_fit_laso$lambda.min
#Training the model with the best lambda value
fitLasso <- glmnet(xLasso, yLasso, alpha = 1, lambda = bestLambda)

#The best lambda value is 0.01
#The lowest point in the curve indicates the optimal lambda which is 0.01 in our case,
#and it's the log value of lambda that best minimised the error in cross-validation

#Building the best model
best_fit_lasso <- cv_fit_laso$glmnet.fit
head(best_fit_lasso)
# Rebuilding the model with optimal lambda value
best_lasso <- glmnet(xLasso, yLasso, alpha = 1, lambda = 0bestLambda
#Plotting the coefficients for the best model
plot(coef(best_lasso))


lassoLambda1 <- glmnet(xLasso, yLasso, alpha = 1, lambda = 1)
plot(coef(lassoLambda1))
#In lasso regression many parameters have become zero when lambda is minimum
