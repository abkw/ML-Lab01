require(caret)
flds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
names(flds)[1] <- "train"