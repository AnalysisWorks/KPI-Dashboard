library(dplyr)
library(e1071)
library(xgboost)
library(Matrix)
library(readr)
library(pROC)


scaled_err <- function(x) { 
    x / sqrt(sum(x^2))
}
# expects a training and test dataset with same number of columns
# the feature to predict is in the first column
# the rest are the training data
# returns the predicted output as well as the average error off the predicted value
xgboostPrediction <- function(train, test){
    train <- as.matrix(train)
    test <- as.matrix(test)
    train.label <- train[,1]
    test.label <- test[,1]
    test.data <- test[,2:ncol(test)]
    train.data <- train[,2:ncol(train)]
    train.data <- Matrix( train.data, sparse =TRUE)
    test.data <- Matrix( test.data, sparse =TRUE)

    bstSparse <- xgboost(   data = train.data, 
                            label = train.label, 
                            max.depth = 10, # tree depth
                            eta = 0.1, # learning rate
                            nthread = 6, # parallelization
                            nrounds = 100,  # epochs
                            subsample = 0.15, # CV
                            gamma = 1.0, # minimum loss reduction for leaf
                            min_child_weight = 1.0, # min leaf partition
                            lambda = 1.5, # L2 regularization
                            verbosity = 0
                        )
    pred <- predict(bstSparse, test.data)
    err <- mean( pred - test.label)
    err <- scaled_err(err)
    
    r <- list( "pred" = pred, "err" = err)
    r
}