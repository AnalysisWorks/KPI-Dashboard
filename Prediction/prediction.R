library(dplyr)
library(e1071)
library(xgboost)
library(Matrix)
library(readr)
library(pROC)

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

# server reactive code
getPredictedKPIs <- eventReactive(input$anomalies,{
    table <- predictedDifferences(debug, getKeyMapping(), getStartDt(), getEndDt(), getGrouping(), getVal(), getServer())
    table
})

# server UI code
if( input$chartTabs == 'prediction_anomalies'){
    return(predictionTrend(debug, getKPI(), getStartDt(), getEndDt(), getServer()))
}


# Table generation code
predictedDifferences <- function( debug, kpiIndIds, start, end, kpiGroup, kpiValue, server){
    if(debug){
            log_event("Start predictedDifferences in tableData.R")
    }
    kpi_ind_ids <- kpiIndIds
    ind_ids <- cbind(unique(kpi_ind_ids$kpi))
    date_range <- as.data.frame(seq(as.Date(start), as.Date(end), "days"))
    names(date_range) <- c("Date")

    anomaly.error <- data.frame(matrix(ncol = 2, nrow = 0))
    names(anomaly.error) <- c('ind_id', 'error')
    for (i in 1:length(ind_ids)) {
        trainSQL <- kpiTrainData(kpi, start, end)
        cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
        train <- getKPIs(cs, trainSQL)

        testSQL <- kpiTestData(kpi, start, end)
        cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
        test <- getKPIs(cs, testSQL)

        result <- tryCatch({
            prediction <- xgboostPrediction(train, test)
            }, error = function(e) {
                NULL
            }, finally = {
                NULL
            })
        if( !is.null(result)){
            err <- prediction$err
            anomaly.error[i,] <- list(ind_ids[[i]], err)
        }
    }
    sql <- kpiDetailsAll(kpiGroup)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    labels <- getKPIs(cs, sql)
    qa_table <- inner_join(anomaly.error[order(-anomaly.error$error),], labels)
    if(debug){
            log_event("End predictedDifferences in tableData.R")
    }
    qa_table
}

