predictionTrend <- function(debug, kpi, start, end, server){
    if(debug){
        log_event("Starting predictionTrend in trend.R")
    }
    trainSQL <- kpiTrainData( kpi, start, end)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    train <- getKPIs(cs, trainSQL)

    testSQL <- kpiTestData(kpi, start, end)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    test <- getKPIs(cs, testSQL)

    prediction <- xgboostPrediction(train, test)
    err <- prediction$err
    pred <- round(prediction$pred)
    test$prediction <- pred

    p <- test %>%
    plot_ly(
        x = ~Date,
        y = ~ind_value_1,
        type = "scatter",
        mode = 'lines',
        fill = 'tozeroy',
        name = "Current Values"
    ) %>%
    add_trace(
        y = ~prediction,
        name = 'Predicted Values',
        mode = 'lines',
        fill = 'tozeroy'
    ) %>%
    layout(
        title = "<b>Predicted Data</b><br>test phase",
        xaxis = list(
            type = 'date'
        ),
        yaxis = list(
            title = "Volumes",
            rangemode = "tozero"
        )
    )
    if(debug){
        log_event("Ending predictionTrend in trend.R")
    }
    p
}
