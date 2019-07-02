require(plotly)
require(xgboost)
trend <- function(debug, df, label){
    if(debug){
        log_event("Starting trend in trend.R")
    }
    if(is.null(df)){
        return(NULL)
    }
    p <- plot_ly(
        df,
        x = ~Date,
        y = ~ind_value,
        name = "Current Value",
        symbol = ~anomaly,
        symbols = c('circle', 'x'),
        colors = c('red', 'gray'),
        type = 'scatter',
        mode = 'markers',
        source = "subset"
    ) %>%
    layout(
        title = label,
        xaxis = list(title = 'Date'),
        yaxis = list(title = 'Value',
        rangemode = "tozero")
    )
    mytext = paste("Current Value = ", df$ind_value, sep = "")
    pp = plotly_build(p)
    style(pp, text = mytext, hoverinfo = "text", traces = c(1, 1))
    if(debug){
        log_event("Ending trend in trend.R")
    }
    p
}

interactiveTrend <- function(debug, df, label){
    if(debug){
        log_event("Starting interactiveTrend in trend.R")
    }
    p <- plot_ly(
                df,
                x = ~Date,
                y = ~series_1,
                name = "Current Value",
                type = 'scatter',
                mode = 'lines+markers',
                source = "subset"
            ) %>%
            add_trace(
                y = ~series_2,
                name = 'Previous Year value',
                mode = 'lines+markers'
            ) %>%
            layout(
                title = label,
                xaxis = list(title = 'Date'),
                yaxis = list(title = 'Value',
                rangemode = "tozero")
            )
    mytext = paste("Current Value = ", df$series_1, "\n", "Previous Year Value = ", df$series_2, "\n", sep = "")
    pp = plotly_build(p)
    style(pp, text = mytext, hoverinfo = "text", traces = c(1, 2, 3))
    if(debug){
        log_event("Ending interactiveTrend in trend.R")
    }
    p
}

comparisionTrend <- function(debug, df, label){
    if(debug){
        log_event("Starting comparisionTrend in trend.R")
    }
    p <- df %>%
        plot_ly(
            x = ~Date,
            y = ~series_1,
            type = "scatter",
            mode = 'lines',
            fill = 'tozeroy',
            name = "Current KPIs"
        ) %>%
        add_trace(
            y = ~df$series_2,
            name = 'Previous KPIs',
            mode = 'lines',
            fill = 'tozeroy'
        ) %>%
        layout(
            title = label,
            xaxis = list(title = 'Date'),
            yaxis = list(title = 'Value',
            rangemode = "tozero")
        )
    if(debug){
        log_event("Ending comparisionTrend in trend.R")
    }
    p
}


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