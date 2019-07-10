require(plotly)
require(xgboost)


statisticalAnomalyTrend <- function(debug, df, label){
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

yearComparisonTrend <- function(debug, df, label){
    if(debug){
        log_event("Starting yearComparisonTrend in trend.R")
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
        log_event("Ending yearComparisonTrend in trend.R")
    }
    p
}

cycleComparisonTrend <- function(debug, df, label){
    if(debug){
        log_event("Starting cycleComparisonTrend in trend.R")
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
        log_event("Ending cycleComparisonTrend in trend.R")
    }
    p
}


