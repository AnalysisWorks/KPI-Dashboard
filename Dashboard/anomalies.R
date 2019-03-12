library(anomalyDetection)
library(dplyr)
library(anomalize)

# Get Time series anomalies,
# time decomposition options are: "stl", "twitter"
# anomalize methods are: "iqr", "gesd"
getAnomalies <- function( df, targetField, timeMethod = "twitter", timePeriod = "2 months", anomMethod = "iqr"){
    # Data Manipulation / Anomaly Detection
    df %>%
        time_decompose( targetField, method = timeMethod, trend = timePeriod) %>%
        anomalize(remainder, method = anomMethod) %>%
        time_recompose()
    df
}

getAnomalyBreakDown <- function( df, targetField, timeMethod = "stl", timePeriod = "2 months", anomMethod = "iqr"){
    # Data Manipulation / Anomaly Detection
    df %>%
        ungroup() %>%
        time_decompose( ind_value_1, method = timeMethod, trend = timePeriod) %>%
        anomalize(remainder, method = anomMethod)
    df
}

getPeriod <- function(){
    # Determine automatically the time frequency and time trends
    time_frequency(df, period = "auto")
    time_trend(df, period = "auto")
}
