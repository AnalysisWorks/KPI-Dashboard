statisticalAnomalies <- function(debug, kpiIndIds, start, end, kpiGroup, kpiValue, server){
    if(debug){
            log_event("Start statisticalAnomalies in tableData.R")
    }

    kpi_ind_ids <- kpiIndIds
    ind_ids <- cbind(unique(kpi_ind_ids$kpi))
    date_range <- as.data.frame(seq(as.Date(start), as.Date(end), "days"))
    names(date_range) <- c("Date")

    anomaly.counts <- data.frame(matrix(ncol = 2, nrow = 0))
    names(anomaly.counts) <- c('ind_id', 'anomalies')
    for (i in 1:length(ind_ids)) {
        sql <- kpiQuery(ind_ids[[i]], start, end, kpiGroup, kpiValue)
        cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
        df <- as_data_frame(kpiTable(cs, sql))

        if (nrow(df) < 10) {
            next
        }
        df <- left_join(date_range, df)
        df$ind_id <- ind_ids[[i]]
        df[is.na(df)] <- 0
        df <- as_tibble(df)

        result <- tryCatch({
            anomalies <- df %>%
                        time_decompose(ind_value, method = "twitter", trend = "3 months") %>%
                        anomalize(remainder, method = "gesd") %>%
                        time_recompose()
            }, error = function(e) {
                NULL
            }, finally = {
                NULL
            })
        if( !is.null(result)){
            anomaly.counts[i,] <- list(ind_ids[[i]], sum(anomalies$anomaly == 'Yes'))
        }
    }
    sql <- kpiDetailsAll(kpiGroup)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    labels <- getKPIs(cs, sql)
    qa_table <- inner_join(anomaly.counts[order(-anomaly.counts$anomalies),], labels)
    if(debug){
            log_event("End statisticalAnomalies in tableData.R")
    }

    qa_table
}


differencePercentages <- function( debug, kpiIndIds, start, end, kpiGroup, kpiValue, server){
    if(debug){
            log_event("Start differencePercentages in tableData.R")
    }

    kpi_ind_ids <- kpiIndIds
    ind_ids <- cbind(unique(kpi_ind_ids$kpi))
    date_range <- as.data.frame(seq(as.Date(start), as.Date(end), "days"))
    names(date_range) <- c("Date")
    period_len <- as.numeric(as.Date(end) - as.Date(start))
    difference.sum <- data.frame(matrix(ncol = 2, nrow = 0))
    names(difference.sum) <- c('ind_id', 'percent_difference')
    for (i in 1:length(ind_ids)) {
        sql <- kpiPeriodComparision(ind_ids[[i]], start, end, kpiGroup, kpiValue)
        cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
        df <- kpiTable(cs, sql)

        if (nrow(df) < 10) {
            next
        }
        df <- left_join(date_range, df)
        df$ind_id <- ind_ids[[i]]
        df[is.na(df)] <- 0
        df <- as_data_frame(df)


        if( !is.null(df)){
            difference.sum[i,] <- list(ind_ids[[i]], round( (sum(  abs(df$series_1 - df$series_2) / (max(df$series_1, df$series_2 )) ) / period_len), 2  ))
        }
    }
    sql <- kpiDetailsAll(kpiGroup)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    labels <- getKPIs(cs, sql)
    qa_table <- inner_join(difference.sum[order(-difference.sum$percent_difference),], labels)

    if(debug){
            log_event("End differencePercentages in tableData.R")
    }
    qa_table
}

periodDifferencePercentages <- function( debug, kpiIndIds, start, end, kpiGroup, kpiValue, server){
    if(debug){
            log_event("Start periodDifferencePercentages in tableData.R")
    }

    kpi_ind_ids <- kpiIndIds
    ind_ids <- cbind(unique(kpi_ind_ids$kpi))
    date_range <- as.data.frame(seq(as.Date(start), as.Date(end), "days"))
    names(date_range) <- c("Date")
    period_len <- as.numeric(as.Date(end) - as.Date(start))
    difference.sum <- data.frame(matrix(ncol = 2, nrow = 0))
    names(difference.sum) <- c('ind_id', 'percent_difference')
    for (i in 1:length(ind_ids)) {
        sql <- kpiPeriodComparisionTrend(ind_ids[[i]], start, end, kpiGroup, kpiValue)
        cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
        df <- kpiTable(cs, sql)

        if (nrow(df) < 10) {
            next
        }
        df <- left_join(date_range, df)
        df$ind_id <- ind_ids[[i]]
        df[is.na(df)] <- 0
        df <- as_data_frame(df)


        if( !is.null(df)){
            difference.sum[i,] <- list(ind_ids[[i]], round( (sum(  abs(df$series_1 - df$series_2) / (max(df$series_1, df$series_2 )) ) / period_len), 2  ))
        }
    }
    sql <- kpiDetailsAll(kpiGroup)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    labels <- getKPIs(cs, sql)
    qa_table <- inner_join(difference.sum[order(-difference.sum$percent_difference),], labels)

    if(debug){
            log_event("End periodDifferencePercentages in tableData.R")
    }
    qa_table
}
