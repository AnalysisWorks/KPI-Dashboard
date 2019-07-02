

trendAnomalySeries <- function(debug, start,end, kpi, kpiValue, kpiGroup, server){
    if(debug){
        log_event("Starting trendAnomalySeries in chartData.R")
    }
    if(is.null(server) || is.null(kpi)){
        if(debug){
            log_event(paste("trendAnomalySeries, server", server, "KPI", kpi, sep = ": "))
        }
        return(NULL)
    }
    if(is.null(kpiValue) || is.null(kpiGroup)){
        if(debug){
            log_event(paste("trendAnomalySeries, KPI Value", kpiValue, "KPI Group", kpiGroup, sep = ": "))
        }
        return(NULL)
    }
    sql <- kpiQuery(kpi, start, end, kpiGroup, kpiValue)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    kpiTable <- kpiTable(cs, sql)

    # padd zero days
    date_range <- as.data.frame(seq(as.Date(start), as.Date(end), "days"))
    names(date_range) <- c("Date")
    kpiTable <- left_join(date_range, kpiTable)
    kpiTable[ is.na(kpiTable)] <- 0
    kpiTable[3] <- kpi

    sql <- kpiQuery(kpi, start, end, kpiGroup, kpiValue)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    df <- kpiTable(cs, sql)
    
    df <- left_join(date_range, df)
    df[is.na(df)] <- 0
    df[3] <- kpi
    df <- as_data_frame(df)

    anomalies <- df %>%
        time_decompose(ind_value, method = "twitter", trend = "3 months") %>%
        anomalize(remainder, method = "gesd") %>%
        time_recompose()
    kpiTable <- left_join(kpiTable, anomalies)
    if(debug){
        log_event("Ending trendAnomalySeries in chartData.R")
    }
    kpiTable
}