
getKPIAverage <- function( debug, kpi, start, end, kpiGroup, kpiValue, server){
    sql <- kpiAverage(kpi, start, end, kpiGroup, kpiValue)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    avg <- getKPIs(cs, sql)
    if( is.null(avg)){
        return("-")
    }
    round(avg, 2)
}

getKPILastDate <- function( debug, kpi, kpiGroup, kpiValue, server){
    sql <- kpiLastDate(kpi, kpiGroup, kpiValue)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    dt <- getKPIs(cs, sql)
    dt
}

getKPIPercentile <- function( debug, kpi, start, end, kpiGroup, kpiValue, server, percentile){
    sql <- kpiPercentile(kpi, start, end, kpiGroup, kpiValue, percentile/ 100)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    perc <- getKPIs(cs, sql)
    if( is.null(perc)){
        return("-")
    }
    perc
}


getKPILastCycleAverage <- function( debug, kpi, start, end, kpiGroup, kpiValue, server){
    sql <- kpiLastCycleAverage(kpi, start, end, kpiGroup, kpiValue)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    avg <- getKPIs(cs, sql)
    if( is.null(avg)){
        return("-")
    }
    round(avg, 2)
}

getKPILastCycleDate <- function( debug, kpi, kpiGroup, kpiValue, server){
    sql <- kpiLastCycleDate(kpi, kpiGroup, kpiValue)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    dt <- getKPIs(cs, sql)
    dt
}

getKPILastCyclePercentile <- function( debug, kpi, start, end, kpiGroup, kpiValue, server, percentile){
    sql <- kpiLastCyclePercentile(kpi, start, end, kpiGroup, kpiValue, percentile/ 100)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    perc <- getKPIs(cs, sql)
    if( is.null(perc)){
        return("-")
    }
    perc
}