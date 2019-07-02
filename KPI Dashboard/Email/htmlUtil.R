require(tableHTML)
require(mailR)
require(sparkline)

convertToHTML <- function(df, server, group, value, start, end){
    html <- tableHTML(df, rownames = FALSE, footer = 'Please report errors to ZW') %>%
        replace_html(' <td id="tableHTML_column_1">([0-9]+)</td>',
              paste0('<td id="tableHTML_column_1"><a href = "192.168.1.59:5050/?server=',server,'&kpiType=',group,'&kpiValue=',value,'&start_dt=',start,'&end_dt=',end,'&kpi=\\1" >\\1</a></td>'), replace_all = TRUE)
    html
}


emailHTML <- function(debug = FALSE, recipients = "zwarnes@analysisworks.com", sender = "zwarnes@analysisworks.com", subject = "Test Dashboard Email", body){
    email <- send.mail(
        from = sender,
        to = recipients,
        subject = "KPI Quality Assurance Report",
        body = body,
        smtp = list(host.name = "securemail.megamailservers.com", port = 25),
        authenticate = FALSE,
        send = FALSE,
        html = TRUE)
    email$send()
}


fullQAEmail <- function(debug, kpiIndIds,kpiGroup, kpiValue, server){
    #a <- differencePercentages( debug, kpiIndIds, start, end, kpiGroup, kpiValue, server)
    #b <- statisticalAnomalies(debug, kpiIndIds, start, end, kpiGroup, kpiValue, server)
    #c <- periodDifferencePercentages( debug, kpiIndIds, start, end, kpiGroup, kpiValue, server)

    # Each date period below contains the start and end dates

    current_period <- list(start = "2019-05-02", end = "2019-05-30")
    previous_period <- list(start = "2019-04-01", end = "2019-05-01")
    yearToDate <- list(start = "2019-01-01", end = "2019-05-30")
    lastFiscalYear <- list(start = "2018-04-01", end = "2019-03-31")
    periods <- list(current_period, previous_period, yearToDate, lastFiscalYear)

    differenceVec <- data.frame(matrix(ncol = 2, nrow = 0))
    StatisticVec <- data.frame(matrix(ncol = 2, nrow = 0))
    PeriodCompVec <- data.frame(matrix(ncol = 2, nrow = 0))

    names(differenceVec) <- c('ind_id', 'metric')
    names(StatisticVec) <- c('ind_id', 'metric')
    names(PeriodCompVec) <- c('ind_id', 'metric')

    for( p in 1:length(periods)){
        st <- periods[[p]]$start
        ed <- periods[[p]]$end
        diff <- differencePercentages( debug, kpiIndIds, st, ed, kpiGroup, kpiValue, server)
        stat <- statisticalAnomalies(debug, kpiIndIds, st, ed, kpiGroup, kpiValue, server)
        ytd <- periodDifferencePercentages( debug, kpiIndIds, st, ed, kpiGroup, kpiValue, server)

        differenceVec[,p] <- diff$percent_difference
        percent_difference

        anomalies

        if( !is.null(df)){
            difference.sum[i,] <- list(ind_ids[[i]], round( (sum(  abs(df$series_1 - df$series_2) / (max(df$series_1, df$series_2 )) ) / period_len), 2  ))
        }
    }

    # Meta info for email
    #
    # format columns based on comp type
    #
    #
    #
    #
    # For each kpi type:
    #
    # list() to be joined to
    # foreach period
    #   a <- comp1
    #   b <- comp2
    #   c <- comp3
    #
    # form label from each field group
    # Update columns to link to dashboard
    # list() <- separate list by field group at this point
    #
    # for each group:
    # separate list by field group
    # remove unneeded fields
    #
    # append to Email with formatted header

    # TODO
    #
    # ED only query
    # Fake discharge/admit
    # Source KPIs
    # Dad KPIs
    # Efficiency KPIs
    # Authority Wide charts in Email
    #
    # filter by field group, sort by  field value
    #
    # CSS conditional formatting for high percentages
    # grouped headers
    #
    #
    # Data to pull in
    # Period to period comparison
    # Year to Year comparison
    # Anomaly counts
    # Cycle to cycle comparison
    #
    # for the following date ranges
    # Current period / last
    # fiscal YTD
    # last full fiscal year
    #


}
