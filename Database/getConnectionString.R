library(RODBC)

# Establish a connection to the desired database
connectionString <- function(Server, Database, user = "ro", password = "read") {
    cs <- odbcDriverConnect(sprintf("driver={SQL Server};server=%s; database=%s;Uid=%s; Pwd=%s; trusted_connection=true", Server, Database, user, password))
    cs
}

# run a SQL query and return the results as a data frame
runSQL <- function(cs, sql, close = FALSE) {
    dataTable <- sqlQuery(cs, sql)
    if (close) {
    odbcClose(cs)
    }
    as.data.frame(dataTable)
}


kpiTable <- function(cs, sql) {
    kpiTable <- sqlQuery(cs, sql)
    odbcClose(cs)
    kpiTable[, 1] <- as.Date(as.character(kpiTable[, 1]), form = "%Y-%m-%d")
    kpitable <- as_data_frame(kpiTable)
    kpiTable
}

kpiMap <- function(cs, sql) {
    kpiTable <- sqlQuery(cs, sql)
    odbcClose(cs)
    kpitable <- as_data_frame(kpiTable)
}

getKPIs <- function(cs, sql) {
    if(is.null(sql)){
        warning("Attempt to query with NULL SQL")
        odbcClose(cs)
        return(NULL)
    }
    kpiTable <- tryCatch({
            sqlQuery(cs, sql)
            }, error = function(e) {
                NULL
            }, finally = {
                NULL
            })
    odbcClose(cs)
    if(is.null(kpiTable)){
        return(NULL)
    }
    kpitable <- as.data.frame(kpiTable)
    kpitable
}
