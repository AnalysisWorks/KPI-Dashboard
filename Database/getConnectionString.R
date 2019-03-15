library(RODBC)

# Establish a connection to the desired database
connectionString <- function( Server, Database, user = "ro", password = "read"){
	cs <- odbcDriverConnect( sprintf("driver={SQL Server};server=%s; database=%s;Uid=%s; Pwd=%s; trusted_connection=true", Server, Database, user, password))
	cs
}

# run a SQL query and return the results as a data frame
runSQL <- function(cs, sql, close = FALSE){
    dataTable <- sqlQuery( cs, sql)
    if(close){
        odbcClose(cs)
    }
    as.data.frame(dataTable)
}