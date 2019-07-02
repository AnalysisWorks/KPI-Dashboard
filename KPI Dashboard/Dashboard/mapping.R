getCompleteMapping <- function(server, group, debug){
    if(debug){
            log_event("Start getCompleteMapping in mapping.R")
    }
    if(is.null(server) || is.null(group)){
        if(debug){
        log_event(paste("KPI server:", server, "KPI Group:", group, sep = " "))
        }
        return(NULL)
    }
    df <- as.data.frame(getKPIs(
            connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators"),
            kpiMapping(group)
        ))
    if(debug){
            log_event("End getCompleteMapping in mapping.R")
    }
    df
}