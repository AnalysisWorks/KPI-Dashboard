require(dplyr)

getParentIndicator <- function(debug, kpi, server, mapping){
    if(debug){
        log_event("Start getParentIndicator in getKPI.R")
    }
    if(is.null(server) || is.null(kpi)){
        if(debug){
            log_event(paste("getParentKPI server",server,"KPI", kpi, sep = ": "))
            }   
        return(NULL)
    }
    if(is.null(mapping)){
    if(debug){
        log_event("getParentKPI call to mapping returned NULL")
    } 
    return(NULL)
    }
    currentKPI <- mapping[which(mapping$kpi == kpi), ]
    parentKPI <- unique(currentKPI$parent_kpi)

    if(debug){
        log_event("End getParentIndicator in getKPI.R")
    }
    parentKPI
}

getChildIndicator <- function(debug, kpi, server, mapping){
    if(debug){
        log_event("Start getChildIndicator in getKPI.R")
    }
    if(is.null(server) || is.null(kpi)){
            if(debug){
            log_event(paste("getChildKPI server",server,"childKPI", kpi, sep = ": "))
            }   
        return(NULL)
    }
    if(is.null(mapping)){
    if(debug){
        log_event("getChildKPI call to mapping returned NULL")
    } 
    return(NULL)
    }
    currentKPI <- mapping[which(mapping$kpi == kpi), ]
    childKPI <- unique(currentKPI$kpi)
    if(debug){
        log_event("End getChildIndicator in getKPI.R")
    }
    childKPI
}