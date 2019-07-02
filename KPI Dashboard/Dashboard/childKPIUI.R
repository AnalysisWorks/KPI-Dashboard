require(dplyr)
childKPIUI <- function(inputId, id, kpi, server, group = "Beds", debug){
    if(debug){
        log_event("Start childKPIUI in childKPIUI.R")
    }
    mapping <- getCompleteMapping(server, group, debug)
    if(is.null(mapping)){
        if(debug){
            log_event("childKPIs call to mapping returned NULL")
        } 
        return(NULL)
    }
    if(is.null(server) || is.null(kpi)){
        if(debug){
            log_event(paste("childKPIs server", server, "KPI", kpi, sep = ": "))
        }
        return(NULL)
    }

    children <- mapping %>%
        dplyr::filter(mapping$kpi == kpi)
    if (all(is.na(children$child_group_value_2))) {
        if(debug){
            log_event("End childKPIUI 1 in childKPIUI.R")
        }
        return(selectizeInput(inputId,
                                "Child KPIs:",
                                choices = setNames(as.list( children$child_kpi), paste( children$child_group_field_1, children$child_group_value_1, sep = ": "))
                            )
                )
    }
    if (all(is.na(children$child_group_value_3))) {
        if(debug){
            log_event("End childKPIUI 2 in childKPIUI.R")
        }
        return(selectizeInput(inputId,
                                "Child KPIs:",
                                choices = setNames(as.list(children$child_kpi), paste(children$child_group_field_2, children$child_group_value_2, sep = ": "))
                            )
                )
    }
    if (all(is.na(children$child_group_value_4))) {
        if(debug){
            log_event("End childKPIUI 3 in childKPIUI.R")
        }
        return(selectizeInput(inputId,
                                "Child KPIs:",
                                choices = setNames(as.list(children$child_kpi), paste( children$child_group_field_3, children$child_group_value_3, sep = ": "))
                            )
                )
    }
    if(debug){
        log_event("End childKPIUI 4 in childKPIUI.R")
    }
    return(selectizeInput(inputId,
                                "Child KPIs:",
                                choices = setNames(as.list(children$child_kpi), paste( children$child_group_field_4, children$child_group_value_4, sep = ": "))
                            )
                )
}