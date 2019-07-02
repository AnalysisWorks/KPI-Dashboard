kpiGroupsUI <- function(inputId, id, label = "Select Value", server, parsedArgs, debug){
    if(debug){
        log_event("Start kpiGroupsUI in kpiGroupsUI.R")
    }
    query <- parsedArgs
    if(server == 'islandhealth' || server == 'vancouvercoastal'|| server == 'wohs'){
        if (!is.null(query[['kpiType']])) {
            if( server == 'vancouvercoastal' ){
                return( selectizeInput(
                    inputId, label = "KPI",
                    choices = c(
                            "Bed Utilization" = "Beds",
                            "ALC Utilization" = "ALC Beds",
                            "ADT Events" = "transfers",
                            "Surgical Cases" = 'srg_cases'),
                    selected = query[['kpiType']],
                    options = list(
                        placeholder = 'Select a KPI type'
                )))
            }
            return( selectizeInput(
                    inputId, label = "KPI",
                    choices = c(
                            "Bed Utilization" = "Beds",
                            "ALC Utilization" = "ALC Beds",
                            "ADT Events" = "transfers"),
                    selected = query[['kpiType']],
                    options = list(
                        placeholder = 'Select a KPI type'
                )))
        }
        if(server == 'islandhealth'){
        return( selectizeInput(
                    inputId, label = "KPI",
                    choices = c(
                            "Bed Utilization" = "Beds",
                            "ALC Utilization" = "ALC Beds",
                            "ADT Events" = "transfers",
                            "Financials" = "financials"),
                    selected = query[['kpiType']],
                    options = list(
                        placeholder = 'Select a KPI type'
                )))
        }
        if( server == 'vancouvercoastal' ){
                return( selectizeInput(
                    inputId, label = "KPI",
                    choices = c(
                            "Bed Utilization" = "Beds",
                            "ALC Utilization" = "ALC Beds",
                            "ADT Events" = "transfers",
                            "Surgical Cases" = 'srg_cases'),
                    options = list(
                        placeholder = 'Select a KPI type'
                )))
            }
        return( selectizeInput(
                inputId, label = "KPI",
                choices = c(
                        "Bed Utilization" = "Beds",
                        "ALC Utilization" = "ALC Beds",
                        "ADT Events" = "transfers"),
                options = list(
                    placeholder = 'Select a KPI type'
                )))
    }else{
        if (!is.null(query[['kpiType']])) {
            return( selectizeInput(
                    inputId, label = "KPI",
                    choices = c("Bed Utilization" = "Beds"),
                    selected = query[['kpiType']],
                    options = list(
                        placeholder = 'Select a KPI type'
                )))
        }
        return( selectizeInput( 
                inputId, label = "KPI", 
                choices = c("Bed Utilization" = "Beds"),
                options = list(
                    placeholder = 'Select a KPI type'
                )))
    }
}