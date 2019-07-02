kpiValueUI <- function( inputId, id, server, parsedArgs, group, debug){
    if(debug){
        log_event("Start kpiValueUI in kpiValueUI.R")
    }
    query <- parsedArgs
    if(is.null(group)){
        return(NULL)
    }
    if(group == 'transfers'){
        return( selectizeInput( 
                    inputId, label = "Select Value",
                    choices = c(
                        "Primary Value" = "1",
                        "Admits" = "2",
                        "Discharges" = "3",
                        "Transfers" = "4",
                        "Other" = "5"),
                    options = list(
                        placeholder = 'Select a KPI Value'
                )))
    }
    if(group == 'financials'){
        return( selectizeInput( 
                    inputId, label = "Select Value",
                    choices = c(
                        "Actual Period" = "1",
                        "Actual YTD" = "2",
                        "Budget Period" = "3",
                        "Budget YTD" = "4"),
                    options = list(
                        placeholder = 'Select a KPI Value'
                )))
    }
    else{
        return(selectizeInput(
                inputId, label = "Select Value", 
                choices = c("Primary Value" = "1"),
                options = list(
                        placeholder = 'Select a KPI Value'
                )))
    }
}