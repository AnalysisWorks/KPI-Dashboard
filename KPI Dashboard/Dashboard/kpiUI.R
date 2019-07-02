
kpiUI <- function(inputId, id, label = "Select Value", server, parsedArgs, group = "Beds", debug){
    if(debug){
        log_event("Start kpiUI in kpiUI.R")
    }

    sql <- kpiDetailsAll(group)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    kpis <- getKPIs(cs, sql)

    if (!is.null(parsedArgs[['kpi']])) {
        return(
            selectizeInput(
                inputId, "Current KPI", 
                choices = unique(kpis$ind_id),
                selected = parsedArgs[['kpi']]
                )
        )
    }
    return(
        selectizeInput(
            inputId, "Current KPI", 
            choices = unique(kpis$ind_id)
        )
    )
}