kpiValueUI <- function( inputId, id, server, group, debug){
    if(debug){
        log_event("Start kpiValueUI in kpiValueUI.R")
    }
    df <- as.data.frame(getKPIs(
            connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators"),
            kpiValues(group)
        ))
    df <- na.omit(as.data.frame(t(df)))
    df <- c(as.matrix(df))

    return( selectizeInput(
                inputId, label = "Select Metric",
                choices = setNames(as.list( 1:length(df)), df),
                options = list(
                    placeholder = 'Select a KPI Value'
            )))
}
