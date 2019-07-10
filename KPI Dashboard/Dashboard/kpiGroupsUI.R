kpiGroupsUI <- function(inputId, id, label = "Select Value", server, kpiType = "Beds", debug){
    if(debug){
        log_event("Start kpiGroupsUI in kpiGroupsUI.R")
    }
    sql <- kpiTypes()
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    df <- as.data.frame(getKPIs( cs, sql))

    return( selectizeInput(
        inputId, label = "Select KPI Type",
        choices = setNames(as.list( df$ind_group_cd), df$ind_group_desc),
        selected = kpiType,
        options = list(
            placeholder = 'Select a KPI type'
    )))
}
