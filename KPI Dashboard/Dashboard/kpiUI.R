library(stringr)
kpiUI <- function(inputId, id, label = "Select Value", server, group = "Beds", kpi = 2, debug){
    if(debug){
        log_event("Start kpiUI in kpiUI.R")
    }

    sql <- kpiSelect(group)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    kpis <- na.omit(getKPIs(cs, sql))

    return(
        selectizeInput(
            inputId, label = "Current KPI for Type Selected",
            choices = setNames(as.list( kpis$ind_id), kpis$group_value),
            selected = kpis$group_value[0]
        )
    )
}


kpiFilter <- function(term = "site_cd", server, group, debug){
    if(debug){
        log_event("Start kpiFilter in kpiUI.R")
    }
    sql <- kpiSelectWhere(group, term)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    kpis <- na.omit(getKPIs(cs, sql))

    return( setNames(as.list( kpis$ind_id), kpis$group_value))
}

kpiFieldsUI <- function( inputId, id, server, group, debug){
    if(debug){
        log_event("Start kpiFieldsUI in kpiUI.R")
    }

    sql <- kpiFields(group)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    kpis <- getKPIs(cs, sql)

    kpis_One = subset(kpis, is.na(group_field_2) == TRUE & is.na(group_field_1) == FALSE)
    kpis_Two = subset(kpis, is.na(group_field_3) == TRUE & is.na(group_field_2) == FALSE & is.na(group_field_1) == FALSE)
    kpis_Three = subset(kpis, is.na(group_field_4) == TRUE & is.na(group_field_3) == FALSE & is.na(group_field_2) == FALSE & is.na(group_field_1) == FALSE)
    kpis_Four = subset(kpis, is.na(group_field_4) == FALSE & is.na(group_field_3) == FALSE & is.na(group_field_2) == FALSE & is.na(group_field_1) == FALSE)

    return(
        selectizeInput(
            "indFields", label = "Select KPI Category",
            choices = list(
                Group_One = c( setNames(as.list( as.vector(kpis_One$group_field_label)), as.vector(kpis_One$group_field_1))),
                Group_Two = c( setNames(as.list( as.vector(kpis_Two$group_field_label)), as.vector(kpis_Two$group_field_2))),
                Group_Three = c( setNames(as.list( as.vector(kpis_Three$group_field_label)), as.vector(kpis_Three$group_field_3)))
                ),
            selected = "site_cd"
        )
    )
}
