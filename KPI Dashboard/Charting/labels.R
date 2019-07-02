getChartLabels <- function(kpi, server, group, debug){
    if(debug){
            log_event("Start getChartLabels in labels.R")
    }

    if (is.null(kpi) || is.null(group) || is.null(server)){
        return(NULL)
    }

    sql <- kpiDetails(kpi , group)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    labels <- getKPIs(cs, sql)
    title <- ""
    
    if(is.null(labels)){
        return(NULL)
    }
    if (is.na(labels$group_field_1)) {
        title <- paste(
            labels$ind_group_cd,
            "Authority Wide",
            sep = ": ")
    }
    else if (is.na(labels$group_field_2)) {
        title <- paste(
            labels$ind_group_cd,
            paste(
                    labels$group_field_1,
                    labels$group_value_1,
                    sep = ": "),
            sep = ", ")
    }
    else if (is.na(labels$group_field_3)) {
        title <- paste(
            labels$ind_group_cd,
            paste(
                    labels$group_field_1,
                    labels$group_value_1,
                    sep = ": "),
            paste(
                    labels$group_field_2,
                    labels$group_value_2,
                    sep = ": "),
            sep = ", ")
    }
    else if (is.na(labels$group_field_4)) {
        title <- paste(
            labels$ind_group_cd,
            paste(
                    labels$group_field_1,
                    labels$group_value_1,
                    sep = ": "),
            paste(
                    labels$group_field_2,
                    labels$group_value_2,
                    sep = ": "),
            paste(
                    labels$group_field_3,
                    labels$group_value_3,
                    sep = ": "),
            sep = ", ")
    }
    else {
        title <- paste(
            labels$ind_group_cd,
            paste(
                    labels$group_field_1,
                    labels$group_value_1,
                    sep = ": "),
            paste(
                    labels$group_field_2,
                    labels$group_value_2,
                    sep = ": "),
            paste(
                    labels$group_field_3,
                    labels$group_value_3,
                    sep = ": "),
            paste(
                    labels$group_field_4,
                    labels$group_value_4,
                    sep = ": "),
            sep = ", ")
    }
    if(debug){
            log_event("Ending getChartLabels in labels.R")
    }
    return(title)
}