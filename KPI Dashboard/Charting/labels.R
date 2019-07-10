getChartLabels <- function(kpi, server, group, debug){
    if(debug){
            log_event("Start getChartLabels in labels.R")
    }

    sql <- kpiDetails(kpi , group)
    cs <- connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators")
    labels <- getKPIs(cs, sql)
    title <- ""

    if(is.null(labels) || nrow(labels) == 0){
        return(NULL)
    }

    if (is.na(labels$group_field_1)) {
        title <- paste(
            labels$ind_group_cd,
            "Authority Wide",
            sep = ": ")
    }
    else if (is.na(labels$group_field_2)) {
        title <- paste0(
                labels$ind_group_cd, " Selected, Filtered on: ",
                labels$group_field_1, ": ", labels$group_value_1
            )
    }
    else if (is.na(labels$group_field_3)) {
        title <- paste0(
                labels$ind_group_cd, " Selected, Filtered on: ",
                labels$group_field_1, ": ", labels$group_value_1, " - ",
                labels$group_field_2, ": ", labels$group_value_2)
    }
    else if (is.na(labels$group_field_4)) {
        title <- paste0(
                labels$ind_group_cd, " Selected, Filtered on: ",
                labels$group_field_1, ": ", labels$group_value_1, " - ",
                labels$group_field_2, ": ", labels$group_value_2, " - ",
                labels$group_field_3, ": ", labels$group_value_3)
    }
    else {
        title <- paste0(
                labels$ind_group_cd, " Selected, Filtered on: ",
                labels$group_field_1, ": ", labels$group_value_1, " - ",
                labels$group_field_2, ": ", labels$group_value_2, " - ",
                labels$group_field_3, ": ", labels$group_value_3, " - ",
                labels$group_field_4, ": ", labels$group_value_4)
    }
    if(debug){
            log_event("Ending getChartLabels in labels.R")
    }
    return(title)
}
