kpiQuery <- function(kpi, start, end, group, value){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT
        dt AS [Date],
        ind_value_%s AS ind_value,
        ind_id
    FROM
        kpi.ind_data_detail
    WHERE
        period_cd = 'day'
        AND dt BETWEEN '%s' AND '%s'
        AND ind_id = %s
        AND ind_group_cd = '%s'
    ORDER BY
        dt,
        ind_id

    ;",value, start, end, kpi, group)
    sql
}

kpiTypes <- function(){
    sql <- "
    SELECT
        [ind_group_cd],
        [ind_group_desc]
    FROM
        [kpi].[prm_ind_type_desc]
    ORDER BY
        IIF( [ind_group_cd] = 'Beds', 'aaa', ind_group_cd)
    "
    sql
}

kpiValues <- function(group){
    sql <- paste0("
    SELECT
        [ind_value_1_desc],
        [ind_value_2_desc],
        [ind_value_3_desc],
        [ind_value_4_desc],
        [ind_value_5_desc]
    FROM
        [kpi].[prm_ind_type_desc]
    WHERE
        ind_group_cd = '", group, "'
    ")
    sql
}

kpiFields <- function(group){
    sql <- paste0("
    SELECT
        DISTINCT
        ISNULL( [group_field_1], '') + ISNULL( '>' + [group_field_2], '') + ISNULL( '>' + [group_field_3], '') + ISNULL( '>' + [group_field_4], '') AS group_field_label,
        group_field_1,
        group_field_2,
        group_field_3,
        group_field_4
    FROM
        [kpi].ind_type
    WHERE
        ind_group_cd = '", group, "'
        AND group_field_1 IS NOT NULL
    ")
    sql
}

kpiQueryAll <- function(start, end, group, value){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT
        dt AS [Date],
        ind_value_%s AS ind_value,
        ind_id
    FROM
        kpi.ind_data_detail
    WHERE
        period_cd = 'day'
        AND dt BETWEEN '%s' AND '%s'
        AND ind_group_cd = '%s'
    ORDER BY
        ind_id,
        dt

    ;",value, start, end, group)
    sql
}


kpiIDs <- function(group){
    sql <- paste0("
        SELECT
            [ind_id]
        FROM
            [kpi].[ind_type_base]
        WHERE
            ind_group_cd = '", group ,"'
        ")
    sql
}


kpiSelect <- function(group){
    sql <- paste0("
        SELECT
            DISTINCT
            ind_id,
            ISNULL( [group_field_1], '') + ISNULL( '>' + [group_field_2], '') + ISNULL( '>' + [group_field_3], '') + ISNULL( '>' + [group_field_4], '') AS group_field,
            ISNULL( [group_value_1], '') + ISNULL( '>' + [group_value_2], '') + ISNULL( '>' + [group_value_3], '') + ISNULL( '>' + [group_value_4], '') AS group_value,
            LEN(ISNULL( [group_field_1], '') + ISNULL( '>' + [group_field_2], '') + ISNULL( '>' + [group_field_3], '') + ISNULL( '>' + [group_field_4], '')) AS length
        FROM
            [kpi].[ind_type]
        WHERE
            ind_group_cd = '", group,"'
        ORDER BY
            LEN(ISNULL( [group_field_1], '') + ISNULL( '>' + [group_field_2], '') + ISNULL( '>' + [group_field_3], '') + ISNULL( '>' + [group_field_4], '')),
            ISNULL( [group_value_1], '') + ISNULL( '>' + [group_value_2], '') + ISNULL( '>' + [group_value_3], '') + ISNULL( '>' + [group_value_4], '')
    ")
    sql
}

kpiSelectWhere <- function(group, match){
    sql <- paste0("
        SELECT
            DISTINCT
            ind_id,
            ISNULL( [group_field_1], '') + ISNULL( '>' + [group_field_2], '') + ISNULL( '>' + [group_field_3], '') + ISNULL( '>' + [group_field_4], '') AS group_field,
            ISNULL( [group_value_1], '') + ISNULL( '>' + [group_value_2], '') + ISNULL( '>' + [group_value_3], '') + ISNULL( '>' + [group_value_4], '') AS group_value,
            LEN(ISNULL( [group_field_1], '') + ISNULL( '>' + [group_field_2], '') + ISNULL( '>' + [group_field_3], '') + ISNULL( '>' + [group_field_4], '')) AS length
        FROM
            [kpi].[ind_type]
        WHERE
            ind_group_cd = '", group,"'
            AND ISNULL( [group_field_1], '') + ISNULL( '>' + [group_field_2], '') + ISNULL( '>' + [group_field_3], '') + ISNULL( '>' + [group_field_4], '') = '",match,"'
        ORDER BY
            LEN(ISNULL( [group_field_1], '') + ISNULL( '>' + [group_field_2], '') + ISNULL( '>' + [group_field_3], '') + ISNULL( '>' + [group_field_4], '')),
            ISNULL( [group_value_1], '') + ISNULL( '>' + [group_value_2], '') + ISNULL( '>' + [group_value_3], '') + ISNULL( '>' + [group_value_4], '')
    ")
    sql
}

kpiSelectOne <- function(kpi){
    sql <- paste0("
        SELECT
            ISNULL( [group_field_1], '') + ISNULL( '>' + [group_field_2], '') + ISNULL( '>' + [group_field_3], '') + ISNULL( '>' + [group_field_4], '') AS group_field,
            ISNULL( [group_value_1], '') + ISNULL( '>' + [group_value_2], '') + ISNULL( '>' + [group_value_3], '') + ISNULL( '>' + [group_value_4], '') AS group_value
        FROM
            [kpi].[ind_type]
        WHERE
            ind_id = '", kpi,"'
    ")
    sql
}

kpiPeriodComparision <- function(kpi, start, end, group, value){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- paste0("
    SELECT
        dt AS [Date],
        [ind_id],
        [ind_value_",value,"] AS series_1,
        [ind_prev_value_",value,"] As series_2
    FROM
        [kpi].[ind_cycle_data_base]
    where
        ind_id = '",kpi,"'
        AND dt BETWEEN '",start,"' AND '",end,"'
        AND ind_group_cd = '",group,"'
    ORDER BY
        dt,
        ind_id
    ;")
}


kpiPeriodComparisionTrend <- function(kpi, start, end, group, value){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- paste0("
    SELECT
        [dt] AS [Date],
        [ind_value_",value,"] AS series_1,
        [one_year_prev_ind_value_",value,"] AS series_2,
        ind_id
    FROM
        [kpi].[ind_annual_data_base]
    WHERE
        ind_id = '",kpi,"'
        AND dt BETWEEN '",start,"' AND '",end,"'
        AND ind_group_cd = '",group,"'
    ORDER BY
        dt,
        ind_id
    ;")
}

kpiDayIndicators <- function(group){
    if(is.null(group)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT
        DISTINCT
        ind_id
    FROM
        kpi.ind_type
    WHERE
        period_cd = 'day'
        AND ind_group_cd = '%s'
    ;",group )
}

kpiDetails <- function(kpi, group){
    if(is.null(group) || is.null(kpi)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT
        DISTINCT
        [ind_id],
        [ind_group_cd],
        [group_field_1],
        [group_value_1],
        [group_field_2],
        [group_value_2],
        [group_field_3],
        [group_value_3],
        [group_field_4],
        [group_value_4]
    FROM
        [kpi].[ind_type]
    WHERE
        ind_id = %s
        AND ind_group_cd = '%s'
        AND period_cd = 'day' ;", kpi, group
    )
    sql
}

kpiDetailsAll <- function(group){
    if(is.null(group)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT
        DISTINCT
        [ind_id],
        [ind_group_cd],
        [group_field_1],
        [group_value_1],
        [group_field_2],
        [group_value_2],
        [group_field_3],
        [group_value_3],
        [group_field_4],
        [group_value_4]
    FROM
        [kpi].[ind_type]
    WHERE
        ind_group_cd = '%s'
        AND period_cd = 'day'
    ;", group)
    sql
}

kpiGroupDetails <- function(group){
    if(is.null(group)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT
        DISTINCT
        [ind_id] AS [kpi],
        [ind_group_cd],
        [group_field_1],
        [group_value_1],
        [group_field_2],
        [group_value_2],
        [group_field_3],
        [group_value_3],
        [group_field_4],
        [group_value_4]
    FROM
        [kpi].ind_type
    WHERE
        ind_group_cd = '%s'
        AND period_cd = 'day'
    ;", group)
    sql
}

kpiAverage <- function(kpi, start, end, group, value){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT
        AVG( ind_value_%s) AS ind_value
    FROM
        kpi.ind_data_detail
    WHERE
        period_cd = 'day'
        AND dt BETWEEN '%s' AND '%s'
        AND ind_id = %s
        AND ind_group_cd = '%s'
    ;",value, start, end, kpi, group)
    sql
}

kpiLastDate <- function(kpi, group, value){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT
        FORMAT( MAX( dt), 'dd MMM, yyyy') AS ind_value
    FROM
        kpi.ind_data_detail
    WHERE
        period_cd = 'day'
        AND ind_id = %s
        AND ind_group_cd = '%s'
    ;", kpi, group)
    sql
}


kpiLastCycleAverage <- function(kpi, start, end, group, value){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- sprintf("
    WITH currentDate AS (
        SELECT
            MAX( _eff_start_dt) AS last_dt
        FROM
            [kpi].[ind_data]
        WHERE
            dt BETWEEN '%s' AND '%s'
            AND ind_id = %s
    )
    SELECT
        AVG( ind_value_%s) AS ind_value
    FROM
        currentDate,
        kpi.[ind_data_base]
    WHERE
        dt BETWEEN '%s' AND '%s'
        AND ind_id = %s
        AND ind_group_cd = '%s'
        AND _eff_start_dt <> last_dt
    ;",start, end, kpi,value, start, end, kpi, group)
    sql
}

kpiLastCycleDate <- function(kpi, group, value){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- sprintf("
    WITH currentDate AS (
        SELECT
            MAX( _eff_start_dt) AS last_dt
        FROM
            [kpi].[ind_data]
        WHERE
            ind_id = %s
    )
    SELECT
        FORMAT( MAX( dt), 'dd MMM, yyyy') AS ind_value
    FROM
        currentDate,
        kpi.[ind_data_base]
    WHERE
        ind_id = %s
        AND ind_group_cd = '%s'
        AND _eff_start_dt <> last_dt
    ;", kpi, kpi, group)
    sql
}
