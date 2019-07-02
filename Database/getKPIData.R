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
        LH_Indicators.kpi.ind_data_detail
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
        LH_Indicators.kpi.ind_data_detail
    WHERE 
        period_cd = 'day'
        AND dt BETWEEN '%s' AND '%s'
        AND ind_group_cd = '%s'
        AND ( group_field_3 <> 'service_cd' OR group_field_3 = 'nu_cd' OR group_field_3 IS NULL )
    ORDER BY
        ind_id,
        dt
    
    ;",value, start, end, group) 
    sql
}
kpiKeyMapping <- function(group){
    if(is.null(group)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT 
        L.ind_id AS kpi,
        L.group_field_1,
        L.group_value_1,
        L.group_field_2,
        L.group_value_2,
        L.group_field_3,
        L.group_value_3,
        L.group_field_4,
        L.group_value_4,

        C.ind_id AS child_kpi,
        C.group_field_1 AS child_group_field_1,
        C.group_value_1 AS child_group_value_1,
        C.group_field_2 AS child_group_field_2,
        C.group_value_2 AS child_group_value_2,
        C.group_field_3 AS child_group_field_3,
        C.group_value_3 AS child_group_value_3,
        C.group_field_4 AS child_group_field_4,
        C.group_value_4 AS child_group_value_4,

        P.ind_id AS parent_kpi,
        P.group_field_1 AS parent_group_field_1,
        P.group_value_1 AS parent_group_value_1,
        P.group_field_2 AS parent_group_field_2,
        P.group_value_2 AS parent_group_value_2,
        P.group_field_3 AS parent_group_field_3,
        P.group_value_3 AS parent_group_value_3,
        P.group_field_4 AS parent_group_field_4,
        P.group_value_4 AS parent_group_value_4
    FROM 
        [LH_Indicators].[kpi].[ind_type] L
        LEFT JOIN [LH_Indicators].[kpi].[ind_type] P
            ON L.ind_group_cd = P.ind_group_cd
            AND P.period_cd = L.period_cd
            AND (
                (	
                    P.group_field_1 IS NULL
                    AND L.group_field_1 IS NOT NULL 
                    AND L.group_field_2 IS NULL
                    AND L.group_field_3 IS NULL
                    AND L.group_field_4 IS NULL
                )
                OR (	
                    L.group_field_1 = P.group_field_1 
                    AND L.group_value_1 = P.group_value_1 
                    AND L.group_field_2 IS NOT NULL 
                    AND L.group_field_3 IS NULL
                    AND L.group_field_4 IS NULL
                    AND P.group_field_2 IS NULL
                )
                OR ( 
                    L.group_field_1 = P.group_field_1 
                    AND L.group_value_1 = P.group_value_1 
                    AND L.group_field_2 = P.group_field_2 
                    AND L.group_value_2 = P.group_value_2 
                    AND L.group_field_3 IS NOT NULL 
                    AND L.group_field_4 IS NULL
                    AND P.group_field_3 IS NULL
                    )
                OR ( 
                    L.group_field_1 = P.group_field_1 
                    AND L.group_value_1 = P.group_value_1 
                    AND L.group_field_2 = P.group_field_2 
                    AND L.group_value_2 = P.group_value_2 
                    AND L.group_field_3 = P.group_field_3 
                    AND L.group_value_3 = P.group_value_3 
                    AND L.group_field_4 IS NOT NULL 
                    AND P.group_field_4 IS NULL
                    )
                )
        LEFT JOIN [LH_Indicators].[kpi].[ind_type] C
            ON L.ind_group_cd = C.ind_group_cd
            AND C.period_cd = L.period_cd
            AND (
                (	
                    L.group_field_1 IS NULL
                    AND C.group_field_1 IS NOT NULL
                    AND C.group_field_2 IS NULL
                    AND C.group_field_3 IS NULL
                    AND C.group_field_4 IS NULL
                )
                OR (	
                    L.group_field_1 = C.group_field_1 
                    AND L.group_value_1 = C.group_value_1 
                    AND L.group_field_2 IS NULL 
                    AND C.group_field_2 IS NOT NULL
                    AND C.group_field_3 IS NULL
                    AND C.group_field_4 IS NULL
                    )
                OR ( 
                    L.group_field_1 = C.group_field_1 
                    AND L.group_value_1 = C.group_value_1 
                    AND L.group_field_2 = C.group_field_2 
                    AND L.group_value_2 = C.group_value_2 
                    AND L.group_field_3 IS NULL 
                    AND C.group_field_3 IS NOT NULL
                    AND C.group_field_4 IS NULL
                    )
                OR ( 
                    L.group_field_1 = C.group_field_1 
                    AND L.group_value_1 = C.group_value_1 
                    AND L.group_field_2 = C.group_field_2 
                    AND L.group_value_2 = C.group_value_2 
                    AND L.group_field_3 = C.group_field_3 
                    AND L.group_value_3 = C.group_value_3 
                    AND L.group_field_4 IS NULL 
                    AND C.group_field_4 IS NOT NULL
                    )
            )
    WHERE 
        L.ind_group_cd = '%s'
        AND L.period_cd = 'day'
        AND ( L.group_field_3 <> 'service_cd' OR L.group_field_3 = 'nu_cd' OR L.group_field_3 IS NULL )
    ;", group)  
    sql
}



kpiMapping <- function(group){
    if(is.null(group)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT 
        L.ind_id AS kpi,
        L.group_field_1,
        L.group_value_1,
        L.group_field_2,
        L.group_value_2,
        L.group_field_3,
        L.group_value_3,
        L.group_field_4,
        L.group_value_4,

        C.ind_id AS child_kpi,
        C.group_field_1 AS child_group_field_1,
        C.group_value_1 AS child_group_value_1,
        C.group_field_2 AS child_group_field_2,
        C.group_value_2 AS child_group_value_2,
        C.group_field_3 AS child_group_field_3,
        C.group_value_3 AS child_group_value_3,
        C.group_field_4 AS child_group_field_4,
        C.group_value_4 AS child_group_value_4,

        P.ind_id AS parent_kpi,
        P.group_field_1 AS parent_group_field_1,
        P.group_value_1 AS parent_group_value_1,
        P.group_field_2 AS parent_group_field_2,
        P.group_value_2 AS parent_group_value_2,
        P.group_field_3 AS parent_group_field_3,
        P.group_value_3 AS parent_group_value_3,
        P.group_field_4 AS parent_group_field_4,
        P.group_value_4 AS parent_group_value_4
    FROM 
        [LH_Indicators].[kpi].[ind_type] L
        LEFT JOIN [LH_Indicators].[kpi].[ind_type] P
            ON L.ind_group_cd = P.ind_group_cd
            AND P.period_cd = L.period_cd
            AND (
                (	
                    P.group_field_1 IS NULL
                    AND L.group_field_1 IS NOT NULL 
                    AND L.group_field_2 IS NULL
                    AND L.group_field_3 IS NULL
                    AND L.group_field_4 IS NULL
                )
                OR (	
                    L.group_field_1 = P.group_field_1 
                    AND L.group_value_1 = P.group_value_1 
                    AND L.group_field_2 IS NOT NULL 
                    AND L.group_field_3 IS NULL
                    AND L.group_field_4 IS NULL
                    AND P.group_field_2 IS NULL
                )
                OR ( 
                    L.group_field_1 = P.group_field_1 
                    AND L.group_value_1 = P.group_value_1 
                    AND L.group_field_2 = P.group_field_2 
                    AND L.group_value_2 = P.group_value_2 
                    AND L.group_field_3 IS NOT NULL 
                    AND L.group_field_4 IS NULL
                    AND P.group_field_3 IS NULL
                    )
                OR ( 
                    L.group_field_1 = P.group_field_1 
                    AND L.group_value_1 = P.group_value_1 
                    AND L.group_field_2 = P.group_field_2 
                    AND L.group_value_2 = P.group_value_2 
                    AND L.group_field_3 = P.group_field_3 
                    AND L.group_value_3 = P.group_value_3 
                    AND L.group_field_4 IS NOT NULL 
                    AND P.group_field_4 IS NULL
                    )
                )
        LEFT JOIN [LH_Indicators].[kpi].[ind_type] C
            ON L.ind_group_cd = C.ind_group_cd
            AND C.period_cd = L.period_cd
            AND (
                (
                    L.group_field_1 IS NULL
                    AND C.group_field_1 IS NOT NULL
                    AND C.group_field_2 IS NULL
                    AND C.group_field_3 IS NULL
                    AND C.group_field_4 IS NULL
                )
                OR (	
                    L.group_field_1 = C.group_field_1 
                    AND L.group_value_1 = C.group_value_1 
                    AND L.group_field_2 IS NULL 
                    AND C.group_field_2 IS NOT NULL
                    AND C.group_field_3 IS NULL
                    AND C.group_field_4 IS NULL
                    )
                OR ( 
                    L.group_field_1 = C.group_field_1 
                    AND L.group_value_1 = C.group_value_1 
                    AND L.group_field_2 = C.group_field_2 
                    AND L.group_value_2 = C.group_value_2 
                    AND L.group_field_3 IS NULL 
                    AND C.group_field_3 IS NOT NULL
                    AND C.group_field_4 IS NULL
                    )
                OR ( 
                    L.group_field_1 = C.group_field_1 
                    AND L.group_value_1 = C.group_value_1 
                    AND L.group_field_2 = C.group_field_2 
                    AND L.group_value_2 = C.group_value_2 
                    AND L.group_field_3 = C.group_field_3 
                    AND L.group_value_3 = C.group_value_3 
                    AND L.group_field_4 IS NULL 
                    AND C.group_field_4 IS NOT NULL
                    )
            )
    WHERE 
        L.ind_group_cd = '%s'
        AND L.period_cd = 'day'
    ;", group)  
    sql
}

kpiPeriodComparision <- function(kpi, start, end, group, value){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- sprintf("
    WITH last_date AS (
        SELECT 
            1 record_order, 
            MAX(L._eff_start_dt) AS last_assembly
        FROM 
            LH_Indicators.kpi.ind_data_base L
            LEFT JOIN kpi.ind_type T
                ON L.ind_id = T.ind_id
                AND L.ind_group_cd = T.ind_group_cd
        WHERE
            T.period_cd = 'day'
            AND T.ind_group_cd = '%s'
    )
    , comparative_date AS (
        SELECT 
            2 as record_order,
            MAX(L._eff_start_dt) AS comparative_date
        FROM 
            LH_Indicators.kpi.ind_data_base L
            LEFT JOIN kpi.ind_type T
                ON L.ind_id = T.ind_id
                AND L.ind_group_cd = T.ind_group_cd
            ,last_date D
        WHERE
            T.period_cd = 'day'
            AND L.ind_id = %s
            AND T.ind_group_cd = '%s'
            AND L._eff_start_dt <> D.last_assembly
    )
    ,filtered_kpis AS (
        SELECT 
            L.dt, 
            L.ind_value_%s, 
            L.ind_id,
            L._eff_start_dt,
            IIF( L._eff_start_dt = R.last_assembly, 1, IIF( L._eff_start_dt = D.comparative_date, 2, 0)) AS [rank]
        FROM 
            LH_Indicators.kpi.ind_data_base L
            ,last_date R   
            ,comparative_date D
        WHERE 
            L.ind_id = %s
            AND L.ind_group_cd = '%s'
            AND L.dt BETWEEN '%s' AND '%s'
            AND (L._eff_start_dt = R.last_assembly
            OR L._eff_start_dt = D.comparative_date)
    )
    SELECT
        D.dt AS [Date],
        %s AS ind_id,
        SUM(IIF( [rank] = 1, L.ind_value_%s, 0)) AS series_1,
        SUM(IIF( [rank] = 2, L.ind_value_%s, 0)) AS series_2
    FROM
        LH_Common.dbo.ref_dt D
        LEFT JOIN filtered_kpis L
            ON D.dt = L.dt
    WHERE 
        D.dt BETWEEN '%s' AND '%s'
    GROUP BY 
        D.dt
    ORDER BY 
        D.dt  
    ;", group, kpi, group, value, kpi, group, start, end, kpi, value, value, start, end)
}


kpiPeriodComparisionTrend <- function(kpi, start, end, group, value){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT 
        L.dt AS [Date], 
        L.ind_value_%s AS series_1, 
        ISNULL( R.ind_value_%s , 0 ) AS series_2, 
        L.ind_id
    FROM 
        LH_Indicators.kpi.ind_data_detail L
        LEFT JOIN LH_Indicators.kpi.ind_data_detail R
            ON L.ind_id = R.ind_id
            AND L.ind_group_cd = R.ind_group_cd
            AND L.dt = DATEADD( yy, 1, R.dt)
    WHERE 
        L.period_cd = 'day'
        AND ( R.period_cd = 'day' OR R.period_cd IS NULL)
        AND L.ind_group_cd = '%s'
        AND ( R.ind_group_cd = '%s' OR R.ind_group_cd IS NULL)
        AND L.dt BETWEEN '%s' AND '%s'
        AND L.ind_id = %s
    ORDER BY
        L.ind_id,
        L.dt
    ;", value, value, group, group, start, end, kpi)
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
        LH_Indicators.kpi.ind_data_detail
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
        [LH_Indicators].[kpi].[ind_data_detail]
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
        [LH_Indicators].[kpi].[ind_data_detail]
    WHERE 
        ind_group_cd = '%s'
        AND period_cd = 'day'
    ;", group)
    sql
}

kpiTrainData <- function(kpi, start, end){
    sql <- sprintf("
    SELECT 
        L.[ind_value_1]
        ,CONVERT(DECIMAL(12,4), CAST( L.[dt] AS datetime)) AS [Date]
        ,CONVERT(DECIMAL(12,4), CAST( L.[_eff_start_dt] AS datetime)) AS _eff_start_dt
        ,IIF( R.group_field_1 IS NOT NULL AND R.group_field_2 IS NULL, 1, 0) AS site_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd', 1, 0) AS program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'MED', 1, 0) AS med_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'SRG', 1, 0) AS srg_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'MH', 1, 0) AS mh_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'EMER', 1, 0) AS emer_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'WH', 1, 0) AS wh_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'CRIT', 1, 0) AS crit_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd', 1, 0) AS nu_program_flag 
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'MED', 1, 0) AS med_nu_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'SRG', 1, 0) AS srg_nu_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'MH', 1, 0) AS mh_nu_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'EMER', 1, 0) AS emer_nu_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'WH', 1, 0) AS wh_nu_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'CRIT', 1, 0) AS crit_nu_program_flag
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd', 1, 0) AS service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'FAMP', 1, 0) AS famp_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'INT', 1, 0) AS int_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'GSUR', 1, 0) AS gsur_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'PSYC', 1, 0) AS psyc_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'CRIT', 1, 0) AS crit_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'HOSP', 1, 0) AS hosp_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'OBG', 1, 0) AS obg_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'EMER', 1, 0) AS emer_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'ORTH', 1, 0) AS orth_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'PAL', 1, 0) AS pal_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'ENDO', 1, 0) AS endo_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'PEDI', 1, 0) AS pedi_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'CARD', 1, 0) AS card_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'nu_cd', 1, 0) AS nu_flag 
    FROM 
        [LH_Indicators].[kpi].[ind_data_base] L
        LEFT JOIN [LH_Indicators].[kpi].ind_type R
            ON L.ind_id = R.ind_id
    WHERE 
        L.ind_id = %s
        AND L._eff_end_dt IS NOT NULL 	 
        AND L.dt BETWEEN '%s' AND '%s'
    ", kpi, start, end)
    sql
}

kpiTestData <- function(kpi, start, end){
    sql <- sprintf("
    SELECT 
        L.[ind_value_1]
        ,CONVERT(DECIMAL(12,4), CAST( L.[dt] AS datetime)) AS [Date]
        ,CONVERT(DECIMAL(12,4), CAST( L.[_eff_start_dt] AS datetime)) AS _eff_start_dt
        ,IIF( R.group_field_1 IS NOT NULL AND R.group_field_2 IS NULL, 1, 0) AS site_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd', 1, 0) AS program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'MED', 1, 0) AS med_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'SRG', 1, 0) AS srg_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'MH', 1, 0) AS mh_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'EMER', 1, 0) AS emer_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'WH', 1, 0) AS wh_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'program_cd' AND R.group_value_1 = 'CRIT', 1, 0) AS crit_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd', 1, 0) AS nu_program_flag 
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'MED', 1, 0) AS med_nu_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'SRG', 1, 0) AS srg_nu_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'MH', 1, 0) AS mh_nu_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'EMER', 1, 0) AS emer_nu_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'WH', 1, 0) AS wh_nu_program_flag
        ,IIF( R.group_field_2 IS NOT NULL AND R.group_field_3 IS NULL AND R.group_field_2 = 'nu_program_cd' AND R.group_value_1 = 'CRIT', 1, 0) AS crit_nu_program_flag
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd', 1, 0) AS service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'FAMP', 1, 0) AS famp_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'INT', 1, 0) AS int_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'GSUR', 1, 0) AS gsur_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'PSYC', 1, 0) AS psyc_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'CRIT', 1, 0) AS crit_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'HOSP', 1, 0) AS hosp_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'OBG', 1, 0) AS obg_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'EMER', 1, 0) AS emer_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'ORTH', 1, 0) AS orth_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'PAL', 1, 0) AS pal_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'ENDO', 1, 0) AS endo_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'PEDI', 1, 0) AS pedi_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'service_cd' AND R.group_value_3 = 'CARD', 1, 0) AS card_service_flag 
        ,IIF( R.group_field_3 IS NOT NULL AND R.group_field_4 IS NULL AND R.group_field_3 = 'nu_cd', 1, 0) AS nu_flag 
    FROM 
        [LH_Indicators].[kpi].[ind_data_base] L
        LEFT JOIN [LH_Indicators].[kpi].ind_type R
            ON L.ind_id = R.ind_id
    WHERE 
        L.ind_id = %s
        AND L._eff_end_dt IS NULL 
        AND L.dt BETWEEN '%s' AND '%s'
    ", kpi, start, end)	 
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

kpiPercentile <- function(kpi, start, end, group, value, perc){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- sprintf("
    SELECT 
        DISTINCT 
        PERCENTILE_CONT( %s) WITHIN GROUP( ORDER BY ind_value_%s)
        OVER() AS ind_value
    FROM 
        kpi.ind_data_detail
    WHERE 
        period_cd = 'day'
        AND dt BETWEEN '%s' AND '%s'
        AND ind_id = %s
        AND ind_group_cd = '%s'
    ;", perc, value, start, end, kpi, group) 
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
            [LH_Indicators].[kpi].[ind_data]
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
            [LH_Indicators].[kpi].[ind_data]
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

kpiLastCyclePercentile <- function(kpi, start, end, group, value, perc){
    if(is.null(group) || is.null(value)){
        return(NULL)
    }
    sql <- sprintf("
    WITH currentDate AS (
        SELECT 
            MAX( _eff_start_dt) AS last_dt
        FROM 
            [LH_Indicators].[kpi].[ind_data]
        WHERE 
            dt BETWEEN '%s' AND '%s'
            AND ind_id = %s
    )
    SELECT 
        DISTINCT 
        PERCENTILE_CONT( %s) WITHIN GROUP( ORDER BY ind_value_%s)
        OVER() AS ind_value
    FROM 
        currentDate,
        kpi.[ind_data_base]
    WHERE 
        dt BETWEEN '%s' AND '%s'
        AND ind_id = %s
        AND ind_group_cd = '%s'
        AND _eff_start_dt <> last_dt
    ;",start, end, kpi, perc, value, start, end, kpi, group) 
    sql
}