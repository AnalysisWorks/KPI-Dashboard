kpiQuery <- function(kpi, start, end){
    sql <- sprintf("
    SELECT 
        dt AS [Date], ind_value_1, ind_id
    FROM 
        LH_Indicators.kpi.ind_data_detail
    WHERE 
        period_cd = 'day'
        AND dt BETWEEN '%s' AND '%s'
        AND ind_id = %s
        AND ind_group_cd = 'Beds'
    ORDER BY
        dt,
        ind_id
    
    ;", start, end, kpi) 
    sql
}
kpiMapping <- function(){
    sql <- "
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
        L.ind_group_cd = 'Beds'
        AND L.period_cd = 'day'
    ;"    
    sql
}
kpiPeriodComparisionTable <- function(){
    sql <- "        
    WITH last_date AS (
        SELECT 
			1 record_order, 
			L.ind_id,
			MAX(L._eff_start_dt) AS last_assembly
        FROM 
            LH_Indicators.kpi.ind_data_base L
            LEFT JOIN kpi.ind_type T
                ON L.ind_id = T.ind_id
                AND L.ind_group_cd = T.ind_group_cd
        WHERE
            T.period_cd = 'fiscal'
            AND T.ind_group_cd = 'Beds'
		GROUP BY	
			L.ind_id
    )
	, comparative_date AS (
        SELECT 
			2 as record_order,
			L.ind_id,
			MAX(L._eff_start_dt) AS comparative_date
        FROM 
            LH_Indicators.kpi.ind_data_base L
            LEFT JOIN kpi.ind_type T
                ON L.ind_id = T.ind_id
                AND L.ind_group_cd = T.ind_group_cd
			INNER JOIN last_date D
				ON L.ind_id = D.ind_id
        WHERE
            T.period_cd = 'fiscal'
            AND T.ind_group_cd = 'Beds'
			AND L._eff_start_dt <> D.last_assembly
		GROUP BY	
			L.ind_id
    )
    ,ordered AS (
        SELECT 
            L.dt, 
            L.ind_value_1, 
            L.ind_id,
			L._eff_start_dt,
			R.last_assembly,
			D.comparative_date
        FROM 
            LH_Indicators.kpi.ind_data_base L
            INNER JOIN kpi.ind_type T
                ON L.ind_id = T.ind_id
                AND L.ind_group_cd = T.ind_group_cd
            LEFT JOIN last_date R
                ON L.ind_id = R.ind_id
			LEFT JOIN comparative_date D
				ON L.ind_id = D.ind_id

        WHERE 
            T.period_cd = 'fiscal'
            AND L.ind_group_cd = 'Beds'
            AND T.ind_group_cd = 'Beds'
    )
    SELECT
        L.ind_id,
        L.dt,
        L.ind_value_1 AS current_period,
        R1.ind_value_1 AS previous_period_1,
        (L.ind_value_1 - R1.ind_value_1) / L.ind_value_1 AS percent_change
    FROM
        ordered L
       LEFT JOIN ordered R1
           ON L.ind_id = R1.ind_id
           AND L.dt = R1.dt
           AND L.comparative_date = R1.comparative_date
   WHERE
       L._eff_start_dt = L.last_assembly
	   AND R1._eff_start_dt = L.comparative_date
   ORDER BY 
       L.ind_id, 
       L.dt 
    ;"
}


kpiPeriodComparisionTrend <- function(kpi, start, end){
    sql <- sprintf("
    SELECT 
        L.dt AS [Date], 
		L.ind_value_1 AS series_1, 
		ISNULL( R.ind_value_1 , 0 ) AS series_2, 
		L.ind_id
    FROM 
        LH_Indicators.kpi.ind_data_detail L
		LEFT JOIN LH_Indicators.kpi.ind_data_detail R
			ON L.ind_id = R.ind_id
            AND L.ind_group_cd = R.ind_group_cd
			AND L.dt = DATEADD( yy, 1, R.dt)
    WHERE 
        L.period_cd = 'day'
        AND R.period_cd = 'day'
        AND L.ind_group_cd = 'Beds'
        AND R.ind_group_cd = 'Beds'
        AND L.dt BETWEEN '%s' AND '%s'
		AND L.ind_id = %s
    ORDER BY
        L.ind_id,
		L.dt
    ;", start, end, kpi)
}

kpiDayIndicators <- function(){
    sql <- "
    SELECT
        DISTINCT
        ind_id
    FROM 
        LH_Indicators.kpi.ind_data_detail
    WHERE 
        period_cd = 'day'
        AND ind_group_cd = 'Beds'
    ;"
}

kpiTable <- function(cs, sql){
    kpiTable <- sqlQuery( cs, sql)
    odbcClose(cs)
    kpiTable[,1] <- as.Date( as.character(kpiTable[,1]), form = "%Y-%m-%d")
    kpitable <- as_data_frame(kpiTable)
    kpitable
}

kpiMap <- function(cs, sql){
    kpiTable <- sqlQuery( cs, sql)
    odbcClose(cs)
    kpitable <- as_data_frame(kpiTable)
    kpitable
}

getKPIs <- function(cs, sql){
    kpiTable <- sqlQuery( cs, sql)
    odbcClose(cs)
    kpitable <- as.data.frame(kpiTable)
    kpitable
}