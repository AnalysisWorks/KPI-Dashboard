kpiQuery <- function(){
    sql <- "
    SELECT 
        dt AS [Date], ind_value_1, ind_id
    FROM 
        LH_Indicators.kpi.ind_data_detail
    WHERE 
        period_cd = 'day'
        AND dt BETWEEN '2016-01-01' AND '2018-12-31'
    ORDER BY
        dt,
        ind_id
    
    ;"    
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

#sql <- kpiQuery()
#cs <- connectionString( Server = "aworks300\\wohs", Database = "LH_Indicators")
#df <- kpiTable(cs, sql)


#sql <- kpiMapping()
#cs <- connectionString( Server = "aworks300\\wohs", Database = "LH_Indicators")
#dfMap <- kpiMap(cs, sql)