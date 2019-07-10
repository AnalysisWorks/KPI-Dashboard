# Train and test data, testing queries
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
