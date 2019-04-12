
require(RODBC)
require(tibble)
library(readr)
source( "PatientFlow\\patientFlow.R")
source( "DataManipulation\\data_conversion.R")
source( "DataManipulation\\data_type_checks.R")

$demoFiles = "lung_transplants.csv"
$extract_path = "C:\Users\zwarnes\Documents\ZW_AW\LightHouseAnalysis\Patient Flow"
$plotData = $true # test_app
$file_delimiter = "\t"




lungTransplants <- function(){
    sql <- "
    WITH lung_transplants AS (
        SELECT 
            DISTINCT L.pat_id, visit_occ
        FROM 
            dbo.bed_util L
            LEFT JOIN dbo.std_dad_int R
                ON L.visit_num = R.visit_num
            INNER JOIN dbo.bed_visit V
                ON L.visit_num = V.visit_num
        WHERE 
            dt BETWEEN '2017-04-01' AND '2018-03-31'
            AND ( R.int_cd LIKE '1GT85%' OR R.int_cd LIKE '1HY85%')
    )
    SELECT 
        L.nu_cd + '-' + CAST( (v.visit_occ - R.visit_occ) AS NVARCHAR) AS id, -- visit since transplant
        L.nu_cd + '-' + CAST( (v.visit_occ - R.visit_occ) AS NVARCHAR) AS label,
        L.nu_cd + '-' + CAST( (v.visit_occ - R.visit_occ) AS NVARCHAR) AS to_start,
        ISNULL(T.nu_cd  + '-' + CAST( (v.visit_occ - R.visit_occ) AS NVARCHAR) , 'Discharged') AS to_end,
        COUNT(*) AS [cases]
    FROM 
        [dbo].[bed_util] L
        INNER JOIN dbo.bed_visit V
            ON L.visit_num = V.visit_num
        INNER JOIN lung_transplants R
            ON L.pat_id = R.pat_id
            AND V.visit_occ >= R.visit_occ	--visits happening after the transplant
        LEFT JOIN bed_util T				-- join to next nursing unit
            ON L.visit_num = T.visit_num
            AND L.dt = DATEADD( day, -1, T.dt)
    GROUP BY 
        L.nu_cd + '-' + CAST( (v.visit_occ - R.visit_occ) AS NVARCHAR),
        ISNULL(T.nu_cd  + '-' + CAST( (v.visit_occ - R.visit_occ) AS NVARCHAR) , 'Discharged')  
    ;"    
    sql
}


transferFlow <- function(){
    sql <- "
    SELECT 
        -- Site Level
        L.site_cd AS site_id,
        L.site_cd AS site_label,
        L.site_cd AS site_to_start,
        ISNULL(T.site_cd, ISNULL( IIF( D.inst_to_cd NOT IN ('VGH', 'RH',' UBC','PHC', 'GFS', 'LGH'), 'Other', D.inst_to_cd), 'Discharged')) AS site_to_end,
        COUNT(*) AS [site_cases],
        
        --NU Program Level
        L.nu_program_cd AS nu_program_id,
        L.nu_program_cd AS nu_program_label,
        L.nu_program_cd AS nu_program_to_start,
        ISNULL(T.nu_program_cd, ISNULL( IIF( D.inst_to_cd NOT IN ('VGH', 'RH',' UBC','PHC', 'GFS', 'LGH'), 'Other', D.inst_to_cd), 'Discharged')) AS nu_program_to_end,
        COUNT(*) AS [nu_program_cases],

        --NU Level
        L.nu_cd AS nu_id,
        L.nu_cd AS nu_label,
        L.nu_cd AS nu_to_start,
        ISNULL(T.nu_cd,  ISNULL( IIF( D.inst_to_cd NOT IN ('VGH', 'RH',' UBC','PHC', 'GFS', 'LGH'), 'Other', D.inst_to_cd), 'Discharged')) AS nu_to_end,
        COUNT(*) AS [nu_cases]
    FROM 
        [dbo].[bed_util] L
        INNER JOIN dbo.bed_dad D
            ON L.visit_num = D.visit_num
            ANd L.authority_cd = D.authority_cd
        INNER JOIN dbo.bed_visit V
            ON L.visit_num = V.visit_num
            ANd L.authority_cd = v.authority_cd
        LEFT JOIN bed_util T				
            ON L.visit_num = T.visit_num
            ANd L.authority_cd = T.authority_cd
            AND L.dt = DATEADD( day, -1, T.dt)
    WHERE
        L.authority_cd = 'VCH'
        AND L.dt BETWEEN '2018-08-01' AND '2018-10-31'
    GROUP BY 
        L.site_cd,
        T.site_cd,
        IIF( D.inst_to_cd NOT IN ('VGH', 'RH',' UBC','PHC', 'GFS', 'LGH'), 'Other', D.inst_to_cd),
        L.nu_program_cd,
        T.nu_program_cd,
        L.nu_cd,
        T.nu_cd;
    "
    sql
}


patientFlow <- function(cs, sql){
    kpiTable <- sqlQuery( cs, sql)
    odbcClose(cs)
    kpiTable <- as_data_frame(kpiTable)
    kpiTable
}

#sql <- lungTransplants()

#cs <- connectionString( Server = "aworks300\\vancouvercoastal", Database = "LH_Inpatient")
#df <- patientFlow(cs, sql)
site_df <- as.data.frame(read_tsv( 'C:\\Users\\zwarnes\\Documents\\ZW_AW\\LightHouseAnalysis\\Patient Flow\\site_patient_flow.csv'))
site_df <- determineDataTypes( site_df)$data

program_df <- as.data.frame(read_tsv( 'C:\\Users\\zwarnes\\Documents\\ZW_AW\\LightHouseAnalysis\\Patient Flow\\program_patient_flow.csv'))
program_df <- determineDataTypes( program_df)$data

nu_df <- as.data.frame(read_tsv( 'C:\\Users\\zwarnes\\Documents\\ZW_AW\\LightHouseAnalysis\\Patient Flow\\nu_patient_flow.csv'))
nu_df <- determineDataTypes( nu_df)$data

options(shiny.host = "0.0.0.0")
options(shiny.port = 5150)

test_App( site_df, program_df, nu_df)