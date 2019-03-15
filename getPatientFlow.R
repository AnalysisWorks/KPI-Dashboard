
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
            dt BETWEEN '2014-04-01' AND '2019-03-31'
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



patientFlow <- function(cs, sql){
    kpiTable <- sqlQuery( cs, sql)
    odbcClose(cs)
    kpiTable <- as_data_frame(kpiTable)
    kpiTable
}


#sql <- lungTransplants()
#cs <- connectionString( Server = "aworks300\\vancouvercoastal", Database = "LH_Inpatient")
#df <- patientFlow(cs, sql)

df <- as.data.frame(read_tsv( 'C:\\Users\\zwarnes\\Documents\\ZW_AW\\LightHouseAnalysis\\Patient Flow\\lung_transplants.csv'))
df <- determineDataTypes( df)$data

test_App(df)