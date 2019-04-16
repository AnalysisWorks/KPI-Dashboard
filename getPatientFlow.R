
require(RODBC)
require(tibble)
library(readr)
source("PatientFlow\\patientFlow.R")
source("DataManipulation\\data_conversion.R")
source("DataManipulation\\data_type_checks.R")

#$demoFiles = "lung_transplants.csv"
#$extract_path = "C:\Users\zwarnes\Documents\ZW_AW\LightHouseAnalysis\Patient Flow"
#$plotData =  # test_app
#$file_delimiter = "\t"


patientFlow <- function(cs, sql) {
  kpiTable <- sqlQuery(cs, sql)
  odbcClose(cs)
  kpiTable <- as_data_frame(kpiTable)
  kpiTable
}

#sql <- lungTransplants()

#cs <- connectionString( Server = "aworks300\\vancouvercoastal", Database = "LH_Inpatient")
#df <- patientFlow(cs, sql)
site_df <- as.data.frame(read_tsv('C:\\Users\\zwarnes\\Documents\\ZW_AW\\LightHouseAnalysis\\Patient Flow\\site_patient_flow.csv'))
site_df <- determineDataTypes(site_df)$data

program_df <- as.data.frame(read_tsv('C:\\Users\\zwarnes\\Documents\\ZW_AW\\LightHouseAnalysis\\Patient Flow\\program_patient_flow.csv'))
program_df <- determineDataTypes(program_df)$data

nu_df <- as.data.frame(read_tsv('C:\\Users\\zwarnes\\Documents\\ZW_AW\\LightHouseAnalysis\\Patient Flow\\nu_patient_flow.csv'))
nu_df <- determineDataTypes(nu_df)$data

options(shiny.host = "0.0.0.0")
options(shiny.port = 5150)

test_App(site_df, program_df, nu_df)