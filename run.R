require(dplyr)
require(shiny)
require(shinydashboard)
require(RODBC)
require(tibble)
require(DT)
source("Database\\getConnectionString.R")
source("Database\\getKPIData.R")

app <- "KPI Dashboard"

source(paste(app, "header.R", sep = "\\"))
source(paste(app, "sidebar.R", sep = "\\"))
source(paste(app, "body.R", sep = "\\"))
source(paste(app, "dashboard.R", sep = "\\"))
source(paste(app, "Dashboard\\kpiValueUI.R", sep = "\\"))
source(paste(app, "Dashboard\\kpiGroupsUI.R", sep = "\\"))
source(paste(app, "Dashboard\\kpiUI.R", sep = "\\"))
source(paste(app, "Dashboard\\childKPIUI.R", sep = "\\"))
source(paste(app, "Dashboard\\mapping.R", sep = "\\"))
source(paste(app, "Dashboard\\getKPI.R", sep = "\\"))
source(paste(app, "Dashboard\\valueBoxes.R", sep = "\\"))
source(paste(app, "Charting\\labels.R", sep = "\\"))
source(paste(app, "Charting\\trend.R", sep = "\\"))
source(paste(app, "Charting\\chartData.R", sep = "\\"))
source(paste(app, "Table\\tableData.R", sep = "\\"))
source(paste(app, "Email\\htmlUtil.R", sep = "\\"))
source(paste(app, "prediction.R", sep = "\\"))

# Establish network conditions
options(shiny.host = "0.0.0.0")
options(shiny.port = 5050)

dashboard(TRUE)


