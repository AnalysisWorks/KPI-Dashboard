require(shiny)
require(shinydashboard)
require(dplyr)
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

# Establish network conditions
options(shiny.host = "0.0.0.0")
options(shiny.port = 5050)

dashboard(TRUE)


