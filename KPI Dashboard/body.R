require(shiny)
require(shinydashboard)
require(dplyr)

getBody <- function() {
  body <- dashboardBody(
        getBoxs(),
        plotlyOutput('plot', height = "400px"),
        getTableTabs()
    )
  body
}


getBoxs <- function(){
    boxes <- fluidRow(
            valueBoxOutput("currentLastAssemble"),
            valueBoxOutput("currentAverageBox")
        )
}

getTableTabs <- function(){
    tableItems <- tabItem(
                    tabName = "table_comparison_tab",
                    tabBox(
                        title = "KPI Analysis",
                        id = "stat_table",
                        width = "100%",
                        tabPanel(
                            "Selected Comparison",
                            DTOutput("problemKPIs")
                        )
                    )
        )
}

