require(shiny)
require(shinydashboard)
require(dplyr)

getSidebar <- function() {
  sidebar <- dashboardSidebar(
        getSidebarMenu(),
        selectizeInput(
            "server",
            label = "Target Server",
            choices = c(
                    "Vancouver Island Health" = "islandhealth",
                    "William Osler" = "wohs",
                    "Vancouver Coastal" = "vancouvercoastal",
                    "Alberta Health" = "AHSEdmontonZone"
                    ),
            options = list(
                placeholder = 'Please Select a Server'
            )
        ),
        dateRangeInput(
            "date_range",
            "Date range:",
            start = "2018-04-01",
            end = "2019-03-14"
        ),
        uiOutput("KPIGroups"),
        uiOutput("KPIValues"),
        uiOutput("KPI"),
        uiOutput("childKPIs"),
        actionButton("anomalies", "Calculate Anomalies"),
        textOutput("parentKPI"),
        textOutput("childKPI"),
        #button to close window and end the session
        tags$button(
            id = 'close',
            type = "button",
            class = "btn action-button",
            onclick = "setTimeout(function(){window.close();},500);",
            "Close window"
        )
    )
  sidebar
}

# Tab Name must correspond to a tab Item in body.R
getSidebarMenu <- function() {
  siderbarMenuItems <- sidebarMenu(
        menuItem(
            "View", icon = icon("dashboard"),
            menuSubItem(
                "Anomaly Detection",
                tabName = "anomaly_detection",
                icon = icon("dashboard")
            ),
            menuSubItem(
                "Period Trend Comparison",
                tabName = "period_comparison_trend",
                icon = icon("th")
            ),
            menuSubItem(
                "Period Aggregates",
                tabName = "period_comparison_interval_trend",
                icon = icon("th")
            )
        )        
    )
  siderbarMenuItems
}

