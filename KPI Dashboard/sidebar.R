require(shiny)
require(shinydashboard)
require(dplyr)

getSidebar <- function() {
  sidebar <- dashboardSidebar(
        selectizeInput(
            "server",
            label = "Current Server",
            choices = c(
                    "Vancouver Coastal" = "vancouvercoastal",
                    "Island Health" = "islandhealth",
                    "William Osler" = "wohs",
                    "Alberta Health" = "AHSEdmontonZone"
                    ),
            options = list(
                placeholder = 'Client'
            )
        ),

        dateRangeInput(
            "date_range",
            label = "Select Date Range",
            start = "2018-04-01",
            end = "2019-03-31"
        ),

        uiOutput("KPIGroups"),
        uiOutput("KPIValues"),
        uiOutput("KPIout"),
        uiOutput("KPIfields"),

        selectizeInput(
            "chartTabs",
            label = "Select Desired View",
            choices = c(
                    "Statistical Outliers" = "anomaly_detection",
                    "Trend with Previous Year" = "period_comparison_trend",
                    "Trend with Previous Cycle" = "period_comparison_interval_trend"
                    ),
            options = list(
                placeholder = 'View'
            )
        ),
        radioButtons(
            "metric", "Generate Tables (May take a few minutes)",
            c(
                "Statistical Anomalies" = "anomalies",
                "Cycle to Cycle Difference" = "differences",
                "Year over Year Difference" = "periodComp"
            )
        ),
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


