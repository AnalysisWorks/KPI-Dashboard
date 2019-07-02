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
                    "Island Health" = "islandhealth",
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
            start = "2019-01-01",
            end = "2019-05-15"
        ),
        sliderInput(
            "target_perc",
            "Percentile Target",
            min = 0,
            max = 100,
            step = 1,
            value = 75
        ),
        uiOutput("KPIGroups"),
        uiOutput("KPIValues"),
        uiOutput("KPIout"),
        uiOutput("childKPIs"),
        
        actionButton("parent", "Select Parent"),
        actionButton("child", "Select Child"),
        actionButton("anomalies", "Calculate Anomalies"),
        actionButton("differences", "Calculate Differences"),
        actionButton("periodComp", "Period Differences"),
        actionButton("email", "Email Findings"),
        
        bookmarkButton(),
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
        id = 'chartTabs',
        menuItem(
            "Charting", icon = icon("dashboard"),
            menuSubItem(
                "Statistical Outliers",
                tabName = "anomaly_detection",
                icon = icon("dashboard")
            ),
            menuSubItem(
                "Trend with Previous Year",
                tabName = "period_comparison_trend",
                icon = icon("th")
            ),
            menuSubItem(
                "Trend with Previous Cycle",
                tabName = "period_comparison_interval_trend",
                icon = icon("th")
            ),
            menuSubItem(
                "Predict from Previous KPIs",
                tabName = "prediction_anomalies",
                icon = icon("th")
            )
        )
    )
  siderbarMenuItems
}

