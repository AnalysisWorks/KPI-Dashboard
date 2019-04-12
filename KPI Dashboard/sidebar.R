require(shiny)
require(shinydashboard)
require(dplyr)

getSidebar <- function(){
    sidebar <- dashboardSidebar(
        getSidebarMenu(),

        radioButtons( 
        "server",
        label   = "Client",
        choices = c(    
                "William Osler" = "wohs", 
                "Vancouver Coastal" = "vancouvercoastal",
                "Vancouver Island Health" = "islandhealth",
                "Alberta Health" = "AHSEdmontonZone"
                )
        ),
        dateRangeInput(
            "date_range", 
            "Date range:",
            start = "2018-04-01",
            end   = "2019-03-14"
        ),
        #button to close window and end the session
        tags$button(
            id = 'close',
            type = "button",
            class = "btn action-button",
            onclick = "setTimeout(function(){window.close();},500);", 
            "Close window"
        ),

        numericInput("KPI", "Current KPI", "2", min = 1, max = 10000, step = 1, width = NULL),
        textOutput("parentKPI"),
        textOutput("childKPI"),
        uiOutput("childKPIs")
    )
    sidebar
}

# Tab Name must correspond to a tab Item in body.R
getSidebarMenu <- function(){
    siderbarMenuItems <- sidebarMenu(
        menuItem(
            "Anomaly Detection", 
            tabName = "anomaly_detection", 
            icon = icon("dashboard")
        ),
        menuItem(
            "Period Comparison", 
            tabName = "period_comparison_table", 
            icon = icon("th")
        ),
        menuItem(
            "Period Trend Comparison", 
            tabName = "period_comparison_trend", 
            icon = icon("th")
        ),
        menuItem(
            "Period Aggregates", 
            tabName = "period_comparison_interval_trend", 
            icon = icon("th")
        )
    )
    siderbarMenuItems
}

