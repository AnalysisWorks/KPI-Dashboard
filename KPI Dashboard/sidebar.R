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
                "Vancouver Island Health" = "islandhealth"
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
            tabName = "period_comparison", 
            icon = icon("th")
        ),
        menuItem(
            "Quality Assurance", 
            tabName = "quality_assurance", 
            icon = icon("th")
        )
    )
    siderbarMenuItems
}

