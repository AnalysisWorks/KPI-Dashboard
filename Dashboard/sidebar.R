require(shiny)
require(shinydashboard)
require(dplyr)

getSidebar <- function(){
    sidebar <- dashboardSidebar(
                    getSidebarMenu()
                )
    sidebar
}

# Tab Name must correspond to a tab Item in body.R
getSidebarMenu <- function(){
    siderbarMenuItems <- sidebarMenu(
        menuItem("Anomaly Detection", tabName = "anomaly_detection", icon = icon("dashboard")),
        menuItem("Period Comparison", tabName = "period_comparison", icon = icon("th")),
        menuItem("Quality Assurance", tabName = "quality_assurance", icon = icon("th"))
    )
    siderbarMenuItems
}

# Recieve test input
getSidebarSearchForm <- function(){
    search <- sidebarSearchForm(
        textId = "searchText", 
        buttonId = "searchButton",
        label = "Search..."
    )
    search
}