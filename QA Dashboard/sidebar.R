require(shiny)
require(shinydashboard)
require(dplyr)

getSidebar <- function() {
  sidebar <- dashboardSidebar(
        getSidebarMenu(),
        uiOutput('test'),
        dateRangeInput(
            "date_range",
            "Date range:",
            start = "2010-01-01",
            end = "2020-01-01"
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
getSidebarMenu <- function() {
  siderbarMenuItems <- sidebarMenu(
        menuItem(
            "View", icon = icon("dashboard"),
            menuSubItem(
                "File Selection",
                tabName = "import_extracts",
                icon = icon("dashboard")
            )
        ),menuItem(
            "View", icon = icon("dashboard"),
            menuSubItem(
                "Analyze Extract",
                tabName = "quality_assurance",
                icon = icon("dashboard")
            )
        )
    )
  siderbarMenuItems
}

