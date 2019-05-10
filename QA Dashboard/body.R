require(shiny)
require(shinydashboard)
require(dplyr)

getBody <- function() {
  body <- dashboardBody(
        getTabItems()
    )
  body
}

getTabItems <- function() {
  tabItems <- tabItems(
        tabItem(
            tabName = "import_extracts",
            tabBox(
                title = "File Import",
                id = "file_imports",
                width = "100%",
                tabPanel(
                    "Importing Extracts",
                    textOutput("Testing")
                )
            )
        ),
        tabItem(
            tabName = "quality_assurance",
            tabBox(
                title = "Quality Assurance",
                id = "trend_qa",
                width = "100%",
                tabPanel(
                    "Trending date feilds",
                    textOutput("Testing")
                )
            )
        )
    )
  tabItems
}

getTabItem <- function() {
}

getBoxItems <- function(id) {
  boxItems <- fluidRow(
        box(plotOutput(id)),
        box(
            "Box content here", br(), "More box content",
            sliderInput("slider", "Slider input:", 1, 100, 50),
            textInput("text", "Text input:")
        )
    )
  boxItems
}

