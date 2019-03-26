require(shiny)
require(shinydashboard)
require(dplyr)

getBody <- function(){
    body <- dashboardBody(
        getTabItems(),
        tableOutput( "Indicators")
    ) 
    body
}

getTabItems <- function(){
    tabItems <- tabItems(
        tabItem( 
            tabName = "anomaly_detection",
            plotOutput('trend', height = "400px")
        ),
        tabItem( 
            tabName = "period_comparison_table",
            dataTableOutput("table")
        ),
        tabItem( 
            tabName = "period_comparison_trend",
            plotOutput('seriesTrend', height = "400px")
        ),
        tabItem( 
            tabName = "quality_assurance",
            getBoxItems("plot1")
        )
    )
    tabItems
}

getTabItem <- function( ){
}

getBoxItems <- function(id){
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

