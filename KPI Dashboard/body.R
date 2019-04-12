require(shiny)
require(shinydashboard)
require(dplyr)

getBody <- function(){
    body <- dashboardBody(
        getTabItems()
        #tableOutput( "Indicators")
    ) 
    body
}

getTabItems <- function(){
    tabItems <- tabItems(
        tabItem( 
            tabName = "anomaly_detection",
            plotOutput('trend', height = "400px"),
            plotOutput('trendChild', height = "400px")
        ),
        tabItem( 
            tabName = "period_comparison_table",
            dataTableOutput("table")
        ),
        tabItem( 
            tabName = "period_comparison_trend",
            tabBox(
                title = "Trend Comparison",
                id = "trend_comp",
                width = "100%",
                tabPanel( 
                    "Current KPI Comparison",
                    plotlyOutput('interactiveTrend', height = "400px", width = "100%")
                ),
                tabPanel( 
                    "Selected Child KPI Comparison",
                    plotlyOutput('interactiveTrendChild', height = "400px", width = "100%")
                )
            )
        ),tabItem( 
            tabName = "period_comparison_interval_trend",
            plotlyOutput('interactiveAggregate', height = "400px", width = "100%"),
            plotlyOutput('interactiveAggregateChild', height = "400px", width = "100%")
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

