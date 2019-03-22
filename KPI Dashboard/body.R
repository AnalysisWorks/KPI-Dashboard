require(shiny)
require(shinydashboard)
require(dplyr)

getBody <- function(){
    body <- dashboardBody(
        getTabItems(),
        plot_anomalies("trend")
    ) 
    body
}

getTabItems <- function(){
    tabItems <- tabItems(
        tabItem( 
            tabName = "anomaly_detection",
            getBoxItems("plot1")
        ),
        tabItem( 
            tabName = "period_comparison",
            getBoxItems("plot2")
        ),
        tabItem( 
            tabName = "quality_assurance",
            getBoxItems("plot3")
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

