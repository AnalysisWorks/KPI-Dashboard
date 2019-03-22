require(shiny)
require(shinydashboard)
require(dplyr)

getBody <- function(){
    body <- dashboardBody(
        getTabItems()
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
            getValueBoxItems(),
            getBoxItems("plot2")
        ),
        tabItem( 
            tabName = "quality_assurance",
            getInfoBoxItems(),
            getTabBoxItems()
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

getTabBoxItems <- function(){
    tabBoxItems <- fluidRow(
        tabBox(
            title = "First tabBox",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", height = "250px",
            tabPanel("Tab1", "First tab content"),
            tabPanel("Tab2", "Tab content 2")
        ),
        tabBox(
            side = "right", height = "250px",
            selected = "Tab3",
            tabPanel("Tab1", "Tab content 1"),
            tabPanel("Tab2", "Tab content 2"),
            tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
        )
    )
    tabBoxItems
}



getInfoBoxItems <- function(){
    infoBoxItems <- fluidRow(
        # A static infoBox
        infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
        # Dynamic infoBoxes
        infoBoxOutput("progressBox"),
        infoBoxOutput("approvalBox")
    )
    infoBoxItems
}

getValueBoxItems <- function(){
    valueBoxItems <- fluidRow(
        # A static valueBox
        valueBox(10 * 2, "New Orders", icon = icon("credit-card")),

        # Dynamic valueBoxes
        valueBoxOutput("progressBox2"),

        valueBoxOutput("approvalBox2")
    )
    valueBoxItems
}