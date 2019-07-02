require(shiny)
require(shinydashboard)
require(dplyr)

getBody <- function() {
  body <- dashboardBody(
        getBoxs(),
        getTabItems(),
        getTableTabs()
    )
  body
}


getBoxs <- function(){
    boxes <- div(fluidRow(
            valueBoxOutput("currentLastAssemble"),
            valueBoxOutput("currentAverageBox"),
            valueBoxOutput("currentPercBox")
        ),
        fluidRow(
            valueBoxOutput("childLastAssemble"),
            valueBoxOutput("childAverageBox"),
            valueBoxOutput("childPercBox")
        ))
}

getTableTabs <- function(){
    tableItems <- tabItem(
                    tabName = "period_comparison_trend",
                    tabBox(
                        title = "KPI Analysis",
                        id = "stat_table",
                        width = "100%",
                        tabPanel(
                            "Statistical Comparison",
                            DTOutput("problemKPIs")
                        ),
                        tabPanel(
                            "Difference Comparison",
                            DTOutput("differenceKPIs")
                        ),
                        tabPanel(
                            "Year over year Comparison",
                            DTOutput("periodCompDT")
                        )
                    )
        )
}

getTabItems <- function() {
  tabItems <- tabItems(
        tabItem(
            tabName = "anomaly_detection",
            tabBox(
                title = "Anomaly Detection",
                id = "anomaly_dect",
                width = "100%",
                tabPanel(
                    "Current KPI",
                    plotlyOutput('trend', height = "400px")
                ),
                tabPanel(
                    "Selected Child KPI",
                    plotlyOutput('trendChild', height = "400px")
                )
            )
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
        ), tabItem(
            tabName = "period_comparison_interval_trend",
            tabBox(
                title = "Trend Comparison",
                id = "trend_aggregate",
                width = "100%",
                tabPanel(
                    "Current KPI Comparison",
                    plotlyOutput('interactiveAggregate', height = "400px", width = "100%")
                ),
                tabPanel(
                    "Selected Child KPI Comparison",
                    plotlyOutput('interactiveAggregateChild', height = "400px", width = "100%")
                )
            )
        ),
        tabItem(
            tabName = "prediction_anomalies",
            tabBox(
                title = "Trend Comparison",
                id = "prediction_trend",
                width = "100%",
                tabPanel(
                    "Current KPI Predicted Values",
                    plotlyOutput('kpiPrediction', height = "400px", width = "100%")
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

