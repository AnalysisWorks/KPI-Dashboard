library(shiny)
library(shinydashboard)
library(dplyr)
library(anomalyDetection)
library(anomalize)

dashboard <- function(dataset) { #, mapping
    shinyApp(
        myUI <- dashboardPage(
            skin = "purple",
            getHeader(),
            getSidebar(),
            getBody()
        ),
        myServer <- function(input, output, session) {

            # If close button is selected, close the application
            observe({
                if (input$close > 0) stopApp()
            })

            getData <- reactive({
                x <- dataset
            })
            
            set.seed(122)
            histdata <- rnorm(500)
            output$plot1 <- renderPlot({
                data <- histdata[seq_len(input$slider)]
                hist(data)
            })


            output$trend <- renderPlot({
                if( input$server == ""){
                    as_data_frame(histdata)
                }
                else{
                    sql <- kpiQuery()
                    cs <- connectionString( Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
                    df <- as_data_frame(kpiTable(cs, sql))
    
                    df <- df %>%
                        filter( df$ind_id == 5)
    
                    df %>%
                        time_decompose(ind_value_1, method = "twitter", trend = "1 month") %>%
                        anomalize(remainder, method = "gesd") %>%
                        time_recompose() %>%
                        plot_anomalies(time_recomposed = TRUE)
                    df
                }
            })
        }
    )
}