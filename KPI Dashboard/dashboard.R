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

            output$plot3 <- renderPlot({
                data <- histdata[seq_len(input$slider)]
                hist(data)
            })

            output$Indicators <- renderTable({ 
                as.data.frame(getKPIs(
                    connectionString( Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators"),
                    kpiDayIndicators()
                ))
            })

            output$trend <- renderPlot({
                if( input$server == ""){
                    as_data_frame(histdata)
                }
                else{
                    sql <- kpiQuery(input$KPI, input$date_range[1], input$date_range[2])
                    cs <- connectionString( Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
                    df <- as_data_frame(kpiTable(cs, sql))
                    df %>%
                        time_decompose(ind_value_1, method = "twitter", trend = "1 month") %>%
                        anomalize(remainder, method = "gesd") %>%
                        time_recompose() %>%
                        plot_anomalies(time_recomposed = TRUE)
                }
            })

            getTable <- reactive({
                sql <- kpiPeriodComparisionTable()
                cs <- connectionString( Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
                kpiTable <- getKPIs(cs, sql)
                kpiTable
            })
            
            output$table <- renderDataTable(
                getTable()
            )

            getTrend <- reactive({
                sql <- kpiPeriodComparisionTrend(input$KPI, input$date_range[1], input$date_range[2])
                cs <- connectionString( Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
                kpiTable <- kpiTable(cs, sql)

                kpiTable <- kpiTable %>%
                    filter( kpiTable$ind_id == input$KPI)
                kpiTable
            })

            output$seriesTrend <- renderPlot({ 
                tr <- getTrend()
                
                chartData <- list( tr$series_1, tr$series_2)
                  
                
                chartTitle <- "Previous Year Comparison"
                
                yrange <- c(0, max(tr$series_1) * 1.1)
                xrange <- range(tr$Date)

                plot(
                    xrange,
                    yrange,
                    type="n",
                    xlab="",
                    ylab="Bed Utilization",
                    cex.lab=1.5,
                    main = paste("Comparison with previous year utilization", chartTitle),
                    )
                lines(tr$Date, chartData[[1]], col="aquamarine4",lwd=3)
                lines(tr$Date, na.omit( chartData[[2]]), col="firebrick3",lwd=3)
                abline(v=input$vertical,lty=2) 
                legend(
                    2012,
                    8,
                    c("Current Year","Previous Year"), 
                    col=c('firebrick3','aquamarine4'),
                    pch=15,ncol=1,bty ="n",cex=1.1
                    )
            },height = 500, width = 600)
        }
    )
}