library(shiny)
library(shinydashboard)
library(dplyr)
library(anomalyDetection)
library(anomalize)
library(tidyverse)
library(plotly)

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

            getMapping <- reactive({
                as.data.frame(getKPIs(
                    connectionString( Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators"),
                    kpiMapping() 
                ))
            })


            

            output$childKPIs <- renderUI({
                mapping <- getMapping()
                children <- mapping %>%
                        filter( mapping$kpi == input$KPI)
                if( is.na(children$child_group_value_2)){
                    return( radioButtons( "childKPI", 
                                        "Child KPIs:", 
                                        choices = setNames( as.list(children$child_kpi), children$child_group_value_1)
                                    )
                        )
                }
                if( is.na(children$child_group_value_3)){
                    return( radioButtons( "childKPI", 
                                        "Child KPIs:", 
                                        choices = setNames( as.list(children$child_kpi), children$child_group_value_2)
                                    )
                        )
                }
                if( is.na(children$child_group_value_4)){
                    return( radioButtons( "childKPI", 
                                        "Child KPIs:", 
                                        choices = setNames( as.list(children$child_kpi), children$child_group_value_3)
                                    )
                        )
                }
                return( radioButtons( "childKPI", 
                                        "Child KPIs:", 
                                        choices = setNames( as.list(children$child_kpi), children$child_group_value_4)
                                    )
                        )                
            })


            output$childKPIGroups <- renderUI({
                mapping <- getMapping()
                currentKPI <- mapping %>%
                        filter( mapping$kpi == input$childKPI)
                if( is.na(currentKPI$child_group_field_1)){
                    return(
                        selectInput( "childKPIGroup", "Grouping", list( "Site Code" = c(unique( currentKPI$child_group_value_1)) ))
                    )
                }
                if( is.na(currentKPI$child_group_field_2)){
                    return(
                        selectInput( "childKPIGroup", "Grouping", 
                        list( 
                            "Program Code" = c(unique( currentKPI$child_group_value_2)),
                            "Nursing Program Code" = c(unique( currentKPI$child_group_value_2)),
                         ))
                    )
                }
                if( is.na(currentKPI$child_group_field_3)){
                    return(
                        selectInput( "childKPIGroup", "Grouping", unique( currentKPI$child_group_field_3))
                    )
                }
                return(
                    selectInput( "childKPIGroup", "Grouping", unique( currentKPI$child_group_field_4))
                )
            })


            getChildKPI <- reactive({
                mapping <- getMapping()
                currentKPI <- mapping %>%
                        filter( mapping$kpi == input$childKPI)
                childKPI <- paste( "Child KPI: ", unique(currentKPI$kpi), sep = "")
            })

            getParentKPI <- reactive({
                mapping <- getMapping()
                currentKPI <- mapping %>%
                        filter( mapping$kpi == input$KPI)
                parentKPI <- paste( "Parent KPI: ", unique(currentKPI$parent_kpi), sep = "")
            })

            output$childKPI <- renderText({
                getChildKPI()
            })

            output$parentKPI <- renderText({
                getParentKPI()
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

            output$trendChild <- renderPlot({
                if( input$server == ""){
                    as_data_frame(histdata)
                }
                else{
                    sql <- kpiQuery(input$childKPI, input$date_range[1], input$date_range[2])
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
                kpiTable
            })

            getTrendChild <- reactive({
                sql <- kpiPeriodComparisionTrend(input$childKPI, input$date_range[1], input$date_range[2])
                cs <- connectionString( Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
                kpiTable <- kpiTable(cs, sql)
                kpiTable
            })

            output$interactiveTrend <- renderPlotly({
                df <- getTrend()
                p <- plot_ly(
                        df, 
                        x = ~Date, 
                        y = ~series_1, 
                        name = "Current Value", 
                        type = 'scatter',
                        mode = 'lines+markers',
                        source = "subset"
                    ) %>% 
                    add_trace(
                        y = ~series_2,
                        name = 'Previous Year value',
                        mode = 'lines+markers'
                    ) %>%
                    layout(
                        title = 'Trend Comparison to previous year',
                        xaxis = list(title = 'Date'),
                        yaxis = list(title = 'Value')
                    )
                    
                p    
                
                mytext = paste("Current Value = ", df$series_1, "\n" , "Previous Year Value = ", df$series_2, "\n",  sep="")    
                pp=plotly_build(p)   
                style( pp, text=mytext, hoverinfo = "text", traces = c(1, 2, 3) )
            })

            output$interactiveTrendChild <- renderPlotly({
                df <- getTrendChild()
                p <- plot_ly(
                        df, 
                        x = ~Date, 
                        y = ~series_1, 
                        name = "Current Value", 
                        type = 'scatter',
                        mode = 'lines+markers',
                        source = "subset"
                    ) %>% 
                    add_trace(
                        y = ~series_2,
                        name = 'Previous Year value',
                        mode = 'lines+markers'
                    ) %>%
                    layout(
                        title = 'Trend Comparison to previous year',
                        xaxis = list(title = 'Date'),
                        yaxis = list(title = 'Value')
                    )
                    
                p    
                
                mytext = paste("Current Value = ", df$series_1, "\n" , "Previous Year Value = ", df$series_2, "\n",  sep="")    
                pp=plotly_build(p)   
                style( pp, text=mytext, hoverinfo = "text", traces = c(1, 2, 3) )
            })

            output$interactiveTable <- renderText({
                event.data <- event_data("plotly_selected", source = "subset")
                
                # If NULL dont do anything
                if(is.null(event.data) == T) return(NULL)


                stats <- subset( plot.df)[subset(event.data)$series_1]

                summary(stats)

            })
        }
    )
}