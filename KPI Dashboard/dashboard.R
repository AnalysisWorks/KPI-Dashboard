library(shiny)
library(shinydashboard)
library(dplyr)
library(anomalyDetection)
library(anomalize)
library(tidyverse)
library(plotly)

dashboard <- function(dataset) {
  #, mapping
  shinyApp(
        myUI <- dashboardPage(
            skin = "purple",
            getHeader(),
            getSidebar(),
            getBody()
        ),
        myServer <- function(input, output, session) {

          # Close dashboard app entirely
          observe({
            if (input$close > 0) stopApp()
          })

          # Retrieve any data passed into initialize app function
          getData <- reactive({
            x <- dataset
          })

          # Get KPI hierarchy mapping
          getMapping <- reactive({
            as.data.frame(getKPIs(
                    connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators"),
                    kpiMapping()
                ))
          })

          getLabels <- reactive({
            sql <- kpiDetails(input$KPI)
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            labels <- getKPIs(cs, sql)
            title <- ""
            if (is.na(labels$group_field_1) || labels$group_field_1 == "NA") {
              title <- paste(
                    labels$ind_group_cd,
                    "Authority Wide",
                    sep = ": ")
            }
            else if (is.na(labels$group_field_2) || labels$group_field_2 == "NA") {
              title <- paste(
                    labels$ind_group_cd,
                    paste(
                            labels$group_field_1,
                            labels$group_value_1,
                            sep = ": "),
                    sep = ", ")
            }
            else if (is.na(labels$group_field_3) || labels$group_field_3 == "NA") {
              title <- paste(
                    labels$ind_group_cd,
                    paste(
                            labels$group_field_1,
                            labels$group_value_1,
                            sep = ": "),
                    paste(
                            labels$group_field_2,
                            labels$group_value_2,
                            sep = ": "),
                    sep = ", ")
            }
            else if (is.na(labels$group_field_4) || labels$group_field_4 == "NA") {
              title <- paste(
                    labels$ind_group_cd,
                    paste(
                            labels$group_field_1,
                            labels$group_value_1,
                            sep = ": "),
                    paste(
                            labels$group_field_2,
                            labels$group_value_2,
                            sep = ": "),
                    paste(
                            labels$group_field_3,
                            labels$group_value_3,
                            sep = ": "),
                    sep = ", ")
            }
            else {
              title <- paste(
                    labels$ind_group_cd,
                    paste(
                            labels$group_field_1,
                            labels$group_value_1,
                            sep = ": "),
                    paste(
                            labels$group_field_2,
                            labels$group_value_2,
                            sep = ": "),
                    paste(
                            labels$group_field_3,
                            labels$group_value_3,
                            sep = ": "),
                    paste(
                            labels$group_field_4,
                            labels$group_value_4,
                            sep = ": "),
                    sep = ", ")
            }
            title
          })

          getChildLabels <- reactive({
            if (input$childKPI == "NA" || is.na(input$childKPI)) {
              return(NULL)
            }
            sql <- kpiDetails(input$childKPI)
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            labels <- getKPIs(cs, sql)
            title <- ""
            if (is.na(labels$group_field_1) || labels$group_field_1 == "NA") {
              title <- paste(
                    labels$ind_group_cd,
                    "Authority Wide",
                    sep = ": ")
            }
            else if (is.na(labels$group_field_2) || labels$group_field_2 == "NA") {
              title <- paste(
                    labels$ind_group_cd,
                    paste(
                            labels$group_field_1,
                            labels$group_value_1,
                            sep = ": "),
                    sep = ", ")
            }
            else if (is.na(labels$group_field_3) || labels$group_field_3 == "NA") {
              title <- paste(
                    labels$ind_group_cd,
                    paste(
                            labels$group_field_1,
                            labels$group_value_1,
                            sep = ": "),
                    paste(
                            labels$group_field_2,
                            labels$group_value_2,
                            sep = ": "),
                    sep = ", ")
            }
            else if (is.na(labels$group_field_4) || labels$group_field_4 == "NA") {
              title <- paste(
                    labels$ind_group_cd,
                    paste(
                            labels$group_field_1,
                            labels$group_value_1,
                            sep = ": "),
                    paste(
                            labels$group_field_2,
                            labels$group_value_2,
                            sep = ": "),
                    paste(
                            labels$group_field_3,
                            labels$group_value_3,
                            sep = ": "),
                    sep = ", ")
            }
            else {
              title <- paste(
                    labels$ind_group_cd,
                    paste(
                            labels$group_field_1,
                            labels$group_value_1,
                            sep = ": "),
                    paste(
                            labels$group_field_2,
                            labels$group_value_2,
                            sep = ": "),
                    paste(
                            labels$group_field_3,
                            labels$group_value_3,
                            sep = ": "),
                    paste(
                            labels$group_field_4,
                            labels$group_value_4,
                            sep = ": "),
                    sep = ", ")
            }
            title
          })


          # Display available child KPIs based off current selection
          output$childKPIs <- renderUI({
            mapping <- getMapping()
            children <- mapping %>%
                        filter(mapping$kpi == input$KPI)
            if (is.na(children$child_group_value_2)) {
              return(radioButtons("childKPI",
                                        "Child KPIs:",
                                        choices = setNames(as.list(children$child_kpi), children$child_group_value_1)
                                    )
                        )
            }
            if (is.na(children$child_group_value_3)) {
              return(radioButtons("childKPI",
                                        "Child KPIs:",
                                        choices = setNames(as.list(children$child_kpi), children$child_group_value_2)
                                    )
                        )
            }
            if (is.na(children$child_group_value_4)) {
              return(radioButtons("childKPI",
                                        "Child KPIs:",
                                        choices = setNames(as.list(children$child_kpi), children$child_group_value_3)
                                    )
                        )
            }
            return(radioButtons("childKPI",
                                        "Child KPIs:",
                                        choices = setNames(as.list(children$child_kpi), children$child_group_value_4)
                                    )
                        )
          })

          # Attempt to group KPIs based on group field
          output$childKPIGroups <- renderUI({
            mapping <- getMapping()
            currentKPI <- mapping %>%
                        filter(mapping$kpi == input$childKPI)
            if (is.na(currentKPI$child_group_field_1)) {
              return(
                        selectInput("childKPIGroup", "Grouping", list("Site Code" = c(unique(currentKPI$child_group_value_1))))
                    )
            }
            if (is.na(currentKPI$child_group_field_2)) {
              return(
                        selectInput("childKPIGroup", "Grouping",
                        list(
                            "Program Code" = c(unique(currentKPI$child_group_value_2)),
                            "Nursing Program Code" = c(unique(currentKPI$child_group_value_2)),
                         ))
                    )
            }
            if (is.na(currentKPI$child_group_field_3)) {
              return(
                        selectInput("childKPIGroup", "Grouping", unique(currentKPI$child_group_field_3))
                    )
            }
            return(
                    selectInput("childKPIGroup", "Grouping", unique(currentKPI$child_group_field_4))
                )
          })


          getChildKPI <- reactive({
            mapping <- getMapping()
            currentKPI <- mapping %>%
                        filter(mapping$kpi == input$childKPI)
            childKPI <- paste("Child KPI: ", unique(currentKPI$kpi), sep = "")
          })

          getParentKPI <- reactive({
            mapping <- getMapping()
            currentKPI <- mapping %>%
                        filter(mapping$kpi == input$KPI)
            parentKPI <- paste("Parent KPI: ", unique(currentKPI$parent_kpi), sep = "")
          })

          output$childKPI <- renderText({
            getChildKPI()
          })

          output$parentKPI <- renderText({
            getParentKPI()
          })


          output$Indicators <- renderTable({
            as.data.frame(getKPIs(
                    connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators"),
                    kpiDayIndicators()
                ))
          })

          getProblemKPIs <- reactive({
            if( input$anomalies > 1){
            kpi_ind_ids <- getMapping()
            ind_ids <- cbind(unique(kpi_ind_ids$kpi))
            date_range <- as.data.frame(seq(as.Date(input$date_range[1]), as.Date(input$date_range[2]), "days"))
            names(date_range) <- c("Date")

            anomaly.counts <- data.frame(matrix(ncol = 2, nrow = 0))
            names(anomaly.counts) <- c('ind_id', 'anomalies')
            for (i in 1:length(ind_ids)) {
              sql <- kpiQuery(ind_ids[[i]], input$date_range[1], input$date_range[2])
              cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
              df <- as_data_frame(kpiTable(cs, sql))
              if (nrow(df) < 10) {
                next
              }
              df <- left_join(date_range, df)
              df$ind_id <- ind_ids[[i]]
              df[is.na(df)] <- 0
              df <- as_data_frame(df)

              anomalies <- df %>%
                            time_decompose(ind_value_1, method = "twitter", trend = "1 month") %>%
                            anomalize(remainder, method = "gesd") %>%
                            time_recompose()
              anomaly.counts[i,] <- list(ind_ids[[i]], sum(anomalies$anomaly == 'Yes'))
            }
            sql <- kpiDetailsAll()
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            labels <- getKPIs(cs, sql)
            qa_table <- inner_join(anomaly.counts[order(-anomaly.counts$anomalies),], labels)
            }
          })

          output$problemKPIs <- renderDataTable(
                getProblemKPIs()
            )



          # Anomaly Trends
          getTrendAnomaly <- reactive({
            sql <- kpiQuery(input$KPI, input$date_range[1], input$date_range[2])
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)


            sql <- kpiQuery(input$KPI, input$date_range[1], input$date_range[2])
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            df <- as_data_frame(kpiTable(cs, sql))
            anomalies <- df %>%
                    time_decompose(ind_value_1, method = "twitter", trend = "1 month") %>%
                    anomalize(remainder, method = "gesd") %>%
                    time_recompose()
            kpiTable <- inner_join(kpiTable, anomalies)
            kpiTable
          })

          getTrendChildAnomaly <- reactive({
            
              sql <- kpiQuery(input$childKPI, input$date_range[1], input$date_range[2])
              cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
              kpiTable <- kpiTable(cs, sql)

              sql <- kpiQuery(input$childKPI, input$date_range[1], input$date_range[2])
              cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
              df <- as_data_frame(kpiTable(cs, sql))
              anomalies <- df %>%
                    time_decompose(ind_value_1, method = "twitter", trend = "1 month") %>%
                    anomalize(remainder, method = "gesd") %>%
                    time_recompose()
              kpiTable <- inner_join(kpiTable, anomalies)
              kpiTable
            
          })


          output$trend <- renderPlotly({
              df <- getTrendAnomaly()
              label <- getLabels()
              p <- plot_ly(
                        df,
                        x = ~Date,
                        y = ~ind_value_1,
                        name = "Current Value",
                        symbol = ~anomaly,
                        symbols = c('circle', 'x'),
                        colors = c('red', 'gray'),
                        type = 'scatter',
                        mode = 'markers',
                        source = "subset"
                    ) %>%
                    layout(
                        title = label,
                        xaxis = list(title = 'Date'),
                        yaxis = list(title = 'Value')
                    )
              p
              mytext = paste("Current Value = ", df$ind_value_1, sep = "")
              pp = plotly_build(p)
              style(pp, text = mytext, hoverinfo = "text", traces = c(1, 1))
            
          })

          output$trendChild <- renderPlotly({
            
              label <- getChildLabels()
              df <- getTrendChildAnomaly()
              p <- plot_ly(
                        df,
                        x = ~Date,
                        y = ~ind_value_1,
                        name = "Current Value",
                        symbol = ~anomaly,
                        symbols = c('circle', 'x'),
                        colors = c('red', 'gray'),
                        type = 'scatter',
                        mode = 'markers',
                        source = "subset"
                    ) %>%
                    layout(
                        title = label,
                        xaxis = list(title = 'Date'),
                        yaxis = list(title = 'Value')
                    )
              p
              mytext = paste("Current Value = ", df$ind_value_1, sep = "")
              pp = plotly_build(p)
              style(pp, text = mytext, hoverinfo = "text", traces = c(1, 1))
            
          })


          # Year over year series comparison

          getTrend <- reactive({
            sql <- kpiPeriodComparisionTrend(input$KPI, input$date_range[1], input$date_range[2])
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)
            kpiTable
          })

          getTrendChild <- reactive({
            sql <- kpiPeriodComparisionTrend(input$childKPI, input$date_range[1], input$date_range[2])
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)
            kpiTable
          })


          output$interactiveTrend <- renderPlotly({
            label <- getLabels()
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
                        title = label,
                        xaxis = list(title = 'Date'),
                        yaxis = list(title = 'Value')
                    )
            p
            mytext = paste("Current Value = ", df$series_1, "\n", "Previous Year Value = ", df$series_2, "\n", sep = "")
            pp = plotly_build(p)
            style(pp, text = mytext, hoverinfo = "text", traces = c(1, 2, 3))
          })

          output$interactiveTrendChild <- renderPlotly({

              label <- getChildLabels()
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
                        title = label,
                        xaxis = list(title = 'Date'),
                        yaxis = list(title = 'Value')
                    )
              p
              mytext = paste("Current Value = ", df$series_1, "\n", "Previous Year Value = ", df$series_2, "\n", sep = "")
              pp = plotly_build(p)
              style(pp, text = mytext, hoverinfo = "text", traces = c(1, 2, 3))
            
          })

          output$interactiveTable <- renderText({
            event.data <- event_data("plotly_selected", source = "subset")

            # If NULL dont do anything
            if (is.null(event.data) == T) return(NULL)
            stats <- subset(plot.df)[subset(event.data)$series_1]
            summary(stats)

          })


        getTrendComp <- reactive({
            sql <- kpiPeriodComparision(input$KPI, input$date_range[1], input$date_range[2])
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)
            kpiTable
          })

          getTrendChildComp <- reactive({
            sql <- kpiPeriodComparision(input$childKPI, input$date_range[1], input$date_range[2])
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)
            kpiTable
          })

          # Year over year comparison with different plotly style
          output$interactiveAggregate <- renderPlotly({
            df <- getTrendComp()

            p <- df %>%
                plot_ly(
                    x = ~Date,
                    y = ~series_1,
                    type = "scatter",
                    mode = 'lines',
                    fill = 'tozeroy',
                    name = "Current KPIs"
                ) %>%
                add_trace(
                    y = ~df$series_2,
                    name = 'Previous KPIs',
                    mode = 'lines',
                    fill = 'tozeroy'
                ) %>%
                layout(
                    title = "<b>Aggregate Data</b><br>test phase",
                    xaxis = list(
                        type = 'date'
                    ),
                    yaxis = list(
                        title = "Volumes"
                    )
                )
          })

          output$interactiveAggregateChild <- renderPlotly({
            df <- getTrendChildComp()

            p <- df %>%
                plot_ly(
                    x = ~Date,
                    y = ~series_1,
                    type = "scatter",
                    mode = 'lines',
                    fill = 'tozeroy',
                    name = "Current KPIs"
                ) %>%
                add_trace(
                    y = ~df$series_2,
                    name = 'Previous KPIs',
                    mode = 'lines',
                    fill = 'tozeroy'
                ) %>%
                layout(
                    title = "<b>Aggregate Data</b><br>test phase",
                    xaxis = list(
                        type = 'date'
                    ),
                    yaxis = list(
                        title = "Volumes"
                    )
                )
          })
        }
    )
}