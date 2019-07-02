library(shiny)
library(shinyEventLogger)
library(shinydashboard)
library(dplyr)
library(tibbletime)
library(anomalize)
library(tidyverse)
library(plotly)
library(DT)
library(sparkline)


dashboard <- function(debug = FALSE) {
    if(debug){
        set_logging()
    }
    enableBookmarking( store = "server")
    shinyApp(
        ui <- function(request){
            myUI <- dashboardPage(
                    skin = "purple",
                    getHeader(),
                    getSidebar(),
                    getBody()
                )
        },
        myServer <- function(input, output, session) {
        if(debug){
            set_logging_session()
        }
          # Close dashboard app entirely
        observe({
            if (input$close > 0 && !is.na(input$close)) stopApp()
        })

        observe({
            req(input$server, input$date_range)
            query <- parseQueryString(session$clientData$url_search)
            if (!is.null(query[['server']])) {
                updateSelectizeInput(session, "server", selected = query[['server']])
            }
            if (!is.null(query[['start_dt']])) {
                updateDateRangeInput(session, "date_range", start = query[['start_dt']])
            }
            if (!is.null(query[['end_dt']])) {
                updateDateRangeInput(session, "date_range", end = query[['end_dt']])
            }
        })

        observe({
            req(input$indGroup, input$indValue)
            query <- parseQueryString(session$clientData$url_search)
            if (!is.null(query[['kpiType']])) {
                updateSelectizeInput(session, "indGroup", selected = query[['kpiType']])
            }
            if (!is.null(query[['kpiValue']])) {
                updateSelectizeInput(session, "indValue", selected = query[['kpiValue']])
            }
        })

        observe({
            req(input$KPI)
            query <- parseQueryString(session$clientData$url_search)
            if (!is.null(query[['kpi']])) {
                updateSelectizeInput(session, "KPI", selected = query[['kpi']])
            }
        })

        observeEvent(input$parent, {
            kpi <- getParentKPI()
            if( is.null(kpi) || is.na(kpi)){
                return(NULL)
            }
            updateSelectizeInput(session, "KPI", selected = kpi)
        })

        observeEvent(input$child, {
            kpi <- getChildKPI()
            if( is.null(kpi) || is.na(kpi) || length(kpi) == 0){
                return(NULL)
            }
            updateSelectizeInput(session, "KPI", selected = kpi)
        })

        output$KPIGroups <- renderUI({
            req(input$server)
            server <- input$server
            parsedArgs <- parseQueryString(session$clientData$url_search)
            return(kpiGroupsUI(inputId = "indGroup", id = "KPIGroups", server = server, parsedArgs = parsedArgs, debug = debug))
        })

        output$KPIValues <- renderUI({
            req(input$server, input$indGroup)
            server <- input$server
            parsedArgs <- parseQueryString(session$clientData$url_search)
            group <- input$indGroup
            return(kpiValueUI(inputId = "indValue", id = "KPIValues", server, parsedArgs, group, debug))
        })

        output$KPIout <- renderUI({
            req(input$server, input$indGroup)
            server <- input$server
            parsedArgs <- parseQueryString(session$clientData$url_search)
            group <- input$indGroup
            return(kpiUI(inputId = "KPI", id = "KPIout", "Select Value", server, parsedArgs, group, debug))
        })

        # Get KPI hierarchy mapping
        getMapping <- reactive({
            mapping <- getCompleteMapping(input$server, input$indGroup, debug)
        })

        getServer <- reactive({
            serv <- input$server
        })

        getGrouping <- reactive({
            group <- input$indGroup
        })

        getVal <- reactive({
            val <- input$indValue
        })

        getStartDt <- reactive({
            start <- input$date_range[1]
        })
        getEndDt <- reactive({
            start <- input$date_range[2]
        })

        output$currentLastAssemble <- renderValueBox({
            req(input$server, input$KPI, input$indGroup, input$indValue)
            valueBox(
                subtitle = "Last date in current assembly",
                getKPILastDate(debug, input$KPI, input$indGroup, input$indValue, input$server),
                icon = icon("credit-card"),
                color = "purple"
            )
        })
        output$currentAverageBox <- renderValueBox({
            req(input$server, input$KPI, input$date_range, input$indGroup, input$indValue)
            valueBox(
                subtitle = "Current assembly Average",
                getKPIAverage(debug, input$KPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue, input$server),
                icon = icon("credit-card"),
                color = "purple"
            )
        })
        output$currentPercBox <- renderValueBox({
            req(input$server, input$KPI, input$date_range, input$indGroup, input$indValue)
            input$target_perc
            valueBox(
                subtitle = paste0("Current cycle ",input$target_perc," Percentile"),
                getKPIPercentile(debug, input$KPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue, input$server, input$target_perc),
                icon = icon("credit-card"),
                color = "purple"
            )
        })
        output$childLastAssemble <- renderValueBox({
            req(input$server, input$childKPI, input$indGroup, input$indValue)
            if( input$chartTabs == "period_comparison_interval_trend"){
                return(
                    valueBox(
                        subtitle = "Last date in previous Assembly",
                        getKPILastCycleDate(debug, input$KPI, input$indGroup, input$indValue, input$server),
                        icon = icon("credit-card"),
                        color = "purple"
                    )
                )
            }
            return(
                valueBox(
                    subtitle = "Last date for child KPI",
                    getKPILastDate(debug, input$childKPI, input$indGroup, input$indValue, input$server),
                    icon = icon("credit-card"),
                    color = "purple"
                )
            )
        })
        output$childAverageBox <- renderValueBox({
            req(input$server, input$childKPI, input$date_range, input$indGroup, input$indValue)
            if( input$chartTabs == "period_comparison_interval_trend"){
                return(
                    valueBox(
                        subtitle = "Last Cycle Average",
                        getKPILastCycleAverage(debug, input$KPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue, input$server),
                        icon = icon("credit-card"),
                        color = "purple"
                    )
                )
            }
            return(
                valueBox(
                    subtitle = "Child KPI Average",
                    getKPIAverage(debug, input$childKPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue, input$server),
                    icon = icon("credit-card"),
                    color = "purple"
                )
            )
        })
        output$childPercBox <- renderValueBox({
            req(input$server, input$childKPI, input$date_range, input$indGroup, input$indValue)
            if( input$chartTabs == "period_comparison_interval_trend"){
                return(
                    valueBox(
                        subtitle = paste0("Last Cycle ", input$target_perc," Percentile"),
                        getKPILastCyclePercentile(debug, input$KPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue, input$server, input$target_perc),
                        icon = icon("credit-card"),
                        color = "purple"
                    )
                )
            }
            return(
                valueBox(
                    subtitle = paste0("Child ", input$target_perc," Percentile"),
                    getKPIPercentile(debug, input$childKPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue, input$server, input$target_perc),
                    icon = icon("credit-card"),
                    color = "purple"
                )
            )
        })


        # Get KPI hierarchy mapping
        getKeyMapping <- reactive({
            if(debug){
                log_event("Start getKeyMapping")
            }
            if(is.null(input$server) || is.null(input$indGroup)){
                if(debug){
                log_event(paste("KPI server:", input$server, "KPI Group:", input$indGroup, sep = " "))
                }
                return(NULL)
            }     
            df <- as.data.frame(getKPIs(
                    connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators"),
                    kpiKeyMapping(input$indGroup)
                ))
            if(debug){
                log_event("Finished getKeyMapping")
            }
            df
        })

        getKPI <- reactive({
            kpi <- input$KPI
            if( kpi == "" || is.null(kpi)){
                return(NULL)
            }
            kpi
        })
        getKPIchild <- reactive({
            kpi <- input$childKPI
            if( kpi == "" || is.null(kpi)){
                return(NULL)
            }
            kpi
        })
        
        getLabels <- reactive({
            kpi <- getKPI()
            labels <- getChartLabels(kpi = kpi, server = input$server, group = input$indGroup, debug = debug)
        })

        getChildLabels <- reactive({
            kpi <- getKPIchild()
            labels <- getChartLabels(kpi = kpi, server = input$server, group = input$indGroup, debug = debug)
        })

        output$childKPIs <- renderUI({
            req(input$server, input$KPI, input$indGroup)
            inputId <- "childKPI"
            id <- "childKPIs"
            kpi <- input$KPI
            server <- input$server
            group <- input$indGroup

            #mapping <- getCompleteMapping(server, group, debug)
            mapping <- as.data.frame(getKPIs(
                connectionString(Server = paste("aworks300", server, sep = "\\"), Database = "LH_Indicators"),
                kpiMapping(group)
            ))
            if(debug){
                log_event("Start childKPIs")
            }
            if(is.null(mapping)){
                if(debug){
                    log_event("childKPIs call to mapping returned NULL")
                } 
                return(NULL)
            }
            if(is.null(server) || is.null(kpi)){
                if(debug){
                    log_event(paste("childKPIs server", server, "KPI", kpi, sep = ": "))
                }
                return(NULL)
            }
            children <- mapping[ mapping$kpi == kpi, ]
            if (all(is.na(children$child_group_value_2))) {
                if(debug){
                    log_event("Return childKPIs 1")
                }
                return(selectizeInput(inputId,
                                        "Child KPIs:",
                                        choices = setNames(as.list( children$child_kpi), paste( children$child_group_field_1, children$child_group_value_1, sep = ": "))
                                    )
                        )
            }
            if (all(is.na(children$child_group_value_3))) {
                if(debug){
                    log_event("Return childKPIs 2")
                }
                return(selectizeInput(inputId,
                                        "Child KPIs:",
                                        choices = setNames(as.list(children$child_kpi), paste(children$child_group_field_2, children$child_group_value_2, sep = ": "))
                                    )
                        )
            }
            if (all(is.na(children$child_group_value_4))) {
                if(debug){
                    log_event("Return childKPIs 3")
                }
                return(selectizeInput(inputId,
                                        "Child KPIs:",
                                        choices = setNames(as.list(children$child_kpi), paste( children$child_group_field_3, children$child_group_value_3, sep = ": "))
                                    )
                        )
            }
            if(debug){
                    log_event("Return childKPIs 4")
            }
            return(selectizeInput(inputId,
                                        "Child KPIs:",
                                        choices = setNames(as.list(children$child_kpi), paste( children$child_group_field_4, children$child_group_value_4, sep = ": "))
                                    )
                        )
        })

        getChildKPI <- reactive({
            childKPI <- getChildIndicator(debug, input$childKPI, input$server, getMapping())
            childKPI
        })

        getParentKPI <- reactive({
            parentKPI <- getParentIndicator(debug, input$KPI, input$server, getMapping())
            parentKPI
        })

        getProblemKPIs <- eventReactive(input$anomalies,{ 
            table <- statisticalAnomalies( debug, getKeyMapping(), input$date_range[1], input$date_range[2], input$indGroup, input$indValue, input$server)
            table
        })

        getStatDT <- reactive({
            spk <- getAllKPITrends()
            diff <- getProblemKPIs()
            if(is.null(spk) || is.null(diff)){
                return(NULL)
            }
            df <- inner_join( spk, diff)
            d1 <- datatable(
                df, 
                selection = list(
                    mode ="single",
                    target = "cell"
                    ), 
                rownames= FALSE,
                escape = FALSE,
                filter = list( 
                    position = 'top', 
                    clear = FALSE
                    ),
                options = list(
                    columnDefs = list(
                        list( 
                            className = 'dt-center',
                            targets = c(1:11)
                        )),
                    search = list(
                        regex = TRUE, 
                        caseInsensitive = FALSE
                    ),
                    fnDrawCallback = htmlwidgets::JS(
                    'function(){
                        HTMLWidgets.staticRender();
                    }')
                )
            )
            d1$dependencies <- append(d1$dependencies, htmlwidgets:::getDependency("sparkline"))
            d1
        })

        output$problemKPIs <- renderDT(
            getStatDT()
        )

        getPeriodCompKPIs <- eventReactive(input$periodComp,{ 
            table <- periodDifferencePercentages(debug, getKeyMapping(), input$date_range[1], input$date_range[2], input$indGroup, input$indValue, input$server)
            table
        })
        getPeriodCompDT <- reactive({
            spk <- getAllKPITrends()
            diff <- getPeriodCompKPIs()
            if(is.null(spk) || is.null(diff)){
                return(NULL)
            }
            df <- inner_join( spk, diff)
            d1 <- datatable(
                df, 
                selection = list(
                    mode ="single",
                    target = "cell"
                    ), 
                rownames= FALSE,
                escape = FALSE,
                filter = list( 
                    position = 'top', 
                    clear = FALSE
                    ),
                options = list(
                    columnDefs = list(
                        list( 
                            className = 'dt-center',
                            targets = c(1:11)
                        )),
                    search = list(
                        regex = TRUE, 
                        caseInsensitive = FALSE
                    ),
                    fnDrawCallback = htmlwidgets::JS(
                    'function(){
                        HTMLWidgets.staticRender();
                    }')
                )
            )
            d1$dependencies <- append(d1$dependencies, htmlwidgets:::getDependency("sparkline"))
            d1
        })

        output$periodCompDT <- renderDT(
            getPeriodCompDT()
        )

        observeEvent(input$problemKPIs_cell_clicked, {
            kpi <- input$problemKPIs_cell_clicked$value
            if( is.null(kpi) || is.na(kpi) || !is.numeric(kpi) || input$problemKPIs_cell_clicked$col != 0){
                return(NULL)
            }
            updateSelectizeInput(session, "KPI", selected = kpi)
        })

        getDifferenceKPIs <- eventReactive(input$differences,{ 
            table <- differencePercentages(debug, getKeyMapping(), input$date_range[1], input$date_range[2], input$indGroup, input$indValue, input$server)
            table
        })

        getAllKPITrends <- reactive({
            sql <- kpiQueryAll(input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiData <- getKPIs(cs, sql)
            if(is.null(kpiData)){
                return(NULL)
            }
            allKPIS <- kpiData %>%
                group_by(ind_id) %>%
                summarize(
                    TrendSparkline = spk_chr(
                    ind_value, type ="line",
                    chartRangeMin = 0, chartRangeMax = max(ind_value)
                )
            )
            allKPIS
        })

        getDiffDT <- reactive({
            spk <- getAllKPITrends()
            diff <- getDifferenceKPIs()
            if(is.null(spk) || is.null(diff)){
                return(NULL)
            }
            df <- inner_join(  spk, diff)
            d1 <- datatable(
                df, 
                selection = list(
                    mode ="single",
                    target = "cell"
                    ), 
                rownames= FALSE,
                escape = FALSE,
                filter = list( 
                    position = 'top', 
                    clear = FALSE
                    ),
                options = list(
                    columnDefs = list(
                        list( 
                            className = 'dt-center',
                            targets = c(1:11)
                        )),
                    search = list(
                        regex = TRUE, 
                        caseInsensitive = FALSE
                    ),
                    fnDrawCallback = htmlwidgets::JS(
                    'function(){
                        HTMLWidgets.staticRender();
                    }')
                )
            )
            d1$dependencies <- append(d1$dependencies, htmlwidgets:::getDependency("sparkline"))
            d1
        })

        output$differenceKPIs <- renderDT(
            getDiffDT()
        )

        observeEvent(input$differenceKPIs_cell_clicked, {
            kpi <- input$differenceKPIs_cell_clicked$value
            if( is.null(kpi) || is.na(kpi) || !is.numeric(kpi) || input$differenceKPIs_cell_clicked$col != 0){
                return(NULL)
            }
            updateSelectizeInput(session, "KPI", selected = kpi)
        })

        observeEvent(input$periodCompDT_cell_clicked, {
            kpi <- input$periodCompDT_cell_clicked$value
            if( is.null(kpi) || is.na(kpi) || !is.numeric(kpi) || input$periodCompDT_cell_clicked$col != 0){
                return(NULL)
            }
            updateSelectizeInput(session, "KPI", selected = kpi)
        })

        observeEvent(input$problemKPIs_cell_clicked, {
            kpi <- input$problemKPIs_cell_clicked$value
            if( is.null(kpi) || is.na(kpi) || !is.numeric(kpi) || input$problemKPIs_cell_clicked$col != 0){
                return(NULL)
            }
            updateSelectizeInput(session, "KPI", selected = kpi)
        })

        getTrendAnomaly <- reactive({
            table <- trendAnomalySeries(debug, input$date_range[1], input$date_range[2], input$KPI, input$indValue, input$indGroup, input$server)
            table
        })

        getTrendChildAnomaly <- reactive({
            table <- trendAnomalySeries(debug, input$date_range[1], input$date_range[2], input$KPI, input$indValue, input$indGroup, input$server)
            table
        })

        output$trend <- renderPlotly({
            df <- getTrendAnomaly()
            label <- getLabels()
            trend(debug, df, label)
        })

        output$trendChild <- renderPlotly({
            df <- getTrendChildAnomaly()
            label <- getChildLabels()
            trend(debug, df, label)
        })

        getTrend <- reactive({
            sql <- kpiPeriodComparisionTrend(input$KPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)
            kpiTable
        })

        getTrendChild <- reactive({
            sql <- kpiPeriodComparisionTrend(input$childKPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)
            kpiTable
        })

        output$interactiveTrend <- renderPlotly({
            label <- getLabels()
            df <- getTrend()
            plot <- ggplotly(interactiveTrend(debug, df, label))
        })

        output$interactiveTrendChild <- renderPlotly({
            label <- getChildLabels()
            df <- getTrendChild()
            plot <- ggplotly(interactiveTrend(debug, df, label))
        })

        output$interactiveTable <- renderText({
            event.data <- event_data("plotly_selected", source = "subset")
            if (is.null(event.data) == T) return(NULL)
            stats <- subset(plot.df)[subset(event.data)$series_1]
            summary(stats)
        })

        getTrendComp <- reactive({
            sql <- kpiPeriodComparision(input$KPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)
            kpiTable
        })

        getTrendChildComp <- reactive({
            sql <- kpiPeriodComparision(input$childKPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)
            kpiTable
        })

          # Year over year comparison with different plotly style
        output$interactiveAggregate <- renderPlotly({
            df <- getTrendComp()
            label <- getLabels()
            comparisionTrend(debug, df , label)
        })

        output$interactiveAggregateChild <- renderPlotly({
            df <- getTrendChildComp()
            label <- getChildLabels()
            comparisionTrend(debug, df , label)
        })

        output$kpiPrediction <- renderPlotly({
            predictionTrend(debug, input$KPI, input$date_range[1], input$date_range[2], input$server)
        })

        getPredictedKPIs <- eventReactive(input$anomalies,{ 
            table <- predictedDifferences(debug, getKeyMapping(), input$date_range[1], input$date_range[2], input$indGroup, input$indValue, input$server)
            table
        })

        output$problemPredictionsKPIs <- renderDT(
            getPredictedKPIs(), 
            selection = list(target = 'cell'), 
            server = FALSE,
            filter = list( 
                position = 'top', 
                clear = FALSE),
            options = list(
                search = list(
                    regex = TRUE, 
                    caseInsensitive = FALSE
                ))
        )

        observeEvent( input$email, {
            log_event("Start email")
            df <- as.data.frame( getDifferenceKPIs())

            html <- convertToHTML(df, server = getServer(), group = getGrouping(), value = getVal(), start = getStartDt(), end = getEndDt())
            emailHTML(body = html)
            log_event("Email Sent")
        })

        onBookmark(function(state){
            state$values$server <- getServer()
            state$values$KPI <- getKPI()
            state$values$date_range_start <- getStartDt()
            state$values$date_range_end <- getEndDt()
            state$values$indGroup <- getGrouping()
            state$values$indValue <- getVal()
        })
        onRestore(function(state){
            req(input$server, input$date_range)
            updateSelectizeInput(session, "server", selected = state$values$server)
            updateDateRangeInput(session, "date_range", start = state$values$date_range_start)
            updateDateRangeInput(session, "date_range", end = state$values$date_range_end)
        
            req(input$indGroup, input$indValue)
            updateSelectizeInput(session, "indGroup", selected = state$values$indGroup)
            updateSelectizeInput(session, "indValue", selected = state$values$indValue)
        
            req(input$KPI)
            updateSelectizeInput(session, "KPI", selected = state$values$KPI)
        })
        #setBookmarkExclude("KPI")
        }
    )
}