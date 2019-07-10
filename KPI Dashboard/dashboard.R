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

            #############################################
            # META DATA SECTION
            # - Logging setup
            # - Parse URL arguments
            #
            #############################################
            if(debug){
                set_logging_session()
            }
            # Close dashboard app entirely
            observe({
                if (input$close > 0 && !is.na(input$close)) stopApp()
            })

            # Bookmarking currently not in use
            onBookmark(function(state){
            })
            onRestore(function(state){
            })

            observe({
                query <- parseQueryString(session$clientData$url_search)
                if (!is.null(query[['view']])) {
                    updateTabsetPanel(session, "chartTabs", selected = query[['view']])
                }
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
                    field <- as.data.frame(
                        getKPIs(
                            connectionString(Server = paste("aworks300", getServer(), sep = "\\"), Database = "LH_Indicators"),
                            kpiSelectOne(query[['kpi']])
                        ))
                    updateSelectizeInput(session,"KPI", choices = kpiFilter( term = as.character(field$group_field), getServer(), getGrouping(), debug), selected = setNames(as.list( query[['kpi']]), field$group_value))
                }
            })

            observeEvent(input$problemKPIs_cell_clicked, {
                kpi <- input$problemKPIs_cell_clicked$value
                if( is.null(kpi) || is.na(kpi) || !is.numeric(kpi) || input$problemKPIs_cell_clicked$col != 0){
                    return(NULL)
                }
                field <- as.data.frame(
                        getKPIs(
                            connectionString(Server = paste("aworks300", getServer(), sep = "\\"), Database = "LH_Indicators"),
                            kpiSelectOne(kpi)
                        ))
                updateSelectizeInput(session,"KPI", choices = kpiFilter( term = as.character(field$group_field), getServer(), getGrouping(), debug), selected = setNames(as.list( kpi), field$group_value))
            })

            #############################################
            # REACTIVE FUNCTIONS
            # - Misc Getter functions
            # - Database Calls
            #############################################

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

            getKPI <- reactive({
                kpi <- input$KPI
            })

            # Get KPI hierarchy mapping
            getKeyMapping <- reactive({
                req(input$server,input$indGroup)
                df <- as.data.frame(getKPIs(
                        connectionString(Server = paste("aworks300", getServer(), sep = "\\"), Database = "LH_Indicators"),
                        kpiGroupDetails(getGrouping())
                    ))
                df
            })

            getLabels <- reactive({
                req( input$KPI, input$indGroup, input$server)
                labels <- getChartLabels(kpi = getKPI(), server = getServer(), group = getGrouping(), debug = debug)
            })

            getDTData <- eventReactive(input$metric, {
                if( input$metric == "anomalies"){
                    return(statisticalAnomalies( debug, getKeyMapping(), getStartDt(), getEndDt(), getGrouping(), getVal(), getServer()))
                }
                if( input$metric == "differences"){
                    return(differencePercentages(debug, getKeyMapping(), getStartDt(), getEndDt(), getGrouping(), getVal(), getServer()))

                }
                if( input$metric == "periodComp"){
                    return(periodDifferencePercentages(debug, getKeyMapping(), getStartDt(), getEndDt(), getGrouping(), getVal(), getServer()))
                }
            })

            getDTTable <- reactive({
                spk <- getAllKPITrends()
                diff <- getDTData()
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

            getAllKPITrends <- reactive({
                sql <- kpiQueryAll(getStartDt(), getEndDt(), getGrouping(), getVal())
                cs <- connectionString(Server = paste("aworks300", getServer(), sep = "\\"), Database = "LH_Indicators")
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

            getTrendAnomaly <- reactive({
                req(input$server,input$indGroup, input$KPI)
                table <- trendAnomalySeries(debug, getStartDt(), getEndDt(), getKPI(), getVal(), getGrouping(), getServer())
                table
            })

            getTrend <- reactive({
                req(input$server,input$indGroup, input$KPI)
                sql <- kpiPeriodComparisionTrend(getKPI(), getStartDt(), getEndDt(), getGrouping(), getVal())
                cs <- connectionString(Server = paste("aworks300", getServer(), sep = "\\"), Database = "LH_Indicators")
                kpiTable <- kpiTable(cs, sql)
                kpiTable
            })

            getTrendComp <- reactive({
                req(input$server, input$indGroup, input$KPI)
                sql <- kpiPeriodComparision(getKPI(), getStartDt(), getEndDt(), getGrouping(), getVal())
                cs <- connectionString(Server = paste("aworks300", getServer(), sep = "\\"), Database = "LH_Indicators")
                kpiTable <- kpiTable(cs, sql)
                kpiTable
            })


            limitKPIs <- reactive({
                return( kpiFilter( term = input$indFields, getServer(), getGrouping(), debug))
            })

            observe({
                updateSelectInput(session,"KPI",choices = limitKPIs())
            })
            #############################################
            # UI - USER INPUTS
            #############################################

            output$KPIGroups <- renderUI({
                req(input$server)
                return(kpiGroupsUI(inputId = "indGroup", id = "KPIGroups", server = getServer(), kpiType = getGrouping(), debug = debug))
            })

            output$KPIValues <- renderUI({
                req(input$server, input$indGroup)
                return(kpiValueUI(inputId = "indValue", id = "KPIValues", server = getServer(), getGrouping(), debug))
            })

            output$KPIout <- renderUI({
                req(input$server, input$indGroup)
                return(kpiUI(inputId = "KPI", id = "KPIout", "Select Value",  server = getServer(), group = getGrouping(), getKPI(), debug))
            })

            output$KPIfields <- renderUI({
                req(input$server, input$indGroup)
                return(kpiFieldsUI(inputId = "indFields", id = "KPIfields", server = getServer(), group = getGrouping(), debug = debug))
            })
            #############################################
            # UI - VALUE BOXES
            #############################################

            output$currentLastAssemble <- renderValueBox({
                req(input$server, input$KPI, input$indGroup, input$indValue)
                valueBox(
                    subtitle = "Last date in current assembly",
                    getKPILastDate(debug, getKPI(), getGrouping(), getVal(), getServer()),
                    icon = icon("credit-card"),
                    color = "purple"
                )
            })
            output$currentAverageBox <- renderValueBox({
                req(input$server, input$KPI, input$date_range, input$indGroup, input$indValue)
                valueBox(
                    subtitle = "Current assembly Average",
                    getKPIAverage(debug, getKPI(), getStartDt(), getEndDt(), getGrouping(), getVal(), getServer()),
                    icon = icon("credit-card"),
                    color = "purple"
                )
            })

            #############################################
            # UI - DATA TABLES
            #############################################

            output$problemKPIs <- renderDT(
                getDTTable()
            )

            #############################################
            # UI - CHARTS
            #############################################

            output$plot <- renderPlotly({
                req(input$KPI, input$indGroup, getLabels())
                label <- getLabels()
                if( input$chartTabs == 'anomaly_detection'){
                    return(statisticalAnomalyTrend(debug, getTrendAnomaly(), label))
                }
                if( input$chartTabs == 'period_comparison_trend'){
                    return(ggplotly(yearComparisonTrend(debug, getTrend(), label)))
                }
                if( input$chartTabs == 'period_comparison_interval_trend'){
                    return(cycleComparisonTrend(debug, getTrendComp(), label))
                }
            })
        }
    )
}
