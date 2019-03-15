library(shiny)
library(shinydashboard)
library(dplyr)
#source( "anomalies.R")
#source( "Database\\getKPIData.R")
library(anomalyDetection)
library(dplyr)
library(anomalize)


options(shiny.host = "0.0.0.0")
options(shiny.port = 5050)
anomalies <- function(dataset, mapping) {
    shinyApp(
        myUI <- dashboardPage(
            # Title of Dashboard instance
            dashboardHeader(
                title = "Database Anomalies",
                dropdownMenu(type = "messages",
                    messageItem(
                        from = "zwarnes",
                        message = "Initial KPI Automated QA."
                    ),
                    messageItem(
                        from = "zwarnes",
                        message = "Contact me to connect",
                        #icon = icon("question"),
                        time = "13:45"
                    ),                  
                    notificationItem(
                        text = "This application is in beta",
                        icon = icon("exclamation-triangle"),
                        status = "warning"
                    ),
                    notificationItem(
                        text = "The date picker does not work",
                        icon = icon("exclamation-triangle"),
                        status = "warning"
                    )
                )
            ),

            
            # Side bar details
            dashboardSidebar(
                id = "sidebar",
                #button to close window and end the session
                tags$button(
                    id = 'close',
                    type = "button",
                    class = "btn action-button",
                    onclick = "setTimeout(function(){window.close();},500);", 
                    "Close window"
                ),
                # Date slider, set to last 3 fiscal years, can be expanded if need be
                dateRangeInput("date_range", "Date range:",
                                start = "2017-04-01",
                                end   = "2018-03-31"
                ),
                actionButton("parent", "Parent KPI"),
                uiOutput("parent_checkbox"),

                numericInput("kpi", "Select Next KPI: ", 221, min = 0, max = 10000, step = 1, width = NULL),
                # Toggle to display field choices
                actionButton("show_checkbox", "Show Choices"),

                # Display the results of field choice, filtered, defined in server.R
                uiOutput("checkbox")    
            ),
            dashboardBody(
                tabItems(
                    tabItem( 
                        tabName = "Anomaly Detection",
                        tableOutput("Concept Design"),
                        uiOutput("CurrentKPI"),

                        fluidRow(
                            style = "padding-bottom: 80px;",
                            plotOutput('Trend', height = "400px")
                        )
                    ),
                    tabItem( 
                        tabName = "Period Comparison",
                        tableOutput("Concept Design"),
                        uiOutput("CurrentKPI"),

                        fluidRow(
                            style = "padding-bottom: 80px;",
                            plotOutput('Trend', height = "400px")
                        )
                    ),
                    tabItem( 
                        tabName = "General Quality Assurance",
                        tableOutput("Concept Design"),
                        uiOutput("CurrentKPI"),

                        fluidRow(
                            style = "padding-bottom: 80px;",
                            plotOutput('Trend', height = "400px")
                        )
                    )
                )
                #,uiOutput("plots")
            ) 
        ),
        myServer <- function(input, output, session) {

            # If close button is selected, close the application
            observe({
                if (input$close > 0) stopApp()
            })

            getData <- reactive({
                x <- dataset
            })

            # Filter dataset based on date slider input, if not date field is set, return all the data
            #filterByDate <- reactive({
            #    if( is.null( input$date_range)){ 
            #        return( getData() )
            #    }
            #    dataset <- filter( dataset, dataset[[Date]] >= input$date_range[1], dataset[[Date]] <= input$date_range[2])
            #})
            # Show checkbox based on field selected


            output$checkbox <- renderUI({
                if ( is.null(input$show_checkbox) ) {
                    return(NULL) 
                }
                if ( input$show_checkbox == 0 ) { 
                    return(NULL) 
                }
                children <- mapping %>%
                        filter( mapping$kpi == input$kpi)
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

            output$parent_checkbox <- renderUI({
                if ( is.null(input$parent) ) {
                    return(NULL) 
                }
                if ( input$parent == 0 ) { 
                    return(NULL) 
                }
                parent <- mapping %>%
                        filter( mapping$kpi == input$kpi)
                return( radioButtons( "parentKPI", 
                                        "Parent KPI: ", 
                                        choices = unique(parent$parent_kpi)
                                    ))
            })


             output$CurrentKPI <- renderUI({
                if ( is.null(input$childKPI) ) {
                    return( paste( "KPI Shown: ", input$kpi)) 
                }
                return( paste( "KPI Shown: ", input$childKPI))
            })




            # Filter dataset based on a particular date field by a selected range and by checkbox selection
            filterBySelection <- reactive({
                if ( is.null(input$show_checkbox) ) {
                    return( df <- dataset %>% 
                        filter( dataset$ind_id == input$kpi  )) 
                }
                if ( input$show_checkbox == 0 ) { 
                    return( df <- dataset %>% 
                        filter( dataset$ind_id == input$kpi  )) 
                }
                if( length(input$childKPI) == 0){
                    return( df <- dataset %>% 
                        filter( dataset$ind_id == input$kpi  )) 
                }
                #if( length( input$childKPI) == 1){
                df <- dataset %>% 
                    filter( dataset$ind_id == input$childKPI )
                #}
                #datasets <- list()
                #for( idx in 1:length(input$childKPI)){
                #    datasets[[idx]] <- dataset %>% 
                #        filter( dataset$ind_id == input$childKPI[[idx]] )
                #}
                #return( list( datasets))
            })



            output$Trend <- renderPlot(
                height = 400, {
                dataset <- filterBySelection()

                dfBase <- as_data_frame(dataset)
                dfBase %>%
                    time_decompose(ind_value_1, method = "twitter", trend = "1 month") %>%
                    anomalize(remainder, method = "gesd") %>%
                    time_recompose() %>%
                    # Anomaly Visualziation
                    plot_anomalies(time_recomposed = TRUE)
                    #plot_anomalies()
            })


            # Generate multiple plots
            #output$plots <- renderUI({
            #    plot_output_list <- lapply( filter(mapping,  mapping$kpi == input$kpi), function(i) {
            #            plotname <- paste0("plot", i)
            #            htmlOutput(plotname)
            #    })
#
            #    tagList(plot_output_list)
            #}) 
#
            ## Display Child KPIs
            #children <- mapping %>%
            #        filter( mapping$kpi == input$kpi)
#
            #for( child in 1:length(children$child_kpi)){
            #    local({
            #        my_i <- child
            #        plotname <- paste0("plot", my_i)
            #        df <- dataset %>%
            #            filter( dataset$ind_id == child )
            #        df <- as_data_frame(df)
#
            #        output[[plotname]] <- renderGvis({    
            #            df %>%
            #                time_decompose(ind_value_1, method = "twitter", trend = "2 months") %>%
            #                anomalize(remainder, method = "gesd") %>%
            #                time_recompose() %>%
            #                # Anomaly Visualziation
            #                plot_anomalies(time_recomposed = TRUE)
            #        })  
            #    })
            #}
        }
    )
}