library(shiny)
library(shinyEventLogger)
library(shinydashboard)
library(dplyr)
library(tibbletime)
library(anomalize)
library(tidyverse)
library(plotly)
library(DT)

kpiValueFunction <- function( input, output, session, server, parsedArgs, group){

    if(debug){
        log_event("Start KPIValues")
    }
    if(is.null(server) || is.null(group)){
        if(debug){
            log_event(paste( "KPIGroups Server", server, "KPI Group", group, sep = ": "))
    }  
        return(NULL)
    }

    if(server == 'islandhealth' || server == 'vancouvercoastal'){
    if(group == 'transfers'){
        if(debug){
            log_event("Return KPIValues")
        }
        if(!is.null(parsedArgs[['kpiType']])){
            return( selectizeInput( 
                    "indValue", label = "Select Value",
                    choices = c(
                        "Primary Value" = "1",
                        "Admits" = "2",
                        "Discharges" = "3",
                        "Transfers" = "4",
                        "Other" = "5"),
                    selected = parsedArgs[['kpiValue']],
                    options = list(
                        placeholder = 'Select a KPI Value'
                )))
        }
        return( selectizeInput( 
                    "indValue", label = "Select Value",
                    choices = c(
                        "Primary Value" = "1",
                        "Admits" = "2",
                        "Discharges" = "3",
                        "Transfers" = "4",
                        "Other" = "5"),
                    options = list(
                        placeholder = 'Select a KPI Value'
                )))
    }
    if(group == 'financials'){
            if(debug){
            log_event("Return KPIValues")
        }
        if(!is.null(parsedArgs[['kpiType']])){
            return( selectizeInput( 
                    "indValue", label = "Select Value",
                    choices = c(
                        "Actual Period" = "1",
                        "Actual YTD" = "2",
                        "Budget Period" = "3",
                        "Budget YTD" = "4"),
                    selected = parsedArgs[['kpiValue']],
                    options = list(
                        placeholder = 'Select a KPI Value'
                )))
        }
        return( selectizeInput( 
                    "indValue", label = "Select Value",
                    choices = c(
                        "Actual Period" = "1",
                        "Actual YTD" = "2",
                        "Budget Period" = "3",
                        "Budget YTD" = "4"),
                    options = list(
                        placeholder = 'Select a KPI Value'
                )))
    }
    else{
        if(debug){
            log_event("Return Other Islandhealth KPIValues")
        }
        if(!is.null(parsedArgs[['kpiType']])){
            return( selectizeInput( 
                    "indValue", label = "Select Value",
                    choices = c(
                        "Primary Value" = "1"),
                    selected = parsedArgs[['kpiValue']],
                    options = list(
                        placeholder = 'Select a KPI Value'
                )))
        }
        return(selectizeInput(
                "indValue", label = "Select Value", 
                choices = c("Primary Value" = "1"),
                options = list(
                        placeholder = 'Select a KPI Value'
                )))
    }
    }
    else{
        if(debug){
            log_event("Return All Other KPIValues")
        }
        if(!is.null(parsedArgs[['kpiType']])){
            return( selectizeInput( 
                    "indValue", label = "Select Value",
                    choices = c("Primary Value" = "1"),
                    selected = parsedArgs[['kpiValue']],
                    options = list(
                        placeholder = 'Select a KPI Value'
                )))
        }
        return(selectizeInput("indValue",label = "Select Value",
                choices = c("Primary Value" = "1"),
                options = list(
                        placeholder = 'Select a KPI Value'
                )))
    }

}

dashboard <- function(debug = FALSE) {
  #, mapping
  if(debug){
      set_logging()
  }
  shinyApp(
        myUI <- dashboardPage(
            skin = "purple",
            getHeader(),
            getSidebar(),
            getBody()
        ),
        myServer <- function(input, output, session) {
          if(debug){
             set_logging_session()
        }
          # Close dashboard app entirely
          observe({
            if (input$close > 0 && !is.na(input$close)) stopApp()
          })
        observe({
            query <- parseQueryString(session$clientData$url_search)
            if (!is.null(query[['server']])) {
                updateSelectizeInput(session, "server", selected = query[['server']])
            }
            if (!is.null(query[['kpi']])) {
                updateSelectizeInput(session, "KPI", selected = query[['kpi']])
            }
            if (!is.null(query[['kpiType']])) {
                updateSelectizeInput(session, "indGroup", selected = query[['kpiType']])
            }
            if (!is.null(query[['kpiValue']])) {
                updateSelectizeInput(session, "indValue", selected = query[['kpiValue']])
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
              if(debug){
                  log_event("Start KPIGroups")
              }
              if(is.null(input$server)){
                  if(debug){
                  log_event(paste( "KPIGroups Server", input$server, sep = ": "))
                }   
                  return(NULL)
              }
              query <- parseQueryString(session$clientData$url_search)
              if(input$server == 'islandhealth' || input$server == 'vancouvercoastal'){
                  if (!is.null(query[['kpiType']])) {
                      return( selectizeInput(
                                "indGroup", label = "KPI",
                                choices = c(
                                        "Bed Utilization" = "Beds",
                                        "ALC Utilization" = "ALC Beds",
                                        "ADT Events" = "transfers"),
                                selected = query[['kpiType']],
                                options = list(
                                    placeholder = 'Select a KPI type'
                            )))
                    }
                  if(input$server == 'islandhealth'){
                    return( selectizeInput(
                                "indGroup", label = "KPI",
                                choices = c(
                                        "Bed Utilization" = "Beds",
                                        "ALC Utilization" = "ALC Beds",
                                        "ADT Events" = "transfers",
                                        "Financials" = "financials"),
                                selected = query[['kpiType']],
                                options = list(
                                    placeholder = 'Select a KPI type'
                            )))
                  }
                  return( selectizeInput(
                            "indGroup", label = "KPI",
                            choices = c(
                                    "Bed Utilization" = "Beds",
                                    "ALC Utilization" = "ALC Beds",
                                    "ADT Events" = "transfers"),
                            options = list(
                                placeholder = 'Select a KPI type'
                            )))
              }else{
                  if (!is.null(query[['kpiType']])) {
                      return( selectizeInput(
                                "indGroup", label = "KPI",
                                choices = c("Bed Utilization" = "Beds"),
                                selected = query[['kpiType']],
                                options = list(
                                    placeholder = 'Select a KPI type'
                            )))
                    }
                  return( selectizeInput( 
                            "indGroup", label = "KPI", 
                            choices = c("Bed Utilization" = "Beds"),
                            options = list(
                                placeholder = 'Select a KPI type'
                            )))
              }
          })

          output$KPIValues <- renderUI({
              if(debug){
                  log_event("Start KPIValues")
              }
              if(is.null(input$server) || is.null(input$indGroup)){
                  if(debug){
                  log_event(paste( "KPIGroups Server", input$server, "KPI Group", input$indGroup, sep = ": "))
              }  
                  return(NULL)
              }

              query <- parseQueryString(session$clientData$url_search)
              if(input$server == 'islandhealth' || input$server == 'vancouvercoastal'){
                if(input$indGroup == 'transfers'){
                    if(debug){
                        log_event("Return KPIValues")
                    }
                    if(!is.null(query[['kpiType']])){
                        return( selectizeInput( 
                                "indValue", label = "Select Value",
                                choices = c(
                                    "Primary Value" = "1",
                                    "Admits" = "2",
                                    "Discharges" = "3",
                                    "Transfers" = "4",
                                    "Other" = "5"),
                                selected = query[['kpiValue']],
                                options = list(
                                    placeholder = 'Select a KPI Value'
                            )))
                    }
                    return( selectizeInput( 
                                "indValue", label = "Select Value",
                                choices = c(
                                    "Primary Value" = "1",
                                    "Admits" = "2",
                                    "Discharges" = "3",
                                    "Transfers" = "4",
                                    "Other" = "5"),
                                options = list(
                                    placeholder = 'Select a KPI Value'
                            )))
                }
                if(input$indGroup == 'financials'){
                     if(debug){
                        log_event("Return KPIValues")
                    }
                    if(!is.null(query[['kpiType']])){
                        return( selectizeInput( 
                                "indValue", label = "Select Value",
                                choices = c(
                                    "Actual Period" = "1",
                                    "Actual YTD" = "2",
                                    "Budget Period" = "3",
                                    "Budget YTD" = "4"),
                                selected = query[['kpiValue']],
                                options = list(
                                    placeholder = 'Select a KPI Value'
                            )))
                    }
                    return( selectizeInput( 
                                "indValue", label = "Select Value",
                                choices = c(
                                    "Actual Period" = "1",
                                    "Actual YTD" = "2",
                                    "Budget Period" = "3",
                                    "Budget YTD" = "4"),
                                options = list(
                                    placeholder = 'Select a KPI Value'
                            )))
                }
                else{
                    if(debug){
                        log_event("Return Other Islandhealth KPIValues")
                    }
                    if(!is.null(query[['kpiType']])){
                        return( selectizeInput( 
                                "indValue", label = "Select Value",
                                choices = c(
                                    "Primary Value" = "1"),
                                selected = query[['kpiValue']],
                                options = list(
                                    placeholder = 'Select a KPI Value'
                            )))
                    }
                    return(selectizeInput(
                            "indValue", label = "Select Value", 
                            choices = c("Primary Value" = "1"),
                            options = list(
                                    placeholder = 'Select a KPI Value'
                            )))
                }
              }
              else{
                  if(debug){
                        log_event("Return All Other KPIValues")
                    }
                  if(!is.null(query[['kpiType']])){
                        return( selectizeInput( 
                                "indValue", label = "Select Value",
                                choices = c("Primary Value" = "1"),
                                selected = query[['kpiValue']],
                                options = list(
                                    placeholder = 'Select a KPI Value'
                            )))
                    }
                  return(selectizeInput("indValue",label = "Select Value",
                            choices = c("Primary Value" = "1"),
                            options = list(
                                    placeholder = 'Select a KPI Value'
                            )))
              }
          })

            output$KPI <- renderUI({
                if(debug){
                  log_event("Start KPI")
                }
                if(is.null(input$server) || is.null(input$indGroup)){
                    if(debug){
                    log_event(paste("KPI server:", input$server, "KPI Group:", input$indGroup, sep = " "))
                }     
                  return(NULL)
                }
                sql <- kpiDetailsAll(input$indGroup)
                cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
                kpis <- getKPIs(cs, sql)
                if(debug){
                    log_event( "Return KPIs")
                }
                query <- parseQueryString(session$clientData$url_search)
                if (!is.null(query[['kpi']])) {
                     return(
                        selectizeInput(
                            "KPI", "Current KPI", 
                            choices = unique(kpis$ind_id),
                            selected = query[['kpi']]
                        )
                    )
                }
                return(
                    selectizeInput(
                        "KPI", "Current KPI", 
                        choices = unique(kpis$ind_id)
                    )
                )
            })
          # Get KPI hierarchy mapping
          getMapping <- reactive({
              if(debug){
                  log_event("Start getMapping")
              }
              if(is.null(input$server) || is.null(input$indGroup)){
                   if(debug){
                  log_event(paste("KPI server:", input$server, "KPI Group:", input$indGroup, sep = " "))
                   }
                   return(NULL)
              }     
            df <- as.data.frame(getKPIs(
                    connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators"),
                    kpiMapping(input$indGroup)
                ))
            if(debug){
                  log_event("Finished getMapping")
             }
             df
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

          getLabels <- reactive({
              if(debug){
                  log_event("Start getLabels")
              }
            if (is.null(input$KPI) ) {
                if(debug){
                  log_event(paste("getLabels KPI", input$KPI, sep = ": "))
              }
              return(NULL)
            }
            sql <- kpiDetails(input$KPI ,input$indGroup)
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
            if(debug){
                  log_event("Finish getLabels")
              }
            title
          })

          getChildLabels <- reactive({
              if(debug){
                  log_event("Start getChildLabels")
              }
            if (is.null(input$childKPI) ) {
                if(debug){
                  log_event(paste("getChildLabels KPI", input$childKPI, sep = ": "))
              }
              return(NULL)
            }
            sql <- kpiDetails(input$childKPI, input$indGroup)
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
            if(debug){
                  log_event("Finish getChildLabels")
              }
            title
          })


          # Display available child KPIs based off current selection
          output$childKPIs <- renderUI({
              if(debug){
                  log_event("Start childKPIs")
              }
            mapping <- getMapping()
            if(is.null(mapping)){
               if(debug){
                  log_event("childKPIs call to mapping returned NULL")
              } 
              return(NULL)
            }
            if(is.null(input$server) || is.null(input$KPI)){
                if(debug){
                    log_event(paste("childKPIs server", input$server, "KPI", input$KPI, sep = ": "))
                }
                return(NULL)
            }
            children <- mapping %>%
                        filter(mapping$kpi == input$KPI)
            if (all(is.na(children$child_group_value_2))) {
              if(debug){
                  log_event("Return childKPIs 1")
              }
              return(selectizeInput("childKPI",
                                        "Child KPIs:",
                                        choices = setNames(as.list( children$child_kpi), paste( children$child_group_field_1, children$child_group_value_1, sep = ": "))
                                    )
                        )
            }
            if (all(is.na(children$child_group_value_3))) {
                if(debug){
                  log_event("Return childKPIs 2")
              }
              return(selectizeInput("childKPI",
                                        "Child KPIs:",
                                        choices = setNames(as.list(children$child_kpi), paste(children$child_group_field_2, children$child_group_value_2, sep = ": "))
                                    )
                        )
            }
            if (all(is.na(children$child_group_value_4))) {
                if(debug){
                  log_event("Return childKPIs 3")
              }
              return(selectizeInput("childKPI",
                                        "Child KPIs:",
                                        choices = setNames(as.list(children$child_kpi), paste( children$child_group_field_3, children$child_group_value_3, sep = ": "))
                                    )
                        )
            }
            if(debug){
                  log_event("Return childKPIs 4")
              }
            return(selectizeInput("childKPI",
                                        "Child KPIs:",
                                        choices = setNames(as.list(children$child_kpi), paste( children$child_group_field_4, children$child_group_value_4, sep = ": "))
                                    )
                        )
          })

          getChildKPI <- reactive({
              if(debug){
                  log_event("Start getChildKPI")
              }
              if(is.null(input$server) || is.null(input$childKPI)){
                    if(debug){
                       log_event(paste("getChildKPI server",input$server,"childKPI", input$childKPI, sep = ": "))
                    }   
                  return(NULL)
              }
            mapping <- getMapping()
            if(is.null(mapping)){
               if(debug){
                  log_event("getChildKPI call to mapping returned NULL")
              } 
              return(NULL)
            }
            currentKPI <- mapping %>%
                        filter(mapping$kpi == input$childKPI)
            childKPI <- unique(currentKPI$kpi)
            if(debug){
                  log_event("Finish getChildKPI")
              }
              childKPI
          })

          getParentKPI <- reactive({
              if(debug){
                  log_event("Start getParentKPI")
              }
              if(is.null(input$server) || is.null(input$KPI)){
                  if(debug){
                       log_event(paste("getParentKPI server",input$server,"KPI", input$KPI, sep = ": "))
                    }   
                  return(NULL)
              }
            mapping <- getMapping()
            if(is.null(mapping)){
               if(debug){
                  log_event("getParentKPI call to mapping returned NULL")
              } 
              return(NULL)
            }
            currentKPI <- mapping %>%
                        filter(mapping$kpi == input$KPI)
            parentKPI <- unique(currentKPI$parent_kpi)
            if(debug){
                  log_event("Finish getParentKPI")
              }
            parentKPI
          })

          output$childKPI <- renderText({
              if(debug){
                  log_event("Start childKPI")
              }
              
              if(is.null(input$server)){
                  return(NULL)
              }
            kpi <- getChildKPI()
            if(is.null(kpi)){
               if(debug){
                  log_event("childKPI call to getChildKPI returned NULL")
              } 
              return(NULL)
            }
            if(debug){
                  log_event("Finish childKPI")
            }
            kpi <- paste("Child KPI: ", kpi, sep = "")
            kpi
          })

          output$parentKPI <- renderText({
              if(debug){
                  log_event("Start parentKPI")
              }
              if(is.null(input$server)){
                  return(NULL)
              }
            kpi <- getParentKPI()
            if(is.null(kpi)){
               if(debug){
                  log_event("parentKPI call to getParentKPI returned NULL")
              } 
              return(NULL)
            }
            if(debug){
                  log_event("Finish parentKPI")
            }
            kpi <- paste("Parent KPI: ", kpi, sep = "")
          })

    
          getProblemKPIs <- eventReactive(input$anomalies,{ 
                kpi_ind_ids <- getKeyMapping()
                ind_ids <- cbind(unique(kpi_ind_ids$kpi))
                date_range <- as.data.frame(seq(as.Date(input$date_range[1]), as.Date(input$date_range[2]), "days"))
                names(date_range) <- c("Date")

                anomaly.counts <- data.frame(matrix(ncol = 2, nrow = 0))
                names(anomaly.counts) <- c('ind_id', 'anomalies')
                for (i in 1:length(ind_ids)) {
                    sql <- kpiQuery(ind_ids[[i]], input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
                    cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
                    df <- as_data_frame(kpiTable(cs, sql))

                    if (nrow(df) < 10) {
                        next
                    }
                    df <- left_join(date_range, df)
                    df$ind_id <- ind_ids[[i]]
                    df[is.na(df)] <- 0
                    df <- as_data_frame(df)

                    result <- tryCatch({
                        anomalies <- df %>%
                                    time_decompose(ind_value, method = "twitter", trend = "3 months") %>%
                                    anomalize(remainder, method = "gesd") %>%
                                    time_recompose()
                        }, error = function(e) {
                            NULL
                        }, finally = {
                            NULL
                        })
                    if( !is.null(result)){
                        anomaly.counts[i,] <- list(ind_ids[[i]], sum(anomalies$anomaly == 'Yes'))
                    }
                }
                sql <- kpiDetailsAll(input$indGroup)
                cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
                labels <- getKPIs(cs, sql)
                qa_table <- inner_join(anomaly.counts[order(-anomaly.counts$anomalies),], labels)
          })

          output$problemKPIs <- renderDT(
                getProblemKPIs(), 
                selection = list(target = 'cell'), 
                server = FALSE,
                filter = list( 
                    position = 'top', 
                    clear = FALSE),
                options = list(
                    search = list(
                        regex = TRUE, 
                        caseInsensitive = FALSE
                    )
                )
            )



          # Anomaly Trends
          getTrendAnomaly <- reactive({
              if(debug){
                  log_event("Start getTrendAnomaly")
              }
              if(is.null(input$server) || is.null(input$KPI)){
                  if(debug){
                    log_event(paste("getTrendAnomaly server", input$server, "KPI", input$KPI, sep = ": "))
                }
                return(NULL)
              }
            sql <- kpiQuery(input$KPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)
            

            # padd zero days
            date_range <- as.data.frame(seq(as.Date(input$date_range[1]), as.Date(input$date_range[2]), "days"))
            names(date_range) <- c("Date")
            kpiTable <- left_join(date_range, kpiTable)
            kpiTable[ is.na(kpiTable)] <- 0
            kpiTable[3] <- input$KPI

            sql <- kpiQuery(input$KPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            df <- kpiTable(cs, sql)
            df <- left_join(date_range, df)

            df[is.na(df)] <- 0
            df[3] <- input$KPI
            df <- as_data_frame(df)


            anomalies <- df %>%
                    time_decompose(ind_value, method = "twitter", trend = "3 months") %>%
                    anomalize(remainder, method = "gesd") %>%
                    time_recompose()
            kpiTable <- left_join(kpiTable, anomalies)
            if(debug){
                  log_event("Finish getTrendAnomaly")
              }
            kpiTable
          })

          getTrendChildAnomaly <- reactive({
              if(debug){
                  log_event("Start getTrendChildAnomaly")
              }
              if(is.null(input$server) || is.null(input$KPI)){
                  if(debug){
                    log_event(paste("getTrendChildAnomaly server", input$server, "KPI", input$KPI, sep = ": "))
                }
                  return(NULL)
              }
              sql <- kpiQuery(input$childKPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
              cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
              kpiTable <- kpiTable(cs, sql)

              # padd zero days
                date_range <- as.data.frame(seq(as.Date(input$date_range[1]), as.Date(input$date_range[2]), "days"))
                names(date_range) <- c("Date")
                kpiTable <- left_join(date_range, kpiTable)
                kpiTable[ is.na(kpiTable)] <- 0
                kpiTable[3] <- input$childKPI

              sql <- kpiQuery(input$childKPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
              cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
              df <- kpiTable(cs, sql)
              
              df <- left_join(date_range, df)            
              df[is.na(df)] <- 0
              df[3] <- input$KPI
              df <- as_data_frame(df)


              anomalies <- df %>%
                    time_decompose(ind_value, method = "twitter", trend = "3 months") %>%
                    anomalize(remainder, method = "gesd") %>%
                    time_recompose()
              kpiTable <- left_join(kpiTable, anomalies)
              if(debug){
                  log_event("Finish getTrendChildAnomaly")
              }
              kpiTable
          })


          output$trend <- renderPlotly({
              df <- getTrendAnomaly()
              if(is.null(df)){
                  if(debug){
                      log_event("trend call to getTrendAnomaly returned NULL")
                  }
                  return(NULL)
              }
              label <- getLabels()
              if(is.null(label)){
                  if(debug){
                      log_event("trend call to getLabels returned NULL")
                  }
                  return(NULL)
              }
              p <- plot_ly(
                        df,
                        x = ~Date,
                        y = ~ind_value,
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
                        yaxis = list(title = 'Value',
                        rangemode = "tozero")
                    )
              p
              mytext = paste("Current Value = ", df$ind_value, sep = "")
              pp = plotly_build(p)
              style(pp, text = mytext, hoverinfo = "text", traces = c(1, 1))
            
          })

          output$trendChild <- renderPlotly({
              df <- getTrendChildAnomaly()
              if(is.null(df)){
                  if(debug){
                      log_event("trend call to getTrendChildAnomaly returned NULL")
                  }
                  return(NULL)
              }
              label <- getChildLabels()
              if(is.null(label)){
                  if(debug){
                      log_event("trend call to getChildLabels returned NULL")
                  }
                  return(NULL)
              }
              p <- plot_ly(
                        df,
                        x = ~Date,
                        y = ~ind_value,
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
                        yaxis = list(title = 'Value',
                        rangemode = "tozero")
                    )
              p
              mytext = paste("Current Value = ", df$ind_value, sep = "")
              pp = plotly_build(p)
              style(pp, text = mytext, hoverinfo = "text", traces = c(1, 1))
            
          })


          # Year over year series comparison

          getTrend <- reactive({
            sql <- kpiPeriodComparisionTrend(input$KPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)
            kpiTable
            # padd zero days
            #date_range <- as.data.frame(seq(as.Date(input$date_range[1]), as.Date(input$date_range[2]), "days"))
            #names(date_range) <- c("Date")
            #kpiTable <- left_join(date_range, kpiTable)
            #kpiTable[ is.na(kpiTable)] <- 0
            #kpiTable[4] <- input$KPI
          })

          getTrendChild <- reactive({
            sql <- kpiPeriodComparisionTrend(input$childKPI, input$date_range[1], input$date_range[2], input$indGroup, input$indValue)
            cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
            kpiTable <- kpiTable(cs, sql)
            kpiTable

            # padd zero days
            #date_range <- as.data.frame(seq(as.Date(input$date_range[1]), as.Date(input$date_range[2]), "days"))
            #names(date_range) <- c("Date")
            #kpiTable <- left_join(date_range, kpiTable)
            #kpiTable[ is.na(kpiTable)] <- 0
            #kpiTable[4] <- input$childKPI
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
                        yaxis = list(title = 'Value',
                        rangemode = "tozero")
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
                        yaxis = list(title = 'Value',
                        rangemode = "tozero")
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
                        title = "Volumes",
                        rangemode = "tozero"
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
                        title = "Volumes",
                        rangemode = "tozero"
                    )
                )
          })

          output$kpiPrediction <- renderPlotly({
              trainSQL <- kpiTrainData(input$KPI, input$date_range[1], input$date_range[2])
              cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
              train <- getKPIs(cs, trainSQL)

              testSQL <- kpiTestData(input$KPI, input$date_range[1], input$date_range[2])
              cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
              test <- getKPIs(cs, testSQL)

              prediction <- xgboostPrediction(train, test)
              err <- prediction$err
              pred <- prediction$pred
              test$prediction <- pred

              p <- test %>%
                plot_ly(
                    x = ~Date,
                    y = ~ind_value_1,
                    type = "scatter",
                    mode = 'lines',
                    fill = 'tozeroy',
                    name = "Current Values"
                ) %>%
                add_trace(
                    y = ~prediction,
                    name = 'Predicted Values',
                    mode = 'lines',
                    fill = 'tozeroy'
                ) %>%          
                layout(
                    title = "<b>Predicted Data</b><br>test phase",
                    xaxis = list(
                        type = 'date'
                    ),
                    yaxis = list(
                        title = "Volumes",
                        rangemode = "tozero"
                    )
                )   
          })

        getPredictedKPIs <- eventReactive(input$anomalies,{ 
                kpi_ind_ids <- getKeyMapping()
                ind_ids <- cbind(unique(kpi_ind_ids$kpi))
                date_range <- as.data.frame(seq(as.Date(input$date_range[1]), as.Date(input$date_range[2]), "days"))
                names(date_range) <- c("Date")

                anomaly.error <- data.frame(matrix(ncol = 2, nrow = 0))
                names(anomaly.error) <- c('ind_id', 'error')
                for (i in 1:length(ind_ids)) {
                    trainSQL <- kpiTrainData(input$KPI, input$date_range[1], input$date_range[2])
                    cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
                    train <- getKPIs(cs, trainSQL)

                    testSQL <- kpiTestData(input$KPI, input$date_range[1], input$date_range[2])
                    cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
                    test <- getKPIs(cs, testSQL)

                    result <- tryCatch({
                        prediction <- xgboostPrediction(train, test)
                        }, error = function(e) {
                            NULL
                        }, finally = {
                            NULL
                        })
                    if( !is.null(result)){
                        err <- prediction$err
                        anomaly.error[i,] <- list(ind_ids[[i]], err)
                    }
                }
                sql <- kpiDetailsAll(input$indGroup)
                cs <- connectionString(Server = paste("aworks300", input$server, sep = "\\"), Database = "LH_Indicators")
                labels <- getKPIs(cs, sql)
                qa_table <- inner_join(anomaly.error[order(-anomaly.error$error),], labels)
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
                    )
                )
            )
        }
    )
}