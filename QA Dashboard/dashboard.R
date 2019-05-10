library(shiny)
library(shinyEventLogger)
library(shinydashboard)
library(dplyr)
library(tibbletime)
library(anomalize)
library(tidyverse)
library(plotly)
library(DT)





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
        
        output$test <- renderUI({
                return(
                    selectizeInput( 
                                "indValue", label = "Select Value",
                                choices = c(
                                    "Actual Period" = "1",
                                    "Actual YTD" = "2",
                                    "Budget Period" = "3",
                                    "Budget YTD" = "4"),
                                options = list(
                                    placeholder = 'Select a KPI Value'
                            ))
                )
            })


        }
    )
}