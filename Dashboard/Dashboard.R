require(shiny)
require(shinydashboard)
require(dplyr)

dashboard <- function() {
    shinyApp(
        myUI <- dashboardPage(
            skin = "purple",
            getHeader(),
            getSidebar(),
            getBody()
        ),
        myServer <- function(input, output, session) {
            set.seed(122)
            histdata <- rnorm(500)

            output$plot1 <- renderPlot({
                data <- histdata[seq_len(input$slider)]
                hist(data)
            })

            output$plot2 <- renderPlot({
                data <- histdata[seq_len(input$slider)]
                hist(data)
            })

            output$progressBox <- renderInfoBox({
                infoBox(
                "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
                color = "purple"
                )
            })
            output$approvalBox <- renderInfoBox({
                infoBox(
                "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
                color = "yellow"
                )
            })

            output$progressBox2 <- renderValueBox({
                valueBox(
                paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
                color = "purple"
                )
            })

            output$approvalBox2 <- renderValueBox({
                valueBox(
                "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
                color = "yellow"
                )
            })
        }
    )
}