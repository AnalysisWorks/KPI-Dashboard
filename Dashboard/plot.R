require(shiny)
require(shinydashboard)
require(dplyr)

# Currently Not functioning
# may have to pass in functional closures
getPlot <- function(){
    output$plot1 <- renderPlot({
                data <- histdata[seq_len(input$slider)]
                hist(data)
            })
}