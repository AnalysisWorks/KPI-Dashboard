library(visNetwork)
library(shiny)
library(shinydashboard)

options(shiny.host = "0.0.0.0")
options(shiny.port = 5050)
test_App <- function(dataset) {

    shinyApp(
        myUI <- dashboardPage(
            # Title of Dashboard instance
            dashboardHeader(title = "Patient Flow Network"),
            
            
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
                )     
            ),
            dashboardBody(
                visNetworkOutput("network", width = "100%", height = "600px")
            )
        ),
        myServer <- function(input, output, session) {

            # If close button is selected, close the application
            observe({
                if (input$close > 0) stopApp()
            })
            output$network <- renderVisNetwork({
                # minimal example
                nodes <- data.frame(    id = c(unique(dataset$id), "Admission", "Discharge"),
                                        #group = c(dataset$group, "Admission"),
                                        #value = c(dataset$group, "Admission"),
                                        #value = c(unique(dataset$to_start), "Admission"),
                                        #group = unique(c(dataset$to_start, dataset$group)),
                                        label = c(unique(dataset$id), "Admission", "Discharge")
                                    )
                edges <- data.frame(    from = dataset$to_start,
                                        to = dataset$to_end,
                                        label = dataset$cases
                                    )
                
                visNetwork(nodes, edges) %>%
                visOptions( manipulation = TRUE,
                            highlightNearest = TRUE,
                            nodesIdSelection = TRUE
                            ) %>%
                visEdges(arrows = "to") %>% 
                visInteraction(hover = TRUE) %>%
                #visHierarchicalLayout(direction = "LR", levelSeparation = 200, sortMethod = "directed") %>%
                visConfigure(enabled = TRUE, container = "sidebar") %>%
                visEvents(hoverNode = "function(nodes) {
                    Shiny.onInputChange('current_node_id', nodes);
                ;}")
            })
        }
    )
}