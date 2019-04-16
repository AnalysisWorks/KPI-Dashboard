library(visNetwork)
library(shiny)
library(shinydashboard)

options(shiny.host = "0.0.0.0")
options(shiny.port = 5050)
test_App <- function(site, program, nu) {

  shinyApp(
        myUI <- dashboardPage(
  # Title of Dashboard instance
            dashboardHeader(title = "Patient Flow Network"),


  # Side bar details
            dashboardSidebar(
                id = "sidebar",
                radioButtons(
                    "level",
                    label = "Scope",
                    choices = c(
                            "Between Sites" = "site_cd",
                            "Between Nursing Programs" = "nu_program_cd",
                            "Between Nursing Units" = "nu_cd"
                            )
                ),

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
                visNetworkOutput("network", width = "100%", height = "900px")
            )
        ),
        myServer <- function(input, output, session) {

          # If close button is selected, close the application
          observe({
            if (input$close > 0) stopApp()
          })
          output$network <- renderVisNetwork({

            if (input$level == "nu_cd") {
              nodes <- data.frame(id = c(unique(nu$nu_id)),
              #group = c(dataset$group, "Admission"),
              #value = c(dataset$group, "Admission"),
              #value = c(unique(dataset$to_start), "Admission"),
              #group = unique(c(dataset$to_start, dataset$group)),
                                            label = c(unique(nu$nu_id))
                                        )
              edges <- data.frame(from = nu$nu_to_start,
                                            to = nu$nu_to_end,
                                            label = nu$nu_cases
                                        )

              visNetwork(nodes, edges) %>%
                    visOptions(manipulation = TRUE,
                                highlightNearest = TRUE,
                                nodesIdSelection = TRUE
                                ) %>%
                    visPhysics(solver = "forceAtlas2Based") %>%
                    visEdges(arrows = "to") %>%
                    visInteraction(hover = TRUE) %>%
              #visHierarchicalLayout(direction = "LR", levelSeparation = 200, sortMethod = "directed") %>%
              visConfigure(enabled = TRUE, container = "sidebar") %>%
                    visEvents(hoverNode = "function(nodes) {
                        Shiny.onInputChange('current_node_id', nodes);
                    ;}")
            }
            else if (input$level == "nu_program_cd") {
              nodes <- data.frame(id = c(unique(program$nu_program_id)),
              #group = c(dataset$group, "Admission"),
              #value = c(dataset$group, "Admission"),
              #value = c(unique(dataset$to_start), "Admission"),
              #group = unique(c(dataset$to_start, dataset$group)),
                                            label = c(unique(program$nu_program_id))
                                        )
              edges <- data.frame(from = program$nu_program_to_start,
                                            to = program$nu_program_to_end,
                                            label = program$nu_program_cases
                                        )

              visNetwork(nodes, edges) %>%
                    visOptions(manipulation = TRUE,
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
            }
            else {
              nodes <- data.frame(id = c(unique(site$site_to_end)),
              #group = c(dataset$group, "Admission"),
              #value = c(dataset$group, "Admission"),
              #value = c(unique(dataset$to_start), "Admission"),
              #group = unique(c(dataset$to_start, dataset$group)),
                                            label = c(unique(site$site_to_end))
                                        )
              edges <- data.frame(from = site$site_to_start,
                                            to = site$site_to_end,
                                            label = site$site_cases
                                        )

              visNetwork(nodes, edges) %>%
                    visOptions(manipulation = TRUE,
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
            }

          })
        }
    )
}