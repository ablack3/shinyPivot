library(shiny)
options(shiny.reactlog=T)  # command f3 + right arrow

moduleUI <- function(id){
     ns <- NS(id)
     tagList(actionButton(ns("button"), "Button"),
             verbatimTextOutput(ns("selected")))
}




module <- function(input, output, session, ns_id){
     ns <- NS(ns_id)
     # need to add namespace to newly created INPUT ids only
     my_input <- reactive(selectInput(ns("letters"), "letters", selected = input$letters, choices = letters, multiple = T))
     
     observeEvent(input$button, {
          # showModal(modalDialog(selectInput("letters", "letters", selected = selected_letters, choices = letters, multiple = T)))
          showModal(modalDialog(my_input(), easyClose = T))
     })
     
     output$selected <- renderPrint(input$letters)
     
}

ui <- fluidPage(
     moduleUI("id"),
     moduleUI("id2")
)

server <- function(input, output) {
     callModule(module, "id", ns_id = "id")
     callModule(module, "id2", ns_id = "id2")
}

shinyApp(ui = ui, server = server)

