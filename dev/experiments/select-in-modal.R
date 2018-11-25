library(shiny)

ui <- fluidPage(
     actionButton("button", "Button"),
     verbatimTextOutput("selected")
)

server <- function(input, output, session) {
     # my_input <- reactive(selectInput("letters", "letters", selected = input$letters, choices = letters, multiple = T))
     my_input <- reactive(selectInput("letters", "letters", choices = character(0), multiple = T))

     updateSelectizeInput(session, "letters", "letters", choices = letters, server = T)

     observeEvent(input$button, {
          showModal(modalDialog(my_input()))
     })

     output$selected <- renderPrint(input$letters)
}

shinyApp(ui = ui, server = server)
