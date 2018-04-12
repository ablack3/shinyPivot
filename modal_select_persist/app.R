library(shiny)
options(shiny.reactlog=T) 

ui <- fluidPage(
     actionButton("button", "Button"),
     verbatimTextOutput("selected")
     )

server <- function(input, output) {
     # selected_letters <- reactiveValues()
     my_input <- reactive(selectInput("letters", "letters", selected = input$letters, choices = letters, multiple = T))
     
     observeEvent(input$button, {
               # showModal(modalDialog(selectInput("letters", "letters", selected = selected_letters, choices = letters, multiple = T)))
               showModal(modalDialog(my_input()))
               # selected_letters <- input$letters
              })

     output$selected <- renderPrint(input$letters)
}

shinyApp(ui = ui, server = server)

