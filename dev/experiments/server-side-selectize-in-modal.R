library(shiny)
library(dplyr)

options(shiny.reactlog=T) # command f3 + right arrow

baby_names <- babynames::babynames %>%
     distinct(name) %>%
     pull(name) %>%
     sort()

ui <- fluidPage(
     actionButton("show", "Show modal dialog"),
     verbatimTextOutput("selections")
)

server <- function(input, output, session) {

     observeEvent(input$show, {
          updateSelectizeInput(session, "babyname", choices = baby_names, selected = input$babyname, server = TRUE)
          showModal(modalDialog(selectInput("babyname", "Baby Name", multiple = TRUE, choices = character(0))))
     })

     output$selections <- renderText(input$babyname)
}

shinyApp(ui, server)
