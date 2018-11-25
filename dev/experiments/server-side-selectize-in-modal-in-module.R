library(shiny)
library(dplyr)

options(shiny.reactlog=T) # command f3 + right arrow

baby_names <- babynames::babynames %>%
     distinct(name) %>%
     pull(name) %>%
     sort()

module_UI <- function(id){
     ns <- NS(id)
     tagList(
          actionButton(ns("show"), "Show modal dialog"),
          verbatimTextOutput(ns("selections")))
}

module <- function(input, output, session, ns_id){
     ns <- NS(ns_id)
     observeEvent(input$show, {
          updateSelectizeInput(session, "babyname", choices = baby_names, selected = input$babyname, server = TRUE)
          showModal(modalDialog(selectInput(ns("babyname"), "Baby Name", multiple = TRUE, choices = character(0))))
     })

     output$selections <- renderText(input$babyname)
}

ui <- fluidPage(
     module_UI("t")
)

server <- function(input, output, session) {
     callModule(module, "t", ns_id = "t")
}
shinyApp(ui, server)
