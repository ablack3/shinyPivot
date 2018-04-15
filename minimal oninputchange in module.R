
# the key to doing this


options(shiny.reactlog=T) # command f3 + right arrow

moduleUI <- function(id){
     ns <- NS(id)
     ns_js <- function(.) glue::glue('"{ns(.)}"')
     
     tagList(
          # a div named mydiv
          tags$div(id=ns("mydiv"), style="width: 50px; height :50px;
              left: 100px; top: 100px;
              background-color: gray;", class = "square"),
          
          # a shiny element to display unformatted text
          verbatimTextOutput(ns("results")),
          
          # javascript code to send data to shiny server
          
          tags$script(HTML(glue::glue('
                 document.getElementById({ns_js("mydiv")}).onclick = function() {{
                 var number = Math.random();
                 Shiny.onInputChange({ns_js("mydata")}, number);
                 }};')))
     )
}

module <- function(input, output, session){
     output$results <- renderPrint({input$mydata})
}

ui <- fluidPage(
     fluidRow(moduleUI("id1")),
     fluidRow(moduleUI("id2"))
)

server <- function(input, output, session) {
     callModule(module, "id1")
     callModule(module, "id2")
}

shinyApp(ui = ui, server = server)
