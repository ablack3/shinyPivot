
# the key to doing this
# all inputs in ui must be namespaced. 
# anytime we send data from JS to shiny the id needs to be namespaced
# however when we use the sent info in the server module function we don't need to use the namespace because Shiny takes care of that
# Any HTML tag id attributes must be namespaced
# If you are selecting DOM elements based on attributes created by Shiny you might need to use ns_js as well

options(shiny.reactlog=T) # command f3 + right arrow

moduleUI <- function(id){
     ns <- NS(id)
     nsq <- function(.) glue::glue('"{ns(.)}"')
     
     tagList(
          # a div named mydiv
          tags$div(id=ns("mydiv"), style="width: 50px; height :50px;
              left: 100px; top: 100px;
              background-color: gray;", class = "square"),
          
          # a shiny element to display unformatted text
          verbatimTextOutput(ns("results")),
          
          # javascript code to send data to shiny server
          
          tags$script(HTML(glue::glue('
                 document.getElementById({nsq("mydiv")}).onclick = function() {{
                 var number = Math.random();
                 Shiny.onInputChange({nsq("mydata")}, number);
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
