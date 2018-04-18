ui <- fluidPage(
     # a div named mydiv
     tags$div(id="mydiv", style="width: 50px; height :50px;
              left: 100px; top: 100px;
              background-color: gray; position: absolute"),
     
     # a shiny element to display unformatted text
     verbatimTextOutput("results"),
     
     # javascript code to send data to shiny server
     tags$script('
                 document.getElementById("mydiv").onclick = function() {
                 var number = Math.random();
                 Shiny.onInputChange("mydata", number);
                 };
                 ')
     )

# server.R

server <- function(input, output, session) {
     output$results = renderPrint({
          input$mydata
     })
     
}

shinyApp(ui = ui, server = server)