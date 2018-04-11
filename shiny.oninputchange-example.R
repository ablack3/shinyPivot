# ui.R
library(shiny)
library(shinyjqui)

ui <- shinyUI( bootstrapPage(
     
     # a div named mydiv
     tags$div(id="mydiv", class = "box", style="width: 50px; height :50px;
              left: 100px; top: 100px;
              background-color: gray; position: absolute"),
     
     # a shiny element to display unformatted text
     verbatimTextOutput("results"),
     orderInput("source_vars", "Variables", items = letters[1]),
     
     # javascript code to send data to shiny server
     # include the js code
     includeScript("mycode.js")
     
     # tags$script('
     #             document.getElementById("mydiv").onclick = function() {
     #             var number = Math.random();
     #             Shiny.onInputChange("mydata", number);
     #             };
     #             ')

     ))
# Now, on the server side, we can simply access the                                                                                                                     # server.R

server <- shinyServer(function(input, output, session) {

     output$results = renderPrint({
     input$mydata
     })
     
     
     # observes if value of mydata sent from the client changes.  if yes
     # generate a new random color string and send it back to the client
     # handler function called 'myCallbackHandler'
     observe({
          input$mydata
          color = rgb(runif(1), runif(1), runif(1))
          session$sendCustomMessage(type = "myCallbackHandler", color)
     })

})

shinyApp(ui = ui, server = server)




