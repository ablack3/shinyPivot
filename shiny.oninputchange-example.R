# ui.R
library(shiny)
library(shinyjqui)

ui <- shinyUI( bootstrapPage(

     tags$div(id="mydiv", class = "box", style="width: 50px; height :50px;
              left: 100px; top: 100px;
              background-color: gray; position: absolute"),
     
     verbatimTextOutput("results"),
     orderInput("source_vars", "Variables", items = letters[1:3]),
     includeScript("mycode.js")
     ))

server <- shinyServer(function(input, output, session) {
     output$results = renderPrint({input$mydata})

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




