library(shiny)
library(shinyjqui)

ui <- shinyUI( bootstrapPage(

     verbatimTextOutput("results"),
     orderInput("source_vars", "Variables", items = letters[1:3]),
     includeScript("mycode.js")
))


server <- shinyServer(function(input, output, session) {
     output$results = renderPrint({input$varname})

     observeEvent(input$varname, {
               showModal(modalDialog(
                    title = "Important message",
                    paste0("you clicked on ",input$varname)
               ))
          # showModal(modalDialog(selectInput("a", choices = letters)))
          
          # color = rgb(runif(1), runif(1), runif(1))
          # session$sendCustomMessage(type = "myCallbackHandler", color)
     })
})

shinyApp(ui = ui, server = server)




