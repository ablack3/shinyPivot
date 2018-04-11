library(shiny)
library(shinyjqui)
library(tidyverse)

# create select boxes
df <- data_frame(var = letters[1:3]) %>% 
     mutate(input_box = map(letters[1:3], ~selectInput(.,., choices = letters, multiple = T)))

ui <- shinyUI( bootstrapPage(

     verbatimTextOutput("results"),
     verbatimTextOutput("results2"),
     orderInput("source_vars", "Variables", items = letters[1:3]),
     includeScript("mycode.js")
))


server <- shinyServer(function(input, output, session) {
     output$results = renderPrint({input$varname})
     output$results2 = renderPrint({input$click_counter})

     
     # need counter variable to trigger click event, event not triggered if same button is clicked > 1 time
     observeEvent(input$click_counter, {
               showModal(modalDialog(
                    title = "Important message",
                    selectInput("id","label", choices = letters, multiple = T),
                    paste0("you clicked on ", input$varname)
               ))
          # showModal(modalDialog(selectInput("a", choices = letters)))
          
          # color = rgb(runif(1), runif(1), runif(1))
          # session$sendCustomMessage(type = "myCallbackHandler", color)
     })
})

shinyApp(ui = ui, server = server)




