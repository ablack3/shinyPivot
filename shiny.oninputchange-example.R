library(shiny)
library(shinyjqui)
library(tidyverse)


df <- starwars %>% 
     select_if(is.character)


ui <- shinyUI( bootstrapPage(
     verbatimTextOutput("results"),
     verbatimTextOutput("results2"),
     orderInput("source_vars", "Variables", items = names(df)),
     includeScript("mycode.js")
))


server <- shinyServer(function(input, output, session) {
     output$results = renderPrint({input$varname})
     output$results2 = renderPrint({input$a})
     
     pivot_vars <- df %>% 
          summarise_all(n_distinct) %>% 
          collect() %>% 
          gather("field", "n_levels") %>% 
          mutate(levels = map(pivot_vars$field, ~pull(distinct(select(df, .))))) %>% 
          mutate(select_input = map2(pivot_vars$field, pivot_vars$levels, 
                ~reactive(selectInput(.x, label = .x, choices = .y, selected = input[[.x]], multiple = T))))
     
     observeEvent(input$click_counter, {
          reactive_select <- pivot_vars[1,"select_input"]
               showModal(modalDialog(
                    title = "Filter",
                    pivot_vars$select_input[[match(input$varname, pivot_vars$field)]]()
               ))
          
          # color = rgb(runif(1), runif(1), runif(1))
          # session$sendCustomMessage(type = "myCallbackHandler", color)
     })
})

shinyApp(ui = ui, server = server)




