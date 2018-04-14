library(shiny)
library(shinyjqui)
library(tidyverse)


df <- starwars %>% 
     select_if(is.character)

pivot_vars <- df %>% 
     summarise_all(n_distinct) %>% 
     collect() %>% 
     gather("field", "n_levels") %>% 
     mutate(levels = map(field, ~pull(distinct(select(df, .))))) 

ui <- shinyUI(bootstrapPage(
     verbatimTextOutput("results"),
     verbatimTextOutput("results2"),
     orderInput("source_vars", "Variables", items = names(df), connect = "col_vars"),
     orderInput("col_vars", "Columns", items = NULL, placeholder = "Drag variables here", connect = c("source_vars")),
     includeScript("mycode.js")
))

server <- shinyServer(function(input, output, session) {

     pivot_vars <- pivot_vars %>% 
          mutate(select_input = map2(field, levels, 
                ~reactive(selectInput(.x, label = .x, choices = c("All levels" = "", .y), selected = input[[.x]], multiple = T)))) %>% 
          mutate(filtered = map(field, ~reactive({length(input[[.x]]) > 0})))
     
     # which variable was click (as a number 1 to number of pivot vars)
     varnum <- reactive(match(input$varname, pivot_vars$field))

     observeEvent(input$click_counter, {
          showModal(modalDialog(easyClose = T, title = "Filter", pivot_vars$select_input[[varnum()]]()))
          
          # color = rgb(runif(1), runif(1), runif(1))
     })
          
     observe({
          for (i in 1:nrow(pivot_vars)) {
               if(pivot_vars$filtered[[i]]() == TRUE){
                    session$sendCustomMessage(type = "shade", pivot_vars$field[i]) 
               } else {
                    session$sendCustomMessage(type = "unshade", pivot_vars$field[i])  
               }
          }
     })
     
     output$results = renderPrint({input$varname})
     # output$results2 = renderPrint({
     #      req(pivot_vars$filtered[[varnum()]]())
     #      pivot_vars$filtered[[varnum()]]()
     # })
})

shinyApp(ui = ui, server = server)




