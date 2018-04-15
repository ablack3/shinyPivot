library(shiny)
library(shinyjqui)
library(tidyverse)

get_pivot_vars <- function(df, max_levels = 1000){
     df %>% 
     summarise_all(n_distinct) %>% 
     collect() %>% 
     tidyr::gather("field", "n_levels") %>% 
     filter(n_levels < max_levels) %>% 
     mutate(levels = purrr::map(field, ~pull(distinct(select(df, .))))) 
}


shiny_pivot_module_UI <- function(id, pivot_vars){
     ns <- NS(id)
     nsq <- function(.) glue::glue('"{ns(.)}"')
     
     tagList(
               tags$script(HTML(glue::glue('
                     $(document).ready(function() {{
     
                     const b = document.querySelectorAll("#{ns("source_vars")} div");
                     //console.log(b);
                     //console.log("b.length is " + b.length)
                     var click_counter = 0;
                     
                     function sendDataToShiny(clicked_button) {{
                          click_counter++;
                          var val = clicked_button.getAttribute("data-value");
                         console.log("click_counter is " + click_counter);
                          Shiny.onInputChange({nsq("varname")}, val);
                          Shiny.onInputChange({nsq("click_counter")}, click_counter);
                     }}
                     
                     for (var i = 0; i < b.length; i++) {{
                          b[i].addEventListener("click", function(){{
                              console.log("you clicked " + this.getAttribute("data-value"));
                              sendDataToShiny(this);
                                   
                         }}, false);
                     }}     
                     
                     Shiny.addCustomMessageHandler("shade", function (val) {{
                          for (var i = 0; i < b.length; i++) {{ 
                               if (val == b[i].getAttribute("data-value")){{
                                    b[i].style.backgroundColor = "#b6b8ba";
                               }}  
                          }}
                     }});
                     
                     Shiny.addCustomMessageHandler("unshade", function (val) {{
                          for (var i = 0, len = b.length; i < len; i++) {{ 
                               if (val == b[i].getAttribute("data-value")){{
                                    b[i].style.backgroundColor = "#ffffff";
                               }}  
                          }}
                     }});
                                                        
                    }});'))),
                
                fluidRow(column(4, verbatimTextOutput(ns("debug_text")))),
                fluidRow(column(12, wellPanel(
                     orderInput(ns("source_vars"), "Variables", items = pivot_vars$field, connect = c(ns("row_vars"), ns("col_vars")))
                ))),
                fluidRow(
                     column(3, downloadButton(ns("download_data"))),
                     column(9, wellPanel(orderInput(ns("col_vars"), "Columns", items = NULL, placeholder = "Drag variables here", connect = c(ns("source_vars"), ns("row_vars")))))
                ),
                fluidRow(
                     column(3, wellPanel(orderInput(ns("row_vars"),"Rows", items = NULL, placeholder = "Drag variables here", connect = c(ns("source_vars"), ns("col_vars"))))),
                     column(9, tags$div(style = "overflow:auto", dataTableOutput(ns("table"))))
                )
     )
}

shiny_pivot_module <- function(input, output, session, df, pivot_vars, record_limit = 1e6){
     
     # add reactive inputs
     pivot_vars <- pivot_vars %>%
          mutate(select_input = map2(field, levels,
               ~reactive(selectInput(ns(.x), label = .x, choices = c("All levels" = "", .y), selected = input[[.x]], multiple = T)))) %>%
          mutate(filtered = purrr::map(field, ~reactive({length(input[[.x]]) > 0})))

     # which variable was clicked (as a number 1 to number of pivot vars)
     varnum <- reactive(match(input$varname, pivot_vars$field))
     
     # open dialog box when clicked
     observeEvent(input$click_counter, {
          showModal(modalDialog(easyClose = T, title = "Filter", pivot_vars$select_input[[varnum()]]()))
          # showModal(modalDialog(easyClose = T, title = "Filter"))
     })


     observe({print(names(input))})
     
     output$table <- renderDataTable(local_table())
     output$debug_text <- renderPrint(varnum())
     output$download_data <- downloadHandler(
          filename = function() paste0("data_", Sys.Date(), ".csv"),
          content = function(file) readr::write_csv(local_table(), file),
          contentType = "text/csv"
     )
} # end server











