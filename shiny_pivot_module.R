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
                     column(3, wellPanel(orderInput(ns("row_vars"), "Rows", items = NULL, placeholder = "Drag variables here", connect = c(ns("source_vars"), ns("col_vars"))))),
                     column(9, tags$div(style = "overflow:auto", dataTableOutput(ns("table"))))
                )
     )
}

shiny_pivot_module <- function(input, output, session, df, pivot_vars, record_limit = 1e6){
     
     # add reactive values to the pivot vars tibble in a list column. 
     # One select input and one T/F filtered indicator per pivot variable.
     pivot_vars <- pivot_vars %>%
          mutate(select_input = map2(field, levels,
               ~reactive(selectInput(ns(.x), label = .x, choices = c("All levels" = "", .y), selected = input[[.x]], multiple = T)))) %>%
          mutate(filtered = purrr::map(field, ~reactive({length(input[[.x]]) > 0})))

     # which variable was clicked? (represented as a number from 1 to number of pivot vars)
     varnum <- reactive(match(input$varname, pivot_vars$field))
     
     # open dialog box when clicked
     observeEvent(input$click_counter, {
          showModal(modalDialog(easyClose = T, title = "Filter", pivot_vars$select_input[[varnum()]]()))
     })
     
     
     filter_expr <- reactive({
          # T/F indicators to select rows of filtered variables
          selector <- map_lgl(pivot_vars$filtered, ~.())
          
          # initially no variables will be filtered
          if(all(!selector)) return(NULL)
          
          exp_builder <- pivot_vars %>% 
               filter(selector) %>% 
               mutate(selected_levels = map(field, ~input[[.]])) %>% 
               mutate(selected_levels = map(selected_levels, ~paste(.))) %>% 
               select(field, selected_levels)
          
          map2(exp_builder$field, exp_builder$selected_levels, ~rlang::expr(!!as.name(.x) %in% !!.y)) %>%
               reduce(function(a,b) rlang::expr(!!a & !!b))
     })
     
     # maybe use event reactive and update button
     filtered_data <- reactive({
          grp_vars <- rlang::parse_quosures(paste0(c(input$row_vars_order, input$col_vars_order), collapse = ";"))
          df %>% 
          {if(!is.null(filter_expr)) filter(., !!!filter_expr()) else .} %>% # conditional pipe
               group_by(!!!grp_vars) %>% 
               summarise(n = n()) %>% 
               ungroup()
     })
     
     # also need to add warning for exceeding row limit
     local_table <- reactive({
          filtered_data() %>% 
               filter(between(row_number(), 1, record_limit)) %>% 
               collect() %>% 
               mutate_if(~class(.) == "integer64", as.numeric) %>% 
               {
                    if(length(input$col_vars_order) > 0 ){
                         tidyr::unite(., "col_var", input$col_vars_order, sep = "_&_") %>% 
                              tidyr::spread(col_var, n, fill = 0)
                    } else .
               }
     })
     

     # update button colors based on filtering
     observe({
          for (i in 1:nrow(pivot_vars)) {
               if(pivot_vars$filtered[[i]]() == TRUE){
                    session$sendCustomMessage(type = "shade", pivot_vars$field[i])
               } else {
                    session$sendCustomMessage(type = "unshade", pivot_vars$field[i])
               }
          }
     })

     output$table <- renderDataTable(local_table())
     output$debug_text <- renderPrint(varnum())
     output$download_data <- downloadHandler(
          filename = function() paste0("data_", Sys.Date(), ".csv"),
          content = function(file) readr::write_csv(local_table(), file),
          contentType = "text/csv"
     )
} # end server











