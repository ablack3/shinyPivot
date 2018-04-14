library(shiny)
library(shinyjqui)
library(tidyverse)
# library(wakefield)

# inputs
# df <- wakefield::r_data_frame(1000, age, gender, coin)
df <- starwars %>% 
     select_if(is.character)

record_limit <- 1e6
max_levels <- 1000

#  
pivot_vars <- df %>% 
     summarise_all(n_distinct) %>% 
     collect() %>% 
     gather("field", "n_levels") %>% 
     filter(n_levels < max_levels) %>% 
     mutate(levels = map(field, ~pull(distinct(select(df, .))))) 


ui <- fluidPage(title = "R pivot table", includeScript("mycode.js"),

     fluidRow(
          column(4, verbatimTextOutput("debug_text1")),
          column(4, textOutput("debug_text2")),
          column(4, textOutput("debug_text3"))
     ),
     fluidRow(column(12, wellPanel(orderInput("source_vars", "Variables", items = pivot_vars$field, connect = c("row_vars","col_vars"))))),
     fluidRow(
          column(2, offset = 1, downloadButton("download_data")),
          column(8, offset = 1, wellPanel(orderInput("col_vars", "Columns", items = NULL, placeholder = "Drag variables here", connect = c("source_vars","row_vars"))))
     ),
     fluidRow(
          column(4, wellPanel(orderInput("row_vars","Rows", items = NULL, placeholder = "Drag variables here", connect = c("source_vars", "col_vars")))),
          column(8, tags$div(style = "overflow:auto", dataTableOutput("table")))
     )
)

server <- function(input, output, session){
     
     # add reactive select inputs to pivot_vars df
     pivot_vars <- pivot_vars %>% 
          mutate(select_input = map2(field, levels, 
               ~reactive(selectInput(.x, label = .x, choices = c("All levels" = "", .y), selected = input[[.x]], multiple = T)))) %>% 
          mutate(filtered = map(field, ~reactive({length(input[[.x]]) > 0})))
     
     # which variable was click (as a number 1 to number of pivot vars)
     varnum <- reactive(match(input$varname, pivot_vars$field))
     
     # open dialog box when clicked
     observeEvent(input$click_counter, {
          showModal(modalDialog(easyClose = T, title = "Filter", pivot_vars$select_input[[varnum()]]()))
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
     
     # filter before the summary right?
     # yes because I want to allow filtering on variables that are not summary variables
     # need to create the expression to go into filter(!!!filter_exprs)
     # for each variable that is filtered create an expression
     
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
          # exp_builder

          map2(exp_builder$field, exp_builder$selected_levels, ~rlang::expr(!!as.name(.x) %in% !!.y)) %>%
               reduce(function(a,b) rlang::expr(!!a & !!b))
     })
     
     filtered_data <- reactive({
          grp_vars <- rlang::parse_quosures(paste0(c(input$row_vars_order, input$col_vars_order), collapse = ";"))
        df %>% 
          {if(!is.null(filter_expr)) filter(., !!!filter_expr()) else .} %>% # conditional pipe
          group_by(!!!grp_vars) %>% 
          summarise(n = n()) %>% 
          ungroup()
     })

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
     
     output$table <- renderDataTable(local_table())
     output$debug_text1 <- renderPrint(filter_expr())
     # output$debug_text1 <- renderPrint({paste(input$row_vars_order, collapse = ";")})
     output$debug_text2 <- renderText(input$col_vars_order)
     output$debug_text3 <- renderText("")
     output$download_data <- downloadHandler(
          filename = function() paste0("data", Sys.Date(), ".csv"),
          content = function(file) readr::write_csv(local_table, file)
     )
     
} # end server

shinyApp(ui = ui, server = server)










