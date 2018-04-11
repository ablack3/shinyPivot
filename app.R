library(shiny)
library(shinyjqui)
library(wakefield)
library(dplyr)
library(tidyr)

df <- wakefield::r_data_frame(1000, age, gender, coin)
record_limit <- 1e6
max_levels <- 1000

table_vars <- colnames(df)

# count number of distinct levels for each variable
ndis <- df %>%
     ungroup() %>% 
     summarise_all(n_distinct) %>% 
     collect()
     
pivot_vars <- ndis %>% 
     gather("field", "n_distinct") %>% 
     filter(n_distinct < max_levels)

# get distinct levels for each pivot variable
# pivot_vars object
pivot_vars$levels <- purrr::map(pivot_vars$field, ~pull(distinct(select(df, .))))
pivot_vars$selected_levels <- pivot_vars$levels
pivot_vars$select_input <- purrr::map2(pivot_vars$field, pivot_vars$levels, ~selectizeInput(.x, label = .x, choices = .y, selected = .y))

# need a way to update selected for each variable.


ui <- fluidPage(title = "R pivot table", includeScript("app.js"), tabsetPanel(
     tabPanel("Patients"),
     tabPanel("Events",

     fluidRow(
          column(4, textOutput("debug_text1")),
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
     ) # end tab
)) # end ui


server <- function(input, output){
     
     filtered_data <- reactive({
          grp_vars <- rlang::parse_quosures(paste0(c(input$row_vars_order, input$col_vars_order), collapse = ";"))
        df %>% 
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
     output$debug_text1 <- renderPrint(input$mydata)
     # output$debug_text1 <- renderPrint({paste(input$row_vars_order, collapse = ";")})
     output$debug_text2 <- renderText(input$col_vars_order)
     output$debug_text3 <- renderText("")
     output$download_data <- downloadHandler(
          filename = function() paste0("data", Sys.Date(), ".csv"),
          content = function(file) readr::write_csv(local_table, file)
     )
     
} # end server

shinyApp(ui = ui, server = server)










