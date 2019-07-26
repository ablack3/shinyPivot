
#' Generate the UI for a pivot table
#'
#' @param id The namespace id as a string. Can be anything but must match the corresponding namespace id of pivot_module.
#' @param pivot_vars A tibble created by get_pivot_vars()
#' @param sum_vars A character vector contiaining the names of numeric variables in the data to summarise by.
#'
#' @return A tag list containing the UI elements for a pivot table module
#' @export
#' @examples
#' ui <- fluidPage(
#'       pivot_module_UI(id = "id1", pivot_vars = my_pivot_vars)
#' )
pivot_rate_module_UI <- function(id, pivot_vars, sum_vars = ""){
     ns <- NS(id)
     nsq <- function(.) glue::glue('"{ns(.)}"')

     tagList(
          tags$script(HTML(glue::glue('
                                      $(document).ready(function() {{

                                      const {id}_b = document.querySelectorAll("#{ns("source_vars")} div");
                                      console.log("#{ns("source_vars")} div")
                                      console.log({id}_b);
                                      console.log("{id}_b.length is " + {id}_b.length)
                                      var click_counter = 0;

                                      //$("#{ns("source_vars")} div").attr("title", "This is the hover-over text");

                                      function sendDataToShiny(clicked_button) {{
                                      click_counter++;
                                      var val = clicked_button.getAttribute("data-value");
                                      //console.log("you clicked " + val + " and click_counter is " + click_counter);
                                      Shiny.onInputChange({nsq("varname")}, val);
                                      Shiny.onInputChange({nsq("click_counter")}, click_counter);
                                      }}

                                      for (var i = 0; i < {id}_b.length; i++) {{
                                      //{id}_b[i].attr("title", "hello tooltip " + i);
                                      {id}_b[i].addEventListener("click", function(){{
                                      //console.log("you clicked " + this.getAttribute("data-value"));
                                      sendDataToShiny(this);

                                      }}, false);
                                      }}

                                      Shiny.addCustomMessageHandler("{id}_shade", function (val) {{
                                      //console.log("recieved " + val)
                                      for (var i = 0; i < {id}_b.length; i++) {{
                                      if (val == {id}_b[i].getAttribute("data-value")){{
                                      {id}_b[i].style.backgroundColor = "#b6b8ba";
                                      }}
                                      }}
                                      }});

                                      Shiny.addCustomMessageHandler("{id}_unshade", function (val) {{
                                      for (var i = 0, len = {id}_b.length; i < len; i++) {{
                                      if (val == {id}_b[i].getAttribute("data-value")){{
                                      {id}_b[i].style.backgroundColor = "#ffffff";
                                      }}
                                      }}
                                      }});

                                      }});'))),

          # fluidRow(column(4, verbatimTextOutput(ns("debug_text")))), # Used for debugging
          fluidRow(column(4, tags$div(style = "color: red", textOutput(ns("warn_text"))))),
          fluidRow(column(12, wellPanel(
               shinyjqui::orderInput(ns("source_vars"), "Variables", items = pivot_vars$field, width = "100%", connect = c(ns("row_vars"), ns("col_vars")))
          ))),
          tags$p(uiOutput(ns("filter_text"))),
          fluidRow(
               column(3,
                      fluidRow(column(6,  actionButton(ns("refresh"), "Refresh")),
                               column(6,  downloadButton(ns("download_data")))),
                      fluidRow(column(12, selectInput(ns("numerator_var"),   "Numerator:", choices = c("Count", sum_vars), selected = "Count")))#,
                      # fluidRow(column(12, selectInput(ns("denominator_var"), "Denominator:", choices = c(paste0(c("", "1,000 ", "10,000 ", "100,000 "), "Person-years eligibility"), "None"), selected = "None")))
               ),
               column(9, wellPanel(shinyjqui::orderInput(ns("col_vars"), "Columns", items = NULL, placeholder = "Drag variables here", width = "100%", connect = c(ns("source_vars"), ns("row_vars")))))
          ),
          fluidRow(
               column(3, wellPanel(shinyjqui::orderInput(ns("row_vars"), "Rows", items = NULL, placeholder = "Drag variables here", width = "100%", connect = c(ns("source_vars"), ns("col_vars"))))),
               column(9, tabsetPanel(
                    tabPanel("Numerator",   tags$div(style = "overflow:auto", DT::dataTableOutput(ns("numer_table")))),
                    tabPanel("Denominator", tags$div(style = "overflow:auto", DT::dataTableOutput(ns("denom_table")))),
                    tabPanel("Rate",        tags$div(style = "overflow:auto", DT::dataTableOutput(ns("rate_table")))),
                    tabPanel("Rate full",   tags$div(style = "overflow:auto", DT::dataTableOutput(ns("rate_table_full"))))
               ))
          )
     )
}

#' The server function for a pivot table module
#'
#' This function should be passed to callModule. See example.
#'
#' @param input A standard argument used by shiny when creating the module.
#' @param output A standard argument used by shiny when creating the module.
#' @param session A standard argument used by shiny when creating the module.
#' @param ns_id The module namespace id as a string. Must match a namespace id of the corresponding UI module element.
#' @param df A local dataframe/tibble or tbl_dbi database connection object.
#' @param pivot_vars A table constructed using the get_pivot_vars function.
#' @param record_limit The maximum number of rows to bring into R to display. This is a saftely measure. You probably don't want to bring 100 million rows of data into R from a database. Defaults to 1 million.
#'
#' @return The server function needed for a pivot table module.
#' @export
#'
#' @examples
#'  # note that the namespace id must
#' server <- function(input, output, session){
#'    callModule(pivot_module, id = "id1", ns_id = "id1", df = df1, pivot_vars = pivot_vars1, record_limit = 20)
#' }
#'
pivot_rate_module <- function(input, output, session, ns_id, numer_df, denom_df, pivot_vars, record_limit = 1e6, show_filter_text = T){

     if(record_limit > 1e6) warning("Allowing variables with more than 1,000,000 levels could result in poor performance")

     ns <- NS(ns_id)
     # Normally when using shiny modules you do not need to pass the namespace id (supplied by the user of the module) to the server function
     # However since we are creating UI elements in the server function we do need the namespace id
     # One tricky thing - We only wrap UI ids with the ns() function. Server ids are not wrapped with ns()

     # add reactive values to the pivot vars tibble in a list column.
     # One select input and one T/F filtered indicator per pivot variable.
     # Need to add namespace to any newly created input ids. Use ns() when defining new input id
     pivot_vars <- pivot_vars %>%
          mutate(filtered = purrr::map(field, ~reactive({length(input[[.x]]) > 0})))

     # get the variables in the denominator table
     denom_pivot_vars <- denom_df %>%
          head() %>%
          collect() %>%
          colnames() %>%
          intersect(pivot_vars$field)

     # which variable was clicked? (represented as a number from 1 to number of pivot vars)
     varnum <- reactive(match(input$varname, pivot_vars$field))

     # # open dialog box when clicked
     observeEvent(input$click_counter, {

          updateSelectizeInput(session, inputId = input$varname, choices = pivot_vars$levels[[varnum()]], server = T, selected = input[[input$varname]])
          showModal(modalDialog(title = "Filter",
                                selectInput(inputId = ns(input$varname), input$varname, choices = character(0), multiple = T),
                                easyClose = T,  pivot_vars$description[[varnum()]], footer = tagList(modalButton("Close"))
          ))
     })

     numer_filter_expr <- reactive({
          # T/F indicators to select rows of filtered variables
          selector <- purrr::map_lgl(pivot_vars$filtered, ~.())

          # initially no variables will be filtered
          if(all(!selector)) return(NULL)

          exp_builder <- pivot_vars %>%
               filter(selector) %>%
               mutate(selected_levels = purrr::map(field, ~input[[.]])) %>%
               mutate(selected_levels = purrr::map(selected_levels, ~paste(.))) %>%
               select(field, selected_levels)

          purrr::map2(exp_builder$field, exp_builder$selected_levels, ~rlang::expr(!!as.name(.x) %in% c(!!.y))) %>%
               purrr::reduce(function(a,b) rlang::expr(!!a & !!b))
     })


     denom_filter_expr <- reactive({
          # T/F indicators to select rows of filtered variables
          selector <- purrr::map_lgl(pivot_vars$filtered, ~.())

          # initially no variables will be filtered
          if(all(!selector)) return(NULL)

          exp_builder <- pivot_vars %>%
               filter(selector) %>%
               mutate(selected_levels = purrr::map(field, ~input[[.]])) %>%
               mutate(selected_levels = purrr::map(selected_levels, ~paste(.))) %>%
               select(field, selected_levels) %>%
               filter(field %in% denom_pivot_vars)

          purrr::map2(exp_builder$field, exp_builder$selected_levels, ~rlang::expr(!!as.name(.x) %in% c(!!.y))) %>%
               purrr::reduce(function(a,b) rlang::expr(!!a & !!b))
          # rlang::expr()
     })

     numer_summarised <- eventReactive(input$refresh, {
          grp_vars <- rlang::parse_quosures(paste0(c(input$row_vars_order, input$col_vars_order), collapse = ";"))
          numer_df %>%
               {if(!is.null(numer_filter_expr)) filter(., !!!numer_filter_expr()) else .}  %>% # conditional pipe
               group_by(!!!grp_vars) %>%
               # another conditional pipe to do the summarization
               {if(input$numerator_var == "Count") summarise(., n = n()) else summarise(., n = sum(!!as.name(input$numerator_var), na.rm = T))} %>%
               ungroup() %>%
               head(record_limit) %>%
               collect() %>%
               mutate_if(~class(.) == "integer64", as.numeric)

          # numer_df
     })

     denom_summarised <- eventReactive(input$refresh, {
          grp_vars <- rlang::parse_quosures(paste0(intersect(c(input$row_vars_order, input$col_vars_order), denom_pivot_vars), collapse = ";"))
          denom_df %>%
               {if(!is.null(denom_filter_expr)) filter(., !!!denom_filter_expr()) else .}  %>% #
               group_by(!!!grp_vars) %>%
               # another conditional pipe to do the summarization
               summarise(person_years = sum(elig, na.rm = T)) %>% ############# elig is a hard coded variable
               # {if(input$numerator_var == "Count") summarise(., n = n()) else summarise(., n = sum(!!as.name(input$numerator_var), na.rm = T))} %>%
               ungroup() %>%
               head(record_limit) %>%
               collect() %>%
               mutate_if(~class(.) == "integer64", as.numeric)

     })


     rate_full <- eventReactive(input$refresh, {
          by_vars <- intersect(c(input$row_vars_order, input$col_vars_order), denom_pivot_vars)

          if(length(by_vars) > 0){
               res <- numer_summarised() %>%
                         left_join(denom_summarised(), by = by_vars) %>%
                         mutate(rate = n/person_years)
          } else {
               res <- numer_summarised() %>%
                    mutate(person_years = pull(denom_summarised(), person_years)) %>%
                    mutate(rate = n/person_years)
          }
          return(res)
     })


     numer_final <- eventReactive(input$refresh, {
          numer_summarised() %>%
          {
               if(length(input$col_vars_order) > 0 ){
                    tidyr::unite(., "col_var", input$col_vars_order, sep = "_&_") %>%
                         tidyr::spread(col_var, n, fill = 0)
               } else .
          }
     })

     denom_final <- eventReactive(input$refresh, {

          denom_col_vars <- intersect(input$col_vars_order, colnames(denom_summarised()))

          denom_summarised() %>%
          {
               if(length(denom_col_vars) > 0 ){
                    tidyr::unite(., "col_var", denom_col_vars, sep = "_&_") %>%
                         tidyr::spread(col_var, person_years, fill = 0)
               } else .
          }
     })

     rate_final <- eventReactive(input$refresh, {

          if(length(input$col_vars_order) > 0 ){
               rate_full() %>%
                    select(-n, -person_years) %>%
                    tidyr::unite(., "col_var", input$col_vars_order, sep = "_&_") %>%
                    tidyr::spread(col_var, rate, fill = 0)
          } else {
               rate_full() %>%
                    select(-n, -person_years)
          }
     })

     ############## outputs ###############
     output$numer_table <- DT::renderDataTable({
          df <- numer_final()
          df %>%
               DT::datatable(rownames = FALSE) %>%
               DT::formatRound(1:ncol(df), digits = 0)
     })

     output$denom_table <- DT::renderDataTable({
          # df <- denom_final()
          df <- denom_final()
          df %>%
               DT::datatable(rownames = FALSE) %>%
               DT::formatRound(1:ncol(df), digits = 0)
     })

     output$rate_table <- DT::renderDataTable({
          df <- rate_final()
          df %>%
               DT::datatable(rownames = FALSE) %>%
               DT::formatRound(1:ncol(df), digits = 0)
     })

     output$rate_table_full <- DT::renderDataTable({
          df <- rate_full()
          df %>%
               DT::datatable(rownames = FALSE) %>%
               DT::formatRound(1:ncol(df), digits = 0)
     })


     # update button colors based on filtering
     observe({
          for (i in 1:nrow(pivot_vars)) {
               if(pivot_vars$filtered[[i]]() == TRUE){
                    # print(paste("sending ", pivot_vars$field[i]))
                    session$sendCustomMessage(type = paste0(ns_id, "_shade"), pivot_vars$field[i])
               } else {
                    session$sendCustomMessage(type = paste0(ns_id, "_unshade"), pivot_vars$field[i])
               }
          }
     })

     output$warn_text <- renderText({
          if(nrow(numer_summarised()) == record_limit){
               return(glue::glue("ERROR: Query returned more rows then the record limit: {record_limit} rows. Inaccurate results!"))
          } else return(NULL)
     })


     output$filter_text <- renderUI({
          req(numer_filter_expr())
          req(show_filter_text)

          numer_filter_expr() %>%
               deparse() %>%
               paste0(collapse = " ") %>%
               # stringr::str_replace_all("%in% \"(.*)\"", "%in% \\{\\1\\}") %>%
               stringr::str_remove_all("%") %>%
               stringr::str_remove_all("\"") %>%
               stringr::str_replace_all("(c\\()+", "{") %>%
               stringr::str_replace_all("(\\))+", "}") %>%
               stringr::str_replace_all("\\&", "AND") %>%
               paste("Filter:", .)
     })

     output$debug_text <- renderText(denom_pivot_vars)
     output$download_data <- downloadHandler(
          filename = function() paste0("data_", Sys.Date(), ".xlsx"),
          content = function(file) write_xlsx_multiple(file, numer_final(), denom_final(), rate_final(), rate_full())
     )
} # end server











