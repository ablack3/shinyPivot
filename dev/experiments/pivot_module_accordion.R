#' Create a table with one row per pivot variable
#'
#' @param df A local dataframe or tbl_dbi database table
#' @param max_levels The maximum number of levels a pivot variable is allowed to have.
#'
#' @return A tibble with one row per pivot variable with the variable's name, number of levels, and a list column containing the levels.
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#' @examples
#' # local table
#' df1 <- starwars %>%
#'      select_if(is.character)
#' pivot_vars1 <- get_pivot_vars(df1)
#'
#' # using a database
#' con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
#' copy_to(con, df1, "star_wars")
#' df2 <- tbl(con, "star_wars")
#' pivot_vars2 <- get_pivot_vars(df2)
#'
get_pivot_vars <- function(df, max_levels = 1000){
     df %>%
     summarise_all(n_distinct) %>%
     collect() %>%
     tidyr::gather("field", "n_levels") %>%
     filter(n_levels < max_levels) %>%
     mutate(levels = purrr::map(field, ~pull(distinct(select(df, .))))) %>%
     mutate(description = field)
}


# internal function to be used inside create_pivot_ui
create_source_vars_accordion <- function(pivot_vars_df, ns){

        stopifnot(nrow(pivot_vars_df) >= 1)

        df <- pivot_vars_df %>%
                select(group, field) %>%
                tidyr::nest(field) %>%
                mutate(id = paste0("source_vars_", row_number())) %>%
                mutate(id = ns(id))

        well_ids <- c(df$id, ns("row_vars"), ns("col_vars"))

        # create one line of pipe
        f <- function(group, id, data) {
                rlang::expr(bsplus::bs_append(title = !!group, content = shinyjqui::orderInput(!!id, label = NULL, items = !!data$field, connect = !!well_ids)))
        }

        df %>%
                mutate(e = purrr::pmap(list(group, id, data), f)) %>%
                pull(e) %>%
                c(rlang::expr(bsplus::bs_set_opts(panel_type = "default", use_heading_link = TRUE)), .) %>%
                c(rlang::expr(bsplus::bs_accordion(id = ns("source_vars"))), .) %>%
                purrr::reduce(function(x, y) rlang::expr(!!x %>% !!y)) %>%
                rlang::eval_tidy()
}


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
pivot_module_UI <- function(id, pivot_vars, sum_vars = ""){
        # browser()
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
                fluidRow(column(12, wellPanel(create_source_vars_accordion(pivot_vars, ns)))),
                tags$p(uiOutput(ns("filter_text"))),
                fluidRow(
                     column(3,
                            fluidRow(column(6, actionButton(ns("refresh"), "Refresh")),
                                     column(6, downloadButton(ns("download_data")))),
                            fluidRow(column(9, selectInput(ns("summary_var"), "Summarise by:", choices = c("Count", sum_vars), selected = "Count")),
                                     column(3, actionButton(ns("plot"), "Plot")))
                     ),
                     column(9, wellPanel(shinyjqui::orderInput(ns("col_vars"), "Columns", items = NULL, placeholder = "Drag variables here", width = "100%", connect = c(ns("source_vars"), ns("row_vars")))))
                ),
                fluidRow(
                     column(3, wellPanel(shinyjqui::orderInput(ns("row_vars"), "Rows", items = NULL, placeholder = "Drag variables here", width = "100%", connect = c(ns("source_vars"), ns("col_vars"))))),
                     column(9, tags$div(style = "overflow:auto", DT::dataTableOutput(ns("table"))))
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
#' @param join_table A reactive expression returned from another pivot module. Used to implement linked pivot tables.
#' @param join_by The primary key variable to perfrorm a filtering join as a string. Must be in both pivot tables to be linked. Only needed if using linked pivot tables.
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
pivot_module <- function(input, output, session, ns_id, df, pivot_vars, record_limit = 1e6, join_table = NULL, join_by = NA, show_filter_text = T){
     require(esquisse)

     stopifnot(is.null(join_table) | is.reactive(join_table))

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

     filter_expr <- reactive({
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

     # observe(print(filter_expr()))

     # maybe use event reactive and update button
     filtered_data <- eventReactive(input$refresh, {
          df %>%
               {if(!is.null(filter_expr)) filter(., !!!filter_expr()) else .} %>%  # conditional pipe
               {if(!is.null(join_table) & !is.na(join_by)) semi_join(., join_table(), by = join_by) else .}
     })

     summarised_data <- eventReactive(input$refresh, {
          grp_vars <- rlang::parse_quosures(paste0(c(input$row_vars_order, input$col_vars_order), collapse = ";"))
          filtered_data() %>%
               group_by(!!!grp_vars) %>%
               # another conditional pipe to do the summarization
               {if(input$summary_var == "Count") summarise(., n = n()) else summarise(., n = sum(!!as.name(input$summary_var), na.rm = T))} %>%
               ungroup()
     })

     local_table <- eventReactive(input$refresh, {
          summarised_data() %>%
               head(record_limit) %>%
               # # filter(between(row_number(), 1, record_limit)) %>%
               collect() %>%
               mutate_if(~class(.) == "integer64", as.numeric)
     })


     display_table <- eventReactive(input$refresh, {
          local_table() %>%
               {
                    if(length(input$col_vars_order) > 0 ){
                         tidyr::unite(., "col_var", input$col_vars_order, sep = "_&_") %>%
                              tidyr::spread(col_var, n, fill = 0)
                    } else .
               }
     })

     observeEvent(input$plot, {
          data_r <- reactiveValues(data = local_table(), name = "df")
          callModule(module = esquisserServer, id = "esquisse", data = data_r)

          showModal(modalDialog(tags$div(
               style = "height: 700px;", # needs to be in fixed height container
               esquisse::esquisserUI(
                    id = ns("esquisse"),
                    header = FALSE, # dont display gadget title
                    choose_data = FALSE # dont display button to change data
               )
          ), easyClose = T))
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
          if(nrow(local_table()) == record_limit){
               return(glue::glue("Warning: Only showing first {record_limit} rows."))
          } else return(NULL)
     })

     output$table <- DT::renderDataTable({
          df <- display_table()
          df %>%
               DT::datatable(rownames = FALSE) %>%
               DT::formatRound(1:ncol(df), digits = 0)
     })

     output$filter_text <- renderUI({
          req(filter_expr())
          req(show_filter_text)

          filter_expr() %>%
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
     output$debug_text <- renderText(input$summary_var)
     output$download_data <- downloadHandler(
          filename = function() paste0("data_", Sys.Date(), ".csv"),
          content = function(file) readr::write_csv(display_table(), file),
          contentType = "text/csv"
     )
     return(filtered_data)
} # end server











