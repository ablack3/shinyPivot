
# generate the expression
library(tidyverse)

create_source_vars_accordion <- function(pivot_vars_df){

    stopifnot(nrow(pivot_vars_df) >= 1)

    df <- pivot_vars_df %>%
        select(group, field) %>%
        tidyr::nest(field) %>%
        mutate(id = paste0("source_vars_", row_number()))

    well_ids <- c(df$id, "row_vars", "col_vars")

    f <- function(group, id, data) {
        rlang::expr(bsplus::bs_append(title = !!group, content = shinyjqui::orderInput(!!id, label = NULL, items = !!data$field, connect = !!well_ids)))
    }

    df %>%
        mutate(e = pmap(list(group, id, data), f)) %>%
        pull(e) %>%
        c(rlang::expr(bsplus::bs_set_opts(panel_type = "default", use_heading_link = TRUE)), .) %>%
        c(rlang::expr(bsplus::bs_accordion(id = "source_vars")), .) %>%
        purrr::reduce(function(x, y) rlang::expr(!!x %>% !!y)) %>%
        rlang::eval_tidy()
}


library(shiny)
library(bsplus)

well_ids <- c(paste0("source_vars_", 1:length(unique(pivot_vars1$group))), "row_vars", "col_vars")

ui <- fluidPage(
    # fluidRow(column(12, #wellPanel(
    #     bs_accordion(id = "source_vars") %>%
    #         bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
    #         bs_append(title = "Patient Variables", content = shinyjqui::orderInput("source_vars1", NULL, items = letters[1:5]), connect = well_ids) %>%
    #         bs_append(title = "Encounter Variables", content = shinyjqui::orderInput("source_vars2", NULL, items = letters[6:10], connect = well_ids))
    #
    #     # shinyjqui::orderInput(ns("source_vars"), "Variables", items = pivot_vars$field, width = "100%", connect = c(ns("row_vars"), ns("col_vars")))
    # )),
    fluidRow(column(12, create_source_vars_accordion(pivot_vars1))),

        fluidRow(
            column(3,
                   fluidRow(column(6, actionButton("refresh", "Refresh")),
                            column(6, downloadButton("download_data"))),
                   fluidRow(column(9, selectInput("summary_var", "Summarise by:", choices = c("Count", "sum_vars"), selected = "Count")),
                            column(3, actionButton("plot", "Plot")))
            ),
            column(9, wellPanel(shinyjqui::orderInput("col_vars", "Columns", items = NULL, placeholder = "Drag variables here", width = "100%", connect = well_ids)))
        ),
        fluidRow(
            column(3, wellPanel(shinyjqui::orderInput("row_vars", "Rows", items = NULL, placeholder = "Drag variables here", width = "100%", connect = well_ids))),
            column(9, tags$div(style = "overflow:auto", DT::dataTableOutput("table")))
        )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$table <- DT::renderDataTable(gt::gtcars)
}

# Run the application
shinyApp(ui = ui, server = server)



# library(shiny)
# library(dqshiny)
# titles <- c("Section 1", "Section 2", "Section 3")
# contents <- list("Lorem ipsum..", "Lorem ipsum..", tags$p("Lorem ipsum.."))
# shinyApp(
#     ui = fluidPage(
#         fluidRow(
#             column(5, dq_accordion("myAccordion", titles, contents, hover = FALSE,
#                                    style = "border:1px solid red;margin-top: 5px;color: red;"
#             ), dq_space(),
#             dq_accordion("myAccordion2", titles, contents,
#                          bg_color = NULL, options = list(animate = 500, collapsible = TRUE),
#                          icons = c(open = "hand-point-down", closed = "hand-point-right")
#             ), dq_space(),
#             dq_accordion("myAccordion3", titles, contents,
#                          bg_color = "pink", icons = NULL, sortable = TRUE
#             ))
#         )
#     ), server = function(input, output) {
#         observeEvent(input$myAccordion, print(input$myAccordion))
#     }
# )


