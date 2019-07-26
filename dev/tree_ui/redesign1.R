
library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyTree)

df1 <- starwars %>%
     select_if(~is.character(.)|is.numeric(.))

devtools::load_all()

pivot_vars <- get_pivot_vars(df1) %>%
     mutate(description = case_when(
          field == "name" ~ "The name of the character",
          field == "height" ~ "The height of the character",
          T ~ field
     )) %>%
     mutate(group = case_when(
          stringr::str_detect(field, "color") ~ "Color fields",
          field %in% c("homeworld", "species", "name", "gender") ~ "Demographic fields",
          T ~ "Other fields"
     )) %>%
     mutate(field_display_name = tools::toTitleCase(gsub("_", " ", field)))

create_varlist <- function(pivot_vars){
     tmp <- select(pivot_vars, group, field, field_display_name)
     tmp %>%
          nest(-group) %>%
          pull(data) %>%
          map(function(df){
               map(df$field, ~structure("", varname = .)) %>%
                    purrr::set_names(df$field_display_name)
          }) %>%
          purrr::set_names(unique(tmp$group))
}


ns <- function(x) x

connect_ids <- c("filter_vars", "group_vars")


# varlist <- list(
#      Demographic_fields = list(name = "Name", gender = "Gender", homeworld = "Homeworld"),
#      Other_fields = list(height = "Height", mass = "Mass", birth_year = "Birth year"),
#      Color_fields = list(hair_color = "Hair color", skin_color = "Skin color", eye_color = "Eye color"))

# need a function to produce this list
# varlist <- list(
#      "Demographic fields" = list("Name" = structure("", varname = "name"),
#                                  "Gender" = structure("", varname = "gender"),
#                                  "Homeworld" = structure("", varname = "homeworld")),
#      "Other fields" = list("Height" = structure("", varname = "height"),
#                            "Mass" = structure("", varname = "mass"),
#                            "Birth year" = structure("", varname = "birth_year")),
#      "Color fields" = list("Hair color" = structure("", varname = "hair_color"),
#                            "Skin color" = structure("", varname = "skin_color"),
#                            "Eye color" = structure("", varname = "eye_color")))

varlist <- create_varlist(pivot_vars)




ui <- fluidPage(
     useShinyjs(),

     tags$p(uiOutput(ns("filter_text"))),

     # buttons
     fluidRow(
          column(1, actionButton("toggleTree", "Show/Hide tree")),
          column(1, actionButton("refresh", "Refresh")),
          column(1, downloadButton("download_data")),
          column(3, selectInput("summary_var", "Summarise by:", choices = c("Count", "sum_vars"), selected = "Count"))
     ),


     fluidRow(
          div(id = "tree_panel", column(2, wellPanel(
               shinyTree::shinyTree("tree", checkbox = T, search = T, theme = "proton", themeIcons = F, unique = T )
          ))),
          column(1,
                 fluidRow(wellPanel(
                      uiOutput("filter_vars")
                      # shinyjqui::orderInput("filter_vars", "Filter Variables", items = NULL, width = "100%", connect = connect_ids)
                 )),
                 fluidRow(wellPanel(shinyjqui::orderInput("group_vars", "Rows", items = NULL, placeholder = "Drag variables here", width = "100%", connect = connect_ids)))
          ),
          column(9, tags$div(style = "overflow:auto", DT::dataTableOutput("table")))
     ),
     verbatimTextOutput("show_vars"),
     verbatimTextOutput("str")

)




server <- function(input, output) {

     output$filter_vars <- renderUI({
          # cities <- getNearestCities(input$lat, input$long)
          # checkboxGroupInput("cities", "Choose Cities", cities)
          shinyjqui::orderInput("filter_vars", "Filter Variables", items = letters[1:5], width = "100%", connect = connect_ids)
     })

     observeEvent(input$toggleTree, {
          shinyjs::toggle(id = "tree_panel")
     })
     output$table <- DT::renderDataTable(gt::gtcars)

     output$tree <- renderTree({
          varlist
     })

     output$show_vars <- reactive({
          print("running...")
          l <- input$tree
          res <- character()
          for(i in seq_along(l)) {
               for(j in seq_along(l[[i]])){
                    selected <- attr(l[[i]][[j]], "stselect")
                    varname <- attr(l[[i]][[j]], "varname")
                    if(!is.null(selected)) if(selected == T) res <- c(res, varname)
                    # if(!is.null(selected)) if(selected == T) print(varname)
               }
          }
          res
     })

     output$str <- renderPrint(input$tree)
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


