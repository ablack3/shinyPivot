
# adding updating of ui draggable elements based on tree selections

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

# connect_ids <- c("filter_vars", "group_vars")


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
     includeCSS("styles.css"),

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
                fluidRow(wellPanel(uiOutput("filter_vars"))),
                fluidRow(wellPanel(uiOutput("group_vars")))
          ),
          column(9, tags$div(style = "overflow:auto", DT::dataTableOutput("table")))
     ),
     verbatimTextOutput("show_vars_print"),
     verbatimTextOutput("filter_vars_print"),
     verbatimTextOutput("group_vars_print"),
     verbatimTextOutput("str")

)




server <- function(input, output) {

     # populate with variables selected in tree but not in filter vars
     output$filter_vars <- renderUI({
          shinyjqui::orderInput("filter_vars", "Filter Variables", items = show_vars(), width = "100%", connect = c("filter_vars", "group_vars"))
     })

     output$group_vars <- renderUI({
                show_vars()
             # print("rendering group vars ui")
             shinyjqui::orderInput("group_vars", "Filter Variables", items = NULL, width = "100%", connect = c("filter_vars", "group_vars"))
     })


        # Toggle the var tree
     observeEvent(input$toggleTree, {
          shinyjs::toggle(id = "tree_panel")
     })
     output$table <- DT::renderDataTable(gt::gtcars)

     output$tree <- renderTree({
          varlist
     })

     # show_vars contains the variables selected in the tree
     show_vars <- reactive({
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

     output$show_vars_print <- renderPrint(show_vars())
     output$str <- renderPrint(input$tree)
     output$filter_vars_print <- renderPrint(input$filter_vars_order)
     output$group_vars_print <- renderPrint(input$group_vars_order)
     # output$str <- renderPrint(input$filter_vars_order)
}

# Run the application
shinyApp(ui = ui, server = server)



