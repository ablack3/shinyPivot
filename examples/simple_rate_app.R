library(shiny)
library(here)
devtools::load_all()
# options(shiny.reactlog=T) # command f3 + right arrow
# source(here("dev/tree_ui/pivot_module_tree.R"))

# local table
library(dplyr)
df1 <- starwars %>%
     select_if(~is.character(.)|is.numeric(.))


pivot_vars1 <- get_pivot_vars(df1) %>%
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


sum_vars <- c("height", "mass")

ui <- fluidPage(title = "R pivot table",
                tabsetPanel(
                     tabPanel("Local pivot", pivot_tree_module_UI(id = "id1", pivot_vars = pivot_vars1, sum_vars = sum_vars))
                )
)

server <- function(input, output, session){
     callModule(pivot_tree_module, id = "id1", ns_id = "id1", df = df1, pivot_vars = pivot_vars1, record_limit = 20)
} # end server

shinyApp(ui = ui, server = server)

# linked pivot table idea?







