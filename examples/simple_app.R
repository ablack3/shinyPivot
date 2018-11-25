library(shiny)
devtools::load_all()
# options(shiny.reactlog=T) # command f3 + right arrow

# local table
library(dplyr)
df1 <- starwars %>%
     select_if(~is.character(.)|is.numeric(.))

pivot_vars1 <- get_pivot_vars(df1)

sum_vars <- c("height", "mass")

ui <- fluidPage(title = "R pivot table", #theme = shinythemes::shinytheme("superhero"),
                tabsetPanel(
                     tabPanel(   "Local pivot", pivot_module_UI(id = "id1", pivot_vars = pivot_vars1, sum_vars = sum_vars))
                )
)

server <- function(input, output, session){
     callModule(pivot_module, id = "id1", ns_id = "id1", df = df1, pivot_vars = pivot_vars1, record_limit = 20)
} # end server

shinyApp(ui = ui, server = server)

# linked pivot table idea?







