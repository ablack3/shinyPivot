library(shiny)
library(shinyjqui)
library(tidyverse)
source("shiny_pivot_module.R")

options(shiny.reactlog=T) # command f3 + right arrow

df <- starwars %>% 
     select_if(is.character)

pivot_vars <- get_pivot_vars(df)


ui <- fluidPage(title = "R pivot table", 
     shiny_pivot_module_UI(id = "id", pivot_vars = pivot_vars)
)


server <- function(input, output, session){
     callModule(shiny_pivot_module, "id", df = df, pivot_vars = pivot_vars)     
} # end server

shinyApp(ui = ui, server = server)










