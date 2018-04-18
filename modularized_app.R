library(shiny)
library(shinyjqui)
library(tidyverse)
source("shiny_pivot_module.R")

options(shiny.reactlog=T) # command f3 + right arrow

# local table
df <- starwars %>% 
     select_if(is.character)



# using a database
# con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
# con <-  DBI::dbConnect(RPostgreSQL::PostgreSQL(), user="adamblack", password="",
#                        host="localhost", port=5432, dbname="test")
# copy_to(con, df, "star_wars")
# df <- tbl(con, "star_wars")

pivot_vars <- get_pivot_vars(df)



ui <- fluidPage(title = "R pivot table", 
     shiny_pivot_module_UI(id = "id", pivot_vars = pivot_vars)
)


server <- function(input, output, session){
     callModule(shiny_pivot_module, "id", ns_id = "id", df = df, pivot_vars = pivot_vars)     
} # end server

shinyApp(ui = ui, server = server)










