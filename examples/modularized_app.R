library(shiny)
options(shiny.reactlog=T) # command f3 + right arrow

# local table
library(dplyr)
df1 <- starwars %>%
     select_if(~is.character(.)|is.numeric(.))


# using a database
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
# con <-  DBI::dbConnect(RPostgreSQL::PostgreSQL(), user="adamblack", password="",
                       # host="localhost", port=5432, dbname="test")
copy_to(con, df1, "star_wars")
df2 <- tbl(con, "star_wars")

pivot_vars1 <- get_pivot_vars(df1)
pivot_vars2 <- get_pivot_vars(df2)

sum_vars <- c("height", "mass")

ui <- fluidPage(title = "R pivot table", #theme = shinythemes::shinytheme("superhero"),
     tabsetPanel(
          tabPanel(   "Local pivot", pivot_module_UI(id = "id1", pivot_vars = pivot_vars1, sum_vars = sum_vars)),
          tabPanel("Database pivot", pivot_module_UI(id = "id2", pivot_vars = pivot_vars2, sum_vars = sum_vars[1]))
     )
)

server <- function(input, output, session){
     callModule(pivot_module, id = "id1", ns_id = "id1", df = df1, pivot_vars = pivot_vars1, record_limit = 20)
     callModule(pivot_module, id = "id2", ns_id = "id2", df = df2, pivot_vars = pivot_vars2, record_limit = 30)
} # end server

shinyApp(ui = ui, server = server)

# linked pivot table idea?







