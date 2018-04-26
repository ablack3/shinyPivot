# test linkage of pivot tables

library(shiny)
library(dplyr)
library(wakefield)

set.seed(1)
person_df <- r_data_frame(100, id, age, sex)
cars_df <- r_data_frame(1000, car, year)
cars_df$ID <- sample(person_df$ID, 1000, replace = T)


options(shiny.reactlog=T) # command f3 + right arrow

# using a database
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
# con <-  DBI::dbConnect(RPostgreSQL::PostgreSQL(), user="adamblack", password="",
# host="localhost", port=5432, dbname="test")

copy_to(con, person_df, "person_df")
copy_to(con, cars_df, "cars_df")

df1 <- tbl(con, "person_df")
df2 <- tbl(con, "cars_df")

pivot_vars1 <- select(df1, -ID) %>% get_pivot_vars()
pivot_vars2 <- select(df2, -ID) %>% get_pivot_vars()

ui <- fluidPage(title = "R pivot table", #theme = shinythemes::shinytheme("superhero"),
                tabsetPanel(
                     tabPanel("Database pivot 1", pivot_module_UI(id = "id1", pivot_vars = pivot_vars1)),
                     tabPanel("Database pivot 2", pivot_module_UI(id = "id2", pivot_vars = pivot_vars2))
                )
)

server <- function(input, output, session){
     tbl_df1 <- callModule(pivot_module, id = "id1", ns_id = "id1", df = df1, pivot_vars = pivot_vars1)
            callModule(pivot_module, id = "id2", ns_id = "id2", df = df2, pivot_vars = pivot_vars2, join_table = tbl_df1, join_by = "ID")
     }

shinyApp(ui = ui, server = server)

# linked pivot table idea?







