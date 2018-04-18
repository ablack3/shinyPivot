# shinyPivot - R package under construction
A pivot table module for Shiny built on dplyr

This is an R package contianing three functions that allow for easy construction of pivot tables using Shiny.
It is built on dplyr so it should be able to be used with local dataframes and remote database connections (tbl_dbi objects from the dbplyr package). 
When a remote database table is used the summarization and filtering take place in the database and only the result is brought into R.
This allows for pivot tables that work on large datasets housed in fast column oriented databases.

The package contains three functions 
- get_pivot_vars
- pivot_module_UI
- pivot_module

The user should be familiar with shiny modules. https://shiny.rstudio.com/articles/modules.html

```
library(shiny)

# local table
library(dplyr)
df1 <- starwars %>%
     select_if(is.character)

# using a database
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
copy_to(con, df1, "star_wars")
df2 <- tbl(con, "star_wars")

pivot_vars1 <- get_pivot_vars(df1)
pivot_vars2 <- get_pivot_vars(df2)

ui <- fluidPage(title = "R pivot table", 
     tabsetPanel(
          tabPanel(   "Local pivot", pivot_module_UI(id = "id1", pivot_vars = pivot_vars1)),
          tabPanel("Database pivot", pivot_module_UI(id = "id2", pivot_vars = pivot_vars2))
     )
)

server <- function(input, output, session){
     callModule(pivot_module, id = "id1", ns_id = "id1", df = df1, pivot_vars = pivot_vars1, record_limit = 20)
     callModule(pivot_module, id = "id2", ns_id = "id2", df = df2, pivot_vars = pivot_vars2, record_limit = 30)
} 

shinyApp(ui = ui, server = server)
```








