# options(shiny.reactlog=T) # command f3 + right arrow

library(shiny)
library(wakefield)
library(dplyr)


# local table
denom <- r_data_frame(10000, age(), gender(), year(x = 2015:2017), month(), elig = runif())
numerator <- r_data_frame(1e5, age(), gender(), year(x = 2015:2017), month(), military(), internet_browser(), height(), income())

# using a database
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")

copy_to(con, denom, "denom")
denom_db <- tbl(con, "denom")

copy_to(con, numerator, "numerator")
numerator_db <- tbl(con, "numerator")

pivot_vars1 <- numerator %>%
     select(-Height, -Income) %>%
     get_pivot_vars(max_levels = 1e7)

sum_vars <- c("Height", "Income")

ui <- fluidPage(title = "R pivot rate table", #theme = shinythemes::shinytheme("superhero"),
                tabsetPanel(
                     tabPanel("Local pivot",    pivot_rate_module_UI(id = "id1", pivot_vars = pivot_vars1, sum_vars = sum_vars)),
                     tabPanel("Database pivot", pivot_rate_module_UI(id = "id2", pivot_vars = pivot_vars1, sum_vars = sum_vars))
                )
)

server <- function(input, output, session){
     callModule(pivot_rate_module, id = "id1", ns_id = "id1", numer_df = numerator,    denom_df = denom,    pivot_vars = pivot_vars1, record_limit = 1e6)
     callModule(pivot_rate_module, id = "id2", ns_id = "id2", numer_df = numerator_db, denom_df = denom_db, pivot_vars = pivot_vars1, record_limit = 1e6)
} # end server

shinyApp(ui = ui, server = server)




