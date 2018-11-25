library(shiny)
library(dplyr)

baby_names <- babynames::babynames %>%
     distinct(name) %>%
     pull(name) %>%
     sort()

ui <- fluidPage(
     selectInput("babyname", "Baby Name", multiple = TRUE, choices = character(0))
)

server <- function(input, output, session) {
     updateSelectizeInput(session, "babyname", choices = baby_names, server = TRUE)
}

shinyApp(ui, server)
