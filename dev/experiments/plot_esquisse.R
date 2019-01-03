# devtools::install_github("dreamRs/esquisse")
library(shiny)
library(esquisse)

ui <- fluidPage(
     tags$h1("Use esquisse as a Shiny module"),
     tags$div(
          style = "height: 700px;", # needs to be in fixed height container
          esquisserUI(
               id = "esquisse",
               header = FALSE, # dont display gadget title
               choose_data = FALSE # dont display button to change data
          )
     )
)

server <- function(input, output, session) {

     data_r <- reactiveValues(data = iris, name = "iris")

     callModule(module = esquisserServer, id = "esquisse", data = data_r)

}

shinyApp(ui, server)


ui <- fluidPage(
     tags$h1("Use esquisse as a Shiny module"),
     selectInput("species", "Species", choices = unique(iris$Species), multiple = T),
     actionButton("plot", "plot"),
     dataTableOutput("table")

)

server <- function(input, output, session) {

     data_filtered <- reactive(filter(iris, Species %in% input$species))
# print(class(data_filtered))
     observeEvent(input$plot, {
          data_r <- reactiveValues(data = data_filtered(), name = "df")
          # data_r <- reactiveValues(data = iris, name = "df")
          callModule(module = esquisserServer, id = "esquisse", data = data_r)

          showModal(modalDialog(tags$div(
               style = "height: 700px;", # needs to be in fixed height container
               esquisserUI(
                    id = "esquisse",
                    header = FALSE, # dont display gadget title
                    choose_data = FALSE # dont display button to change data
               )
          ), easyClose = T))
     })

     output$table <- renderDataTable(data_filtered())
}

shinyApp(ui, server)

