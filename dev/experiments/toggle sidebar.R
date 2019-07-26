library(shiny)
library(shinyjs)

ui <- fluidPage(
     useShinyjs(),
     fluidPage("",
                fluidRow("tab",
                         div( id ="Sidebar", column(1, wellPanel())),
                         # div( id ="Sidebar",sidebarPanel()),


                         fluidRow(actionButton("toggleSidebar", "Toggle sidebar")
                         )
                )
     )
)

server <-function(input, output, session) {
     observeEvent(input$toggleSidebar, {
          shinyjs::toggle(id = "Sidebar")
     })
}

shinyApp(ui, server)
