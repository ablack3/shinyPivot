#in global.R
# options (scipen=FALSE,stringsAsFactors=FALSE)
library(xlsx)
library(RODBC)
library(dplyr)

get_DW <- function (mydf) {
     mydf
}

shinyUI(
     fluidPage(
          titlePanel("POC"),
          sidebarLayout(
               sidebarPanel(

                    fileInput(inputId = 'file1',label =  'Choose An Excel File',
                              accept=c('.xlxs')),

                    radioButtons(inputId = "radio", label = "Search By:",
                                 choices = list("option 1" = 1,
                                                "option 2" = 2,
                                                "option 3" = 3),
                                 selected = 1),

                    hr(),

                    fluidRow(column(1,
                                    actionButton("go", "Get Records")),
                             column(2,offset = 2,
                                    downloadButton('downloadData', 'Download')),
                             br()
                    )),

               mainPanel(
                    verbatimTextOutput("Inputnum"),
                    br(),
                    verbatimTextOutput("Outputnum")
               )
          )
     ))
# Server Code
server <- shinyServer(function(input, output) {

     # Create a reactive expression which will grab the data
     # We pass that to the outputs reactive element to the outputs
     data <- reactive({
          iris
     })

     # Return the number of records
     output$Inputnum <- renderText({
          paste(nrow(data()), "records to be checked")
     })

     # Data returned by Database
     en_data <- eventReactive(input$go, {
          get_DW(data())
     })

     # Return the number of records
     output$Outputnum <- renderText({
          paste(nrow(en_data()), "records matched")
     })

     output$downloadData<- downloadHandler(
          filename = function() { "name.xlsx" },

          content = function(file) {
               tempFile <- tempfile(fileext = ".xlsx")
               write.xlsx(en_data(), tempFile)
               file.rename(tempFile, file)
          })
})
