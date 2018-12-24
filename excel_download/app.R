library(shiny)

xlsx.writeMultipleData <- function (file, ...){
     temp_file <- tempfile(fileext = ".xlsx")

     require(xlsx, quietly = TRUE)
     objects <- list(...)
     fargs <- as.list(match.call(expand.dots = TRUE))
     objnames <- as.character(fargs)[-c(1, 2)]
     nobjects <- length(objects)
     for (i in 1:nobjects) {
          if (i == 1)
               write.xlsx(objects[[i]], temp_file, sheetName = objnames[i])
          else write.xlsx(objects[[i]], temp_file, sheetName = objnames[i], append = TRUE)
     }

     file.copy(temp_file, file)
}

# xlsx.writeMultipleData("adam.xlsx", mtcars, Titanic, AirPassengers, state.x77)

ui <- fluidPage(
     downloadButton("download_data")
)

server <- function(input, output) {

     output$download_data <- downloadHandler(
          filename = function() paste0("data_", Sys.Date(), ".xlsx"),
          content = function(file) xlsx.writeMultipleData(file, mtcars, Titanic, AirPassengers, state.x77)
     )
}

shinyApp(ui = ui, server = server)

