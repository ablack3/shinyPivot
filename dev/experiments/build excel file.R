#+++++++++++++++++++++++++++
# xlsx.writeMultipleData
#+++++++++++++++++++++++++++++
# file : the path to the output file
# ... : a list of data to write to the workbook
xlsx.writeMultipleData <- function (file, ...){
     require(xlsx, quietly = TRUE)
     objects <- list(...)
     fargs <- as.list(match.call(expand.dots = TRUE))
     objnames <- as.character(fargs)[-c(1, 2)]
     nobjects <- length(objects)
     for (i in 1:nobjects) {
          if (i == 1)
               write.xlsx(objects[[i]], file, sheetName = objnames[i], row.names = F)
          else write.xlsx(objects[[i]], file, sheetName = objnames[i], row.names = F, append = TRUE)
     }
}

# This function is inspired from the one published on statmethods website
#
# The function xlsx.writeMultipleData works for data frames, matrices, time series, and tables.

# Example of usage :

     # Use the R code below to save mtcars (a data frame), Titanic (a table), AirPassengers (a time series) and state.x77 (a matrix) :

xlsx.writeMultipleData("myworkbook.xlsx", mtcars, Titanic, AirPassengers, state.x77)



tf <- tempfile(fileext = ".xlsx")
tf
# xlsx.writeMultipleData(tf, mtcars, Titanic, AirPassengers, state.x77)

write_xlsx_multiple(tf, mtcars, Titanic, AirPassengers, state.x77)
file.copy(tf, "asdf.xlsx")
