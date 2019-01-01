write_xlsx_multiple <- function (file, ...){
     temp_file <- tempfile(fileext = ".xlsx")

     require(xlsx, quietly = TRUE)
     objects <- list(...)
     fargs <- as.list(match.call(expand.dots = TRUE))
     objnames <- stringr::str_remove_all(as.character(fargs)[-c(1, 2)],  "\\(|\\)")
     nobjects <- length(objects)
     for (i in 1:nobjects) {
          if (i == 1)
               xlsx::write.xlsx(objects[[i]], temp_file, sheetName = objnames[i], row.names = T)
          else xlsx::write.xlsx(objects[[i]], temp_file, sheetName = objnames[i], row.names = T, append = TRUE)
     }

     file.copy(temp_file, file)
}
