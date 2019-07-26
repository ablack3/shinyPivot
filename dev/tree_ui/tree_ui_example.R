
library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Mike Schaffer \email{mschaff@gmail.com}
ui <- shinyUI(
     pageWithSidebar(
          # Application title
          headerPanel("shinyTree with node selection using node stids and stclass labels"),

          sidebarPanel(
               shinyTree("tree", checkbox=TRUE, theme="proton")
          ),
          mainPanel(
               "Currently Selected:",
               verbatimTextOutput("selTxt"),
               hr(),
               "Full Tree:",
               verbatimTextOutput("treeTxt")
          )
     ))

library(shiny)
library(shinyTree)

#' Define server logic required to generate a simple tree
#' @author Mike Schaffer \email{mschaff@gmail.com}
server <- shinyServer(function(input, output, session) {
     log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))

     output$tree <- renderTree({
          structure(
               list(
                    'Project A' = structure(
                         list(
                              'Study A-A'=structure('',stid=1,stclass='study'),
                              'Study A-B'=structure('',stid=2,stclass='study')
                         ),
                         stid=1,
                         stopened=TRUE,
                         stclass='project'
                    ),
                    'Project B' = structure(
                         list(
                              'Study B-A'=structure('',stid=3,stclass='study'),
                              'Study B-B'=structure('',stid=4,stclass='study')
                         ),
                         stid=2,
                         stopened=TRUE,
                         stclass='project'
                    )
               ),
               stopened=TRUE
          )
     })

     output$selTxt <- renderPrint({
          tree <- input$tree
          if (is.null(tree)){
               "None"
          } else{
               #cat( attr(get_selected(input$tree, format="slices"),"id" ) )
               # names(as.data.frame(get_selected(input$tree, format = "slices")))

               str(get_selected(input$tree, format = "classid"))
          }
     })

     output$treeTxt <- renderPrint({
          tree <- input$tree
          if (is.null(tree)){
               "None"
          } else{
               str(input$tree)
          }
     })
})




shinyApp(ui, server)

