

library(shiny)
library(shinyBS)
shinyApp(
     ui =
          fluidPage(
                         bsTooltip("", ""),
                         actionButton("button1", "Button")
          ),
     server =
          function(input, output, session) {
               addTooltip(session, "button1", "asdf;alsdkjfa;sldf")
          }
)
