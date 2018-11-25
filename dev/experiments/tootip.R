

library(shiny)
library(shinyBS)
shinyApp(
     ui =
          fluidPage(
               sidebarLayout(
                    sidebarPanel(
                         sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 30),
                         bsTooltip("bins", "The wait times will be broken into this many equally spaced bins",
                                   "right", options = list(container = "body"))
                    ),
                    mainPanel(
                         plotOutput("distPlot"),
                         actionButton("button1", "Button")
                    )
               )
          ),
     server =
          function(input, output, session) {
               output$distPlot <- renderPlot({

                    # generate bins based on input$bins from ui.R
                    x    <- faithful[, 2]
                    bins <- seq(min(x), max(x), length.out = input$bins + 1)

                    # draw the histogram with the specified number of bins
                    hist(x, breaks = bins, col = 'darkgray', border = 'white')

               })
               addTooltip(session, "distPlot", "Hi there")
               addTooltip(session, "button1", "asdf;alsdkjfa;sldf")
          }
)
