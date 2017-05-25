library(shiny)
library(ggplot2)
library(plotly)


ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlotly({
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      ggplotly(
        ggplot(faithful) +
          geom_histogram(aes(waiting), fill = "blue", breaks = bins) +
          labs(title = "Histogram of Waiting Times for Old Faithful Eruptions",
               x = "Waiting Time (mins)")
      )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

