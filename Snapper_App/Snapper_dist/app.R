library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

snapper <- 
  read.csv("C:/Users/Annie/Documents/Data+/dataplus/Snapper_App/Snapper_dist/SnapperHG1.csv")

ui <- fluidPage(
  # Application title
  titlePanel("Snapper Data for Two Survey Locations"),
  
  #Creating different tabs
  tabsetPanel(
    tabPanel("Distribution of Lengths",
             sliderInput("bins", 
                         "Number of bins: ",
                         min = 1,
                         max = 50,
                         value = 30),
             plotlyOutput("lengthdist")),
    #Distribution of snapper lengths tab with a plot output 
    
    tabPanel("Age Versus Length By Location",
             selectInput("location",
                          "Survey location: ",
                           c("Location KAH8810" = "KAH8810",
                             "Location KAH0012" = "KAH0012")),
             plotlyOutput("scatter")),
    #Tab for age of the snapper versus its length by location, with a plot output
    
    tabPanel("Age Versus Length: Both Locations",
             plotlyOutput("scatter2"))
    #Tab for the age versus the length of the snapper in both locations, with a plot output
  )
)

# Define server logic required to draw graphs
server <- function(input, output) {
   
  #Output a histogram of the distribution of snapper lengths
  output$lengthdist <- renderPlotly({
    x = snapper[, "len"]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #the number of bins in the histogram are adjustable based on the "bins" input
    
    # draw the histogram with the specified number of bins
    ggplotly(
      ggplot(snapper) +
        geom_histogram(aes(len, fill = survey), breaks = bins, linetype = 1, color = "black") +
        labs(title = "Histogram of Snapper Lengths",
             x = "Length (cm)") +
        scale_fill_discrete(name = "Location")
    )
    })
    
    #create a reactive function for the location of the fish
    snapper_loc <- reactive({input$location})
    
    #creates a scatterplot that depends on the input of the location
    output$scatter <- renderPlotly({
      ggplotly(
        ggplot(filter(snapper, survey == snapper_loc()), mapping = aes(x = age, y = len)) +
          geom_point(shape = 21, fill = "blue", position = "jitter") +
          labs(title = paste("Snapper Age vs. Length at", snapper_loc()),
               x = "Age (years)",
               y = "Length (cm)")
          
      )
    })
    
    #creates a scatterplot that does not depend on the location of the fish
    output$scatter2 <- renderPlotly({
      ggplotly(
        ggplot(snapper, mapping = aes(x= age, y = len, fill = survey))+
          geom_point(position = "jitter") +
          labs(title = "Snapper Age vs. Length at Both Locations",
               x = "Age (years)",
               y = "Length (cm)") +
          scale_fill_discrete(name = "Location")
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

