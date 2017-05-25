library(shiny)
library(leaflet)
library(magrittr)


ui <- fluidPage(
   
   # Application title
   titlePanel("Map of the World"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("latitude", label = "Latitude", min = -90, max = 90, value = 0),
         numericInput("longitude", label = "Latitude", min = -180, max = 180, value = 0)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("mymap")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$mymap <- renderLeaflet({
      leaflet() %>%
       addTiles() %>%
       addMarkers(lng = input$longitude, lat = input$latitude, popup = "Location")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

