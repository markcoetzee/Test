library(shiny)
library(leaflet)

#Load library
library(raster)
library(rgdal)

library('sf')

# Load shapefile
income2013 <- read_sf("age-and-income-in-2013-by-talb2013.shp")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  


    leafletOutput("mymap"),
    

  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  output$mymap <- renderLeaflet({
    mypal <- colorQuantile(palette = "RdYlGn", domain = income2013$Number_of_, n = 20, reverse = TRUE)
    
    
    leaflet() %>%
      addPolygons(data = income2013, stroke = FALSE,fillColor = ~mypal(income2013$Number_of_)) %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE))
      
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)