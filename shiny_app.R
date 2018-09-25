library(shiny)
library(leaflet)
library(dygraphs)

source("AllData_Merge.R")
masterDataset <- getMasterData()
data_MASTER <- masterDataset[[1]]
shapefile_MASTER <- masterDataset[[2]]

#shapefile_MASTER = read_sf("age-and-income-in-2013-by-talb2013.shp")

Schoollocations <- read.csv(file="S_Locations_Secondary.csv", header=TRUE, sep=",")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dataland.css"),
    tags$link(rel = "stylesheet", href="https://use.typekit.net/qkv6utb.css")
  ),
  
  # App title ----
  fluidRow(class = "first_row",
    tags$h2(class = "page_heading", "THE INSIGHTS MACHINE")
    ),
  hr(),
  
  #Selection area
  fluidRow(
    column(4,
           selectInput('x', 'Select a Territorial Authority', c("All Territorial Authorities",data_MASTER$Territorial.Authority))
    ),
    column(4,
           selectInput('ChartValue1', 'X-Axis Variable:', 
                       c("School Attendance", "School Retention", "Household Income", "Low_Income", "Medium_Income", "Upper_Income", "Deprivation Index"), 
                       selected = "School Attendance")
    ),
    column(4,
           selectInput('ChartValue2', 'Y-Axis Variable:', 
                       c("School Attendance", "School Retention", "Household Income", "Low_Income", "Medium_Income", "Upper_Income", "Deprivation Index"),
                       selected = "School Retention")
    )     
    
  ),
  
  fluidRow(
    column(6, leafletOutput("mymap")),
    column(6, plotOutput(outputId = "statsPlot"))
  ),
  hr(),
  fluidRow(
    column(10, offset = 1, dygraphOutput("tsPlot"))
  ),
  hr()
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  
  
  dataset <- reactive({
    xName = input$ChartValue1
    yName = input$ChartValue2
    
    x <- switch(xName,
                "School Attendance" = 10, 
                "School Retention" = 14, 
                "Household Income" = 3, 
                "Low_Income" = 4, 
                "Medium_Income" = 5, 
                "Upper_Income" = 6,
                "Deprivation Index" = 2)
    y <- switch(yName,
                "School Attendance" = 10, 
                "School Retention" = 14, 
                "Household Income" = 3, 
                "Low_Income" = 4, 
                "Medium_Income" = 5, 
                "Upper_Income" = 6,
                "Deprivation Index" = 2)
    
    
    data2 <- data.frame(TA = data_MASTER$Territorial.Authority, x = data_MASTER[,x], y = data_MASTER[,y])
    return(data2)
  })
  
  regression <- reactive({
    mod1 <- lm(y~x, data = dataset())
  })

  taOfInterest <- reactive({
    dataset <- dataset()
    
    taDataset <- dataset[dataset$TA == input$x,]
  })
    
  output$statsPlot <- renderPlot({

    plot(dataset()$x, dataset()$y,
         main = paste(input$ChartValue1, "v.s.", input$ChartValue2), 
         xlab = input$ChartValue1, ylab = input$ChartValue2)
    abline(regression())
    if(input$x != "All Territorial Authorities") points(taOfInterest()$x, taOfInterest()$y, col = "red", pch = 19, cex = 1.5)
    
  })
  
  map <- reactive({

    #a = input$x
    a = "Participation_2013"
    
    data = shapefile_MASTER
    data_col = data$Participation_2013
    
    mypal <- colorQuantile(palette = "RdYlGn", domain = data_col, n = 10, reverse = TRUE)
    
    leaflet() %>%
      addPolygons(data = data, fillOpacity = 2, color= 'white',weight= 0, fillColor = ~mypal(data_col)) %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = 173.28 , lat = -41.27, zoom = 5) %>%
      #setView(data[1,], zoom = 5) %>%
      addTiles(urlTemplate = 'http://{s}.tile.stamen.com/watercolor/{z}/{x}/{y}.png', attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
      # addCircleMarkers(Schoollocations$Longitude, Schoollocations$Latitude)
      addLegend("bottomright", pal = mypal, values = data_col, title = "Participation 2013",
              #labFormat = labelFormat(prefix = "$"),
              opacity = 1) %>%
      flyTo(lng = 173.28 , lat = -41.27, zoom = 5)
  })
  
  output$mymap <- renderLeaflet({
    map()
  })
  
  
  timeseriesAttendance <- reactive({
    library(xts)
    
    TA <- input$x
    if(TA == "All Territorial Authorities"){
      return(message("Please select at territorial authority"))
    }else{
    
      dataTS <- xts(t(data_MASTER[which(data_MASTER$Territorial.Authority == TA), c(7:10)]), 
                    order.by = as.POSIXct(c("2013-6-1", "2014-6-1", "2015-6-1", "2016-6-1")))
      
      survey123Attendance <- Schoollocations$Attendance[Schoollocations$Territorial.Authority.with.Auckland.Local.Board == TA]
      
      fakeTS <- xts(survey123Attendance, 
                    order.by = sample(seq(as.Date('2017/01/01'), as.Date('2018/07/01'), by="day"), length(survey123Attendance)))
                      # c(as.POSIXct(paste("2017", c(1, 3, 5, 7, 9, 11), round(runif(6, 1, 28)), sep = "-")),
                      #            as.POSIXct(paste("2018", c(1, 3, 5, 7, 9, 11), round(runif(6, 1, 28)), sep = "-"))))
      
      attendanceTS <- rbind(dataTS, fakeTS)
      colnames(attendanceTS) <- "Attendance"
      
      return(attendanceTS)
    }
  })
  
  output$tsPlot <- renderDygraph({
    tsData <- timeseriesAttendance()
    
    if(class(tsData) != "NULL") dygraph(tsData, main = input$x, ylab = "Attendance") %>% dyOptions(stepPlot = T, fillGraph = T)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)