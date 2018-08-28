library(shiny)
library(leaflet)
library(tidyverse)

crash_grouping2 <- read_rds("/home/SHIP/lebryant/philadelphia-crash-map/shiny/app-data/crash_grouping2.rds")

ui <- fluidPage(
  titlePanel("Philadelphia Historical Crash Data"),
  sidebarLayout(
    sidebarPanel(
      # Select type of trend to plot
      selectInput(inputId = "hr_block", label = strong("Hour Block"),
                  choices = unique(crash_grouping2$hr_block)),
      selectInput(inputId = "crash_month", label = strong("Month"),
                  choices = seq(1, 12)),
      selectInput(inputId = "day_of_week", label = strong("Day of the Week"),
                  choices = seq(1, 7))
    ),
    mainPanel(
      dataTableOutput("dtf_test"),
      leafletOutput("mymap"))
  )
)

server <- function(input, output) {
  
  selected_crash_grouping2 <- reactive({
    crash_grouping2  %>%
      filter(
        hr_block == input$hr_block ,
        day_of_week == input$day_of_week,
        crash_month == input$crash_month)
  })
  
  # philly_crash_sites <- reactive({
  #   philly  %>%
  #     filter(
  #       hr_block == input$hr_block ,
  #       day_of_week == input$day_of_week,
  #       crash_month == input$crash_month)
  # })
  
  pop_up <- reactive({
    crash_grouping2  %>%
      filter(
        hr_block == input$hr_block ,
        day_of_week == input$day_of_week,
        crash_month == input$crash_month) %>% 
      filter(layerId == input$mymap_shape_click[[3]]) # %>% 
    # select(injury_count, 
    #        fatality_count,
    #        accident_count)
  })  
  
  output$dtf_test <- renderDataTable({
    pop_up()
  })
  
  # Dr. B.: I added the rectangles just to see if things would
  # work for us. We now have the rectangles working. Enjoy.
  output$mymap <- renderLeaflet({
    leaflet(selected_crash_grouping2()) %>% 
      setView(lng = -75.2891, lat = 39.9328, zoom = 10) %>% 
      addTiles() %>%
      addRectangles(
        lng1 = ~long1, lat1 = ~lat1,
        lng2 = ~long2, lat2 = ~lat2,
        fillColor = ~color, stroke = FALSE,
        fillOpacity = 0.3, popup = ~as.character(layerId),
        layerId = ~layerId
      )
  })
}

shinyApp(ui, server)