library(shiny)
library(leaflet)
library(tidyverse)

crash_grouping2 <- read_rds("/home/SHIP/lebryant/philadelphia-crash-map/shiny/app-data/crash_grouping2.rds")
crash_grouping2 %>% 
  ungroup() %>% 
  mutate(crash_month = as.integer(crash_month))


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


## this crashes... :(

server <- function(input, output) {
    
  selected_crash_grouping2 <- reactive({
    crash_grouping2 # %>%
       filter(
         hr_block == input$hr_block) #,
      #   crash_month == input$crash_month,
      #   day_of_week == input$day_of_week)
    })
  
  output$dtf_test <- renderDataTable({
    selected_crash_grouping2()
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -75.2891, lat = 39.9328, zoom = 10) %>% 
      addTiles() #%>%
      # addRectangles(
      #   lng1 = selected_crash_grouping2$long1, lat1 = selected_crash_grouping2$lat1,
      #   lng2 = selected_crash_grouping2$long2, lat2 = selected_crash_grouping2$lat2,
      #   fillColor = "transparent"
      #)
  })
}

shinyApp(ui, server)
