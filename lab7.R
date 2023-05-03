#Histograms

library(shiny)
library(plotly)
library(dplyr)

data <- iris

ui <- fluidPage(
  titlePanel("Histograms of Iris Dataset Properties"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a variable:",
                  choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
      sliderInput("bins", "Number of bins:", min = 1, max = 30, value = 10),
      textInput("color", "Histogram Color:", value = "blue")
    ),
    mainPanel(
      plotlyOutput("histogram", height = "500px", width = "100%")
    )
  )
)

server <- function(input, output) {
  
  output$histogram <- renderPlotly({
    fig <- data %>%
      plot_ly(x = ~get(input$variable), type = "histogram", autobinx = FALSE,
              xbins = list(start = min(data[[input$variable]]), end = max(data[[input$variable]]), size = (max(data[[input$variable]]) - min(data[[input$variable]])) / input$bins),
              marker = list(color = input$color)) %>%
      layout(title = paste("Histogram of", input$variable),
             xaxis = list(title = input$variable),
             yaxis = list(title = "Frequency"))
    fig <- fig %>% event_register(event = "plotly_relayout")
    fig
  })
  
  observeEvent(input$plotly_relayout, {
    layout <- event_data("plotly_relayout")
    xlim <- c(layout$xaxis.range[1], layout$xaxis.range[2])
    fig <- data %>%
      plot_ly(x = ~get(input$variable), type = "histogram", autobinx = FALSE,
              xbins = list(start = min(data[[input$variable]]), end = max(data[[input$variable]]), size = (max(data[[input$variable]]) - min(data[[input$variable]])) / input$bins),
              marker = list(color = input$color)) %>%
      layout(title = paste("Histogram of", input$variable),
             xaxis = list(title = input$variable, range = xlim),
             yaxis = list(title = "Frequency"))
    fig <- fig %>% event_register(event = "plotly_relayout")
    plotlyProxy("histogram") %>% plotlyProxyInvoke("relayout", list(xaxis.range = xlim)) %>% plotlyProxyInvoke("redraw")
  })
  
}

shinyApp(ui = ui, server = server)

