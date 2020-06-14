library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Histogram"),
  
  sidebarPanel(
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500),
  ),
  
  mainPanel(
    uiOutput("reacOut")
  )
  
))