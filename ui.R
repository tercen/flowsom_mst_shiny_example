library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  
  shinyjs::useShinyjs(),
  tags$script(
    HTML(
      'setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);'
    )
  ),
  tags$footer(shinyjs::hidden(
    actionButton(inputId = "hiddenButton", label = "hidden")
  )),
  titlePanel("FlowSOM - MST"),
  
  sidebarPanel(
    selectInput("plot_type", "Plot type:", c("Markers", "Stars"), "Markers"),
    uiOutput("selectMarker"),
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 700)
  ),
  
  mainPanel(
    uiOutput("reacOut")
  )
  
))