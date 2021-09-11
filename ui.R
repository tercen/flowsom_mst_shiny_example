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
  titlePanel("FlowSOM - Minimum Spanning Tree visualisation"),
  
  sidebarPanel(
    selectInput("plot_type", "Plot type:", c("Markers", "Stars"), "Markers"),
    uiOutput("selectMarker"),
    sliderInput("maxNodeSize", "Node size", 0, 10, value = 1.2, step = 0.05),
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 750),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 750)
  ),
  
  mainPanel(
    uiOutput("reacOut")
  )
  
))