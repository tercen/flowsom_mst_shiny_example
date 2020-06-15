library(shiny)

shinyUI(fluidPage(
  
  titlePanel("FlowSOM - MST"),
  
  mainPanel(
    plotOutput("main.plot")
  )
  
))