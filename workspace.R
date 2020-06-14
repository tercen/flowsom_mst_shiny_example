library(shiny)
library(tercen)
library(dplyr)
library(tidyr)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "9b7619c7-4d66-49fa-9bb3-2b06209e58e4",
                   workflowId = "f81d245ef22a2ff192ed2533a6002ec3")
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
  
  titlePanel("Histogram"),
  
  sidebarPanel(
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500),
  ),
  
  mainPanel(
    uiOutput("reacOut")
  )
  
))

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })
  
  output$reacOut <- renderUI({
    plotOutput(
      "main.plot",
      height = input$plotHeight,
      width = input$plotWidth
    )
  }) 
  
  output$main.plot <- renderPlot({
    values <- dataInput()
    data <- values$data$.y
    hist(data)
  })
  
})

getValues <- function(session){
  ctx <- getCtx(session)
  values <- list()

  values$data <- ctx %>% select(.y, .ri, .ci) %>%
    group_by(.ci, .ri) %>%
    summarise(.y = mean(.y)) # take the mean of multiple values per cell

  return(values)
}

runApp(shinyApp(ui, server))  
