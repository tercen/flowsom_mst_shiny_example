library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(FlowSOM)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "a3f464fd-cd95-41fa-97e2-6e9b058a6269",
                   workflowId = "7eee20aa9d6cc4eb9d7f2cc2430313b6")
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
  
  titlePanel("FlowSOM - MST"),
  
  sidebarPanel(
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 700)
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
    
    ctx <- getCtx(session)
    
    values <- dataInput()
    
    dat <- flowCore::flowFrame(as.matrix(values))
    
    n.clust <- NULL
    if(!is.null(ctx$op.value('nclust'))) {
      if(ctx$op.value('nclust') != "NULL") n.clust <- as.integer(ctx$op.value('nclust'))
    } 
    
    fSOM <- FlowSOM(
      dat,
      scale = TRUE,
      colsToUse = 1:ncol(dat),
      nClus = n.clust,
      maxMeta = 10,
      seed = 42
    )
    
    PlotStars(fSOM[[1]], backgroundValues = as.factor(fSOM[[2]]))
    
  })
  
})

getValues <- function(session){
  
  ctx <- getCtx(session)
  
  data = ctx %>% 
    select(.ci, .ri, .y) %>% 
    reshape2::acast(.ci ~ .ri, value.var='.y', fill=NaN, fun.aggregate=mean)
  
  colnames(data) <- ctx$rselect()[[1]]
  
  return(data)
}

runApp(shinyApp(ui, server))  


