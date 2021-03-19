library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(FlowSOM)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

shinyServer(function(input, output, session) {
  
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
  
  output$selectMarker <- renderUI({
    d <- dataInput()
    markers <- colnames(d)
    selectInput(inputId = "select_marker", label = "Select marker:", choices = markers)
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
    
    # input par: plot stars or plot  marker
    # dropdown menu
    
    if(input$plot_type == "Stars") PlotStars(fSOM[[1]], backgroundValues = as.factor(fSOM[[2]]))
    if(input$plot_type == "Markers") PlotMarker(fSOM[[1]], input$select_marker)
    
  })
  
})

getValues <- function(session){
  
  ctx <- getCtx(session)
  
  data = ctx %>% 
    select(.ci, .ri, .y) %>% 
    reshape2::acast(.ci ~ .ri, value.var='.y', fill=NaN, fun.aggregate=mean)
  
  colnames(data) <- ctx$rselect() %>% unite("rnames") %>% unlist %>% unname
  
  return(data)
}
