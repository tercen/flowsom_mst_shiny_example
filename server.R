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

getValues <- function(session){
  
  ctx <- getCtx(session)
  
  data = ctx %>% 
    select(.ci, .ri, .y) %>% 
    reshape2::acast(.ci ~ .ri, value.var='.y', fill=NaN, fun.aggregate=mean)
  
  colnames(data) <- ctx$rselect()[[1]]
  
  return(data)
}

shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
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
      maxMeta = as.integer(ctx$op.value('maxMeta')),
      seed = as.integer(ctx$op.value('seed'))
    )
    
    PlotStars(fSOM[[1]], backgroundValues = as.factor(fSOM[[2]]))
    
  })
  
})




