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
    values <- dataInput()
    
    dat <- flowCore::flowFrame(as.matrix(values))
    
    n.clust <- NULL
    if(!ctx$op.value('nclust') == "NULL") n.clust <- as.integer(ctx$op.value('nclust'))
    
    fSOM <- FlowSOM(
      dat,
      scale = TRUE,
      colsToUse = 1:ncol(dat),
      nClus = n.clust,
      maxMeta = as.integer(ctx$op.value('maxMeta')),
      xdim   = as.integer(ctx$op.value('xdim')),
      ydim   = as.integer(ctx$op.value('ydim')),
      rlen   = as.integer(ctx$op.value('rlen')),
      mst    = as.integer(ctx$op.value('mst')),
      alpha  = c(as.integer(ctx$op.value('alpha_start')),(as.double(ctx$op.value('alpha_end')))),
      distf  = as.integer(ctx$op.value('distf'))
    )
    
    PlotStars(fSOM[[1]], backgroundValues = as.factor(fSOM[[2]]))
    
  })
  
})



