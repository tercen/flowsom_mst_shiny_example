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

getValues <- function(session) {
  
  ctx <- getCtx(session)
  
  data = ctx %>% 
    select(.ci, .ri, .y) %>% 
    reshape2::acast(.ci ~ .ri, value.var='.y', fill=NaN, fun.aggregate=mean)
  
  colnames(data) <- ctx$rselect()[[1]]
  
  flow.dat <- flowCore::flowFrame(as.matrix(data))
  
  n.clust <- NULL
  if(ctx$op.value('nclust') != "NULL") n.clust <- as.integer(ctx$op.value('nclust'))
  
  fsom <- FlowSOM(
    flow.dat,
    colsToUse = 1:ncol(flow.dat),
    nClus = n.clust,
    maxMeta = 10#as.integer(ctx$op.value('maxMeta')),
    # seed = as.integer(ctx$op.value('seed')),
    # xdim   = as.integer(ctx$op.value('xdim')),
    # ydim   = as.integer(ctx$op.value('ydim')),
    # rlen   = as.integer(ctx$op.value('rlen')),
    # mst    = as.integer(ctx$op.value('mst')),
    # alpha  = c(as.integer(ctx$op.value('alpha_start')), (as.double(ctx$op.value('alpha_end')))),
    # distf  = as.integer(ctx$op.value('distf'))
  )
  return(fsom)
}

fsom <- getValues(session)

shinyServer(function(input, output, session) {
  
  output$main.plot <- renderPlot({
    PlotStars(fsom[[1]], backgroundValues = as.factor(fsom[[2]]))
  })
  
})


