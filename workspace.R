library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(FlowSOM)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "d9da8ab3-ea4f-4da6-88d2-2d8e566040fe",
                   workflowId = "7eee20aa9d6cc4eb9d7f2cc2430313b6")
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
  
  titlePanel("FlowSOM - MST"),
  
  mainPanel(
    plotOutput("main.plot")
  )
  
))

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })
  
  output$main.plot <- renderPlot({
    values <- dataInput()
    
    dat <- flowCore::flowFrame(as.matrix(values))
    
    n.clust <- NULL
    # if(!ctx$op.value('nclust') == "NULL") n.clust <- as.integer(ctx$op.value('nclust'))
    
    fSOM <- FlowSOM(
      dat,
      scale = TRUE,
      colsToUse = 1:ncol(dat),
      nClus = n.clust,
      maxMeta = 10
      # xdim   = as.integer(ctx$op.value('xdim')),
      # ydim   = as.integer(ctx$op.value('ydim')), 
      # rlen   = as.integer(ctx$op.value('rlen')), 
      # mst    = as.integer(ctx$op.value('mst')), 
      # alpha  = c(as.integer(ctx$op.value('alpha_start')),(as.double(ctx$op.value('alpha_end')))),
      # distf  = as.integer(ctx$op.value('distf'))
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


