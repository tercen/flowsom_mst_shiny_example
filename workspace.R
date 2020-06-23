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
  
  mainPanel(
    plotOutput("main.plot")
  )
  
))

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })

  output$main.plot <- renderPlot({
    ctx <- getCtx(session)
    
    values <- dataInput()
    colnames(values) <- ctx$rselect()[[1]]

    flow.dat <- flowCore::flowFrame(as.matrix(values))
    
    n.clust <- NULL
    # if(!ctx$op.value('nclust') == "NULL") n.clust <- as.integer(ctx$op.value('nclust'))
    
    fsom <- FlowSOM(
      flow.dat,
      colsToUse = 1:ncol(flow.dat),
      nClus = n.clust,
      maxMeta = 10
      # maxMeta = as.integer(ctx$op.value('maxMeta')),
      # seed = as.integer(ctx$op.value('seed')),
      # xdim   = as.integer(ctx$op.value('xdim')),
      # ydim   = as.integer(ctx$op.value('ydim')), 
      # rlen   = as.integer(ctx$op.value('rlen')), 
      # mst    = as.integer(ctx$op.value('mst')), 
      # alpha  = c(as.integer(ctx$op.value('alpha_start')), (as.double(ctx$op.value('alpha_end')))),
      # distf  = as.integer(ctx$op.value('distf'))
    )

    PlotStars(fsom[[1]], backgroundValues = as.factor(fsom[[2]]))

  })
  
})

getValues <- function(session){

  ctx <- getCtx(session)
  
  data = ctx %>% 
    select(.ci, .ri, .y) %>% 
    reshape2::acast(.ci ~ .ri, value.var='.y', fill=NaN, fun.aggregate=mean)
  
  return(data)
}

runApp(shinyApp(ui, server))  


