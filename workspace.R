library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(FlowSOM)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "c7272671-643f-4d7b-8138-e01c46b43e64",
                   workflowId = "969f44a542c98b7d15717e0d35000cdd")
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
    fSOM <- FlowSOM(
      dat,
      scale = TRUE,
      colsToUse = 1:ncol(dat),
      nClus = 10,
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

getValues <- function(session){

  ctx <- getCtx(session)
  
  data = ctx %>% 
    select(.ci, .ri, .y) %>% 
    reshape2::acast(.ci ~ .ri, value.var='.y', fill=NaN, fun.aggregate=mean)
  
  return(data)
}

runApp(shinyApp(ui, server))  


