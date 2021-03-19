library(shiny)
library(shinyjs)
library(tercen)
library(dplyr)
library(tidyr)
library(FlowSOM)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "52110340-a9c9-49fd-ba1e-a9b0cc4639b4",
                   workflowId = "686a2e2bba117e0c118bcb715300b5d3")
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
  
  shinyjs::useShinyjs(),
  tags$script(
    HTML(
      'setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);'
    )
  ),
  tags$footer(shinyjs::hidden(
    actionButton(inputId = "hiddenButton", label = "hidden")
  )),
  titlePanel("FlowSOM - MST"),
  
  sidebarPanel(
    selectInput("plot_type", "Plot type:", c("Markers", "Stars"), "Markers"),
    uiOutput("selectMarker"),
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

runApp(shinyApp(ui, server))  


