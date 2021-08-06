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
    fsom <- dataInput()
    markers <- unname(colnames(fsom$FlowSOM$data))
    selectInput(inputId = "select_marker", label = "Select marker:", choices = markers)
  }) 
  
  output$main.plot <- renderPlot({
    
    ctx <- getCtx(session)
    fSOM <- dataInput()
    
    if(input$plot_type == "Stars") {
      PlotStars(
        fSOM[[1]],
        maxNodeSize = input$maxNodeSize,
        backgroundValues = as.factor(fSOM[[2]]))
    } 
    else if(input$plot_type == "Markers") {
      PlotMarker(
        fSOM[[1]],
        maxNodeSize = input$maxNodeSize,
        input$select_marker)
    } 
    
  })
  
})

getValues <- function(session){
  
  ctx <- getCtx(session)
  # search for a schema that contains a column name 
  # schema = find.schema.by.factor.name(ctx, '.base64.serialized.r.model')
  schema = find.schema.by.factor.name(ctx, ctx$labels[[1]])
  # get the data
  table = ctx$client$tableSchemaService$select(schema$id, Map(function(x) x$name, schema$columns), 0, schema$nRows)
  
  fsom = lapply(as_tibble(table)[[".base64.serialized.r.model"]], deserialize.from.string)[[1]]
  
  return(fsom)
}
