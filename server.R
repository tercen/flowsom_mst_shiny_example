library(shiny)
library(shinyjs)
library(tercen)
library(dplyr)
library(tidyr)
library(FlowSOM)
library(tim)

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
    req(fsom)
    markers <- names(fsom$FlowSOM$prettyColnames)
    selectInput(inputId = "select_marker", label = "Select marker:", choices = markers)
  }) 
  
  output$main.plot <- renderPlot({
    
    ctx <- getCtx(session)
    fsom <- dataInput()
    req(fsom)
    
    if(input$plot_type == "Stars") {
      PlotStars(fsom = fsom, maxNodeSize = input$maxNodeSize,
                backgroundValues = fsom$metaclustering)
    } 
    else if(input$plot_type == "Markers") {
      PlotMarker(fsom = fsom, maxNodeSize = input$maxNodeSize, marker =  input$select_marker,
                 backgroundValues = fsom$metaclustering)
    } 
    
  })
  
})

getValues <- function(session){
  
  ctx <- getCtx(session)
  # search for a schema that contains a column name 
  schema = find_schema_by_factor_name(ctx, ctx$labels[[1]])
  # get the data
  table = ctx$client$tableSchemaService$select(schema$id, Map(function(x) x$name, schema$columns), 0, schema$nRows)
  
  fsom = lapply(as_tibble(table)[[".base64.serialized.r.model"]], deserialize_from_string)[[1]]
  # fsom$data <- NULL
  return(fsom)
}
