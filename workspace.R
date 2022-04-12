library(shiny)
library(shinyjs)
library(tercen)
library(dplyr)
library(tidyr)
library(FlowSOM)
library(tim)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "209ff3ab-8fe4-4815-8a07-b388564a632b",
                   workflowId = "0a835848c8dfb205b9151ce2ee2e738a")
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
  titlePanel("FlowSOM - Minimum Spanning Tree visualisation"),
  
  sidebarPanel(
    selectInput("plot_type", "Plot type:", c("Markers", "Stars"), "Markers"),
    uiOutput("selectMarker"),
    sliderInput("maxNodeSize", "Node size", 0, 10, value = 1.2, step = 0.05),
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 750),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 750)
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

runApp(shinyApp(ui, server))  


