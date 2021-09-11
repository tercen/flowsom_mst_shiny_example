library(shiny)
library(shinyjs)
library(tercen)
library(dplyr)
library(tidyr)
library(FlowSOM)
# devtools::install_github("tercen/tim")
library(tim)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "56954774-4f4f-4289-9b32-86a714831ac7",
                   workflowId = "e9482bf86b951d32f9ff44a499015119")
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
    markers <- names(fsom$prettyColnames)
    selectInput(inputId = "select_marker", label = "Select marker:", choices = markers)
  }) 
  
  output$main.plot <- renderPlot({
    
    ctx <- getCtx(session)
    fSOM <- dataInput()

    if(input$plot_type == "Stars") {
      PlotStars(fSOM, maxNodeSize = input$maxNodeSize,
                backgroundValues = fsom$metaclustering)
    } 
    else if(input$plot_type == "Markers") {
      PlotMarker(fSOM, maxNodeSize = input$maxNodeSize, input$select_marker,
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


