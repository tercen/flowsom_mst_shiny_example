library(shiny)
library(shinyjs)
library(tercen)
library(dplyr)
library(tidyr)
library(FlowSOM)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "696ceac5-92e5-4b8b-b76d-06d4a2346a16",
                   workflowId = "527f056bd521570e65126f6dbb0091e3")
  return(ctx)
}
####
############################################

ctx = tercenCtx()

deserialize.from.string = function(str64){
  con = rawConnection(base64enc::base64decode(str64), "r+")
  object = readRDS(con)
  close(con)
  return(object)
}
find.schema.by.factor.name = function(ctx, factor.name){
  visit.relation = function(visitor, relation){
    if (inherits(relation,"SimpleRelation")){
      visitor(relation)
    } else if (inherits(relation,"CompositeRelation")){
      visit.relation(visitor, relation$mainRelation)
      lapply(relation$joinOperators, function(jop){
        visit.relation(visitor, jop$rightRelation)
      })
    } 
    invisible()
  }
  
  myenv = new.env()
  add.in.env = function(object){
    myenv[[toString(length(myenv)+1)]] = object$id
  }
  
  visit.relation(add.in.env, ctx$query$relation)
  
  schemas = lapply(as.list(myenv), function(id){
    ctx$client$tableSchemaService$get(id)
  })
  
  Find(function(schema){
    !is.null(Find(function(column) column$name == factor.name, schema$columns))
  }, schemas);
}


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
    fsom <- dataInput()
    markers <- unname(colnames(fsom$FlowSOM$data))
    selectInput(inputId = "select_marker", label = "Select marker:", choices = markers)
  }) 
  
  output$main.plot <- renderPlot({
    
    ctx <- getCtx(session)
    fSOM <- dataInput()

    if(input$plot_type == "Stars") PlotStars(fSOM[[1]], backgroundValues = as.factor(fSOM[[2]]))
    if(input$plot_type == "Markers") PlotMarker(fSOM[[1]], input$select_marker)
    
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

runApp(shinyApp(ui, server))  


