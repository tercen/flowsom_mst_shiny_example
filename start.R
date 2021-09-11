options("tercen.serviceUri"=Sys.getenv("TERCEN_SERVICE_URI"))
options("shiny.host"="0.0.0.0")
shiny::runApp(port=8080,launch.browser=FALSE)