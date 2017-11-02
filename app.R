library(shiny)
#library(ggplot2)
library(leaflet) # used in tabMap.R
library(rgdal) # used in tabMap.R
library(DBI)
library(RSQLite)
ui <- navbarPage(id="navbar", "LoggerMapApp",
  source(file.path("ui", "uiTabData.R"),local=TRUE)$value,
  source(file.path("ui", "uiTabGraph.R"),local=TRUE)$value,
  source(file.path("ui", "uiTabMap.R"),local=TRUE)$value
)

server <- function(input, output, session) {
  source(file.path("server", "srvFileIO.R"),local=TRUE)$value
  source(file.path("server", "srvFilters.R"),local=TRUE)$value
  source(file.path("server", "srvTabData.R"),local=TRUE)$value
  source(file.path("server", "srvTabGraph.R"),local=TRUE)$value
  source(file.path("server", "srvTabMap.R"),local=TRUE)$value
  
}


shinyApp(ui=ui, server=server)