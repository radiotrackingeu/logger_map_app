required_packages<-c("shiny",
                     "ggplot2",
                     "leaflet",
                     "rgdal",
                     "DBI",
                     "RSQLite")

install_and_load_packages <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

install_and_load_packages(required_packages)

ui <- navbarPage(id="navbar", "LoggerMapApp",
  source(file.path("ui", "uiTabData.R"),local=TRUE)$value,
  #source(file.path("ui", "uiTabDB.R"),local=TRUE)$value,
  source(file.path("ui", "uiTabFilter.R"),local=TRUE)$value,
  source(file.path("ui", "uiTabMap.R"),local=TRUE)$value
)

server <- function(input, output, session) {
  source(file.path("server", "srvFileIO.R"),local=TRUE)$value
  source(file.path("server", "srvDBIO.R"),local=TRUE)$value
  source(file.path("server", "srvFilters.R"),local=TRUE)$value
  source(file.path("server", "srvTabData.R"),local=TRUE)$value
  #source(file.path("server", "srvTabDB.R"),local=TRUE)$value
  source(file.path("server", "srvTabFilter.R"),local=TRUE)$value
  source(file.path("server", "srvTabMap.R"),local=TRUE)$value
  
}

shinyApp(ui=ui, server=server)