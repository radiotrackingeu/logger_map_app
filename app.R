required_packages<-c("shiny",
                     "ggplot2",
                     "leaflet",
                     "rgdal",
                     "DBI",
                     "RSQLite",
                     "htmlwidgets",
                     "shinyjs",
                     "RMySQL",
                     "pool",
                     "tools",
                     "splusTimeSeries")

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

jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}

shinyjs.disableButton = function(name) {
var btn = $()
}
"

css <- "
.nav li a.disabled {
background-color: #f8f8f8 !important;
color: #999 !important;
cursor: not-allowed !important;
border-color: #f8f8f8 !important;
}"


install_and_load_packages(required_packages)

options(digits.secs=3)

ui <- tagList(
  useShinyjs(),
  extendShinyjs(text=jscode),
  inlineCSS(css),
  navbarPage(id="navbar", "LoggerMapApp",
    source(file.path("ui", "uiTabData.R"),local=TRUE)$value,
    source(file.path("ui", "uiTabFilter.R"),local=TRUE)$value,
    source(file.path("ui", "uiTabMap.R"),local=TRUE)$value
  )
)

server <- function(input, output, session) {
  source(file.path("server", "srvFileIO.R"),local=TRUE)$value
  source(file.path("server", "srvDBIO.R"),local=TRUE)$value
  source(file.path("server", "srvFilters.R"),local=TRUE)$value
  source(file.path("server", "srvTabData.R"),local=TRUE)$value
  source(file.path("server", "srvTabFilter.R"),local=TRUE)$value
  source(file.path("server", "srvTabMap.R"),local=TRUE)$value
  source(file.path("server", "srvDownload.R"), local=TRUE)$value
}

shinyApp(ui=ui, server=server)