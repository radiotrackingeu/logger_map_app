required_packages<-c("shiny",
                     "ggplot2",
                     "leaflet",
                     "rgdal",
                     "DBI",
                     "RSQLite",
                     "mapview")

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

#taken from https://github.com/daattali/advanced-shiny
# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(style= "float:right", class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}


install_and_load_packages(required_packages)

ui <- tagList(
  tags$head(HTML(
    '
    <script type="text/javascript">
    //taken from https://stackoverflow.com/questions/25247852/shiny-app-disable-downloadbutton
    $(document).ready(function() {
    // disable download at startup. data_file is the id of the downloadButton
    $("#dl").attr("disabled", "true").attr("onclick", "return false;");

    Shiny.addCustomMessageHandler("changeTab", function(newTab) {
    $("#dl").removeAttr("disabled).removeAttr("onclick").html(
    "<i class=\\"fa fa-download\\"></i> Download" + newTab.pic );
    });
    })
    </script>
    '
  )),
  navbarPageWithInputs(id="navbar", "LoggerMapApp",
  source(file.path("ui", "uiTabData.R"),local=TRUE)$value,
  #source(file.path("ui", "uiTabDB.R"),local=TRUE)$value,
  source(file.path("ui", "uiTabFilter.R"),local=TRUE)$value,
  source(file.path("ui", "uiTabMap.R"),local=TRUE)$value,
  inputs=downloadButton("dl", label = "Download")
)
)

server <- function(input, output, session) {
  source(file.path("server", "srvFileIO.R"),local=TRUE)$value
  source(file.path("server", "srvDBIO.R"),local=TRUE)$value
  source(file.path("server", "srvFilters.R"),local=TRUE)$value
  source(file.path("server", "srvTabData.R"),local=TRUE)$value
  #source(file.path("server", "srvTabDB.R"),local=TRUE)$value
  source(file.path("server", "srvTabFilter.R"),local=TRUE)$value
  source(file.path("server", "srvTabMap.R"),local=TRUE)$value
  source(file.path("server", "srvDownload.R"), local=TRUE)$value
}

shinyApp(ui=ui, server=server)