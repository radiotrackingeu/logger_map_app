shinyjs::disable("dl")

observeEvent(input$navbar,{
  value<-switch(input$navbar,
                "Input" = "",
                "Filter" = " Graph",
                "Map" = " Map"
  )
  shinyjs::html("dl", sprintf("<i class='fa fa-download'> </i> Download%s",value))
  if (value=="") shinyjs::disable("dl")
  else shinyjs::enable("dl")
})

output$dl <-downloadHandler(
  filename = function() {
    switch(input$navbar,
           "Filter" = "plot.pdf",
           "Map" = "map.png"
           )
  },
  content = function(f) {
    value<-switch(input$navbar,
           "Filter" = ggsave(f),
           "Map"  = {
             # print(str(leafletProxy("logger_map")))
             print("saving map...")
             b<-Sys.time()
             printmap<-map()%>%addAntennaePositions()%>%addAntennaeCones()%>%addDetectionCones()
             # print(str(printmap))
             mapshot(printmap,file=f)
             e<-Sys.time()
             print(paste("saving took", e-b,"s"))
           }
           )
    return(value)
    
  },
  contentType = "image/png"
  
)

savemap<-function(file) {
  m<-leaflet() %>% addTiles()
  mapshot(m,file)
}