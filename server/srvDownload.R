observe({
  value<-switch(input$navbar,
                "Input" = "",
                "Filter" = " Graph",
                "Map" = " Map"
  )
  session$sendCustomMessage("changeTab", list(pic=value))
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