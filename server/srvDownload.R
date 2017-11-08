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
           "Map" = "map.pdf"
           )
  },
  content = function(file) {
    value<-switch(input$navbar,
           "Filter" = ggsave(file),
           "Map"  = mapshot(map,file, TRUE )
           )
    return(value)
    
  },
  contentType = "application/pdf"
  
)