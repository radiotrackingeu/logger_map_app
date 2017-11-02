############ srvTabGraph.R ############


plot_time_signal <- function(data, multifilter){
  
  p<-ggplot(data) + geom_point(aes(timestamp, strength), size=I(0.8)) + labs(x="Time", y = "Signal Strength") 
  if(multifilter){
    p + facet_wrap(~ data$freq_tag)
  }
  else{
    p
  }
}


# list of frequencies to watch
freqs <- reactive({
  inFile <- input$freq_file
  if(is.null(inFile)){
    tmp<-NULL
    for(i in 1:9)
      if(input[[paste0("act_freq_",i)]]){
        zws<-data.frame(freq=input[[paste0("freq",i)]],label=input[[paste0("freq_id_",i)]])
        tmp<-rbind.data.frame(tmp,zws)
      }
    return(tmp)
  }
  else{
    return(read.csv2(inFile$datapath))
  }
  
})

# applying filters
filtered_data <- reactive({
  if (is.null(logger_data()))
    return(NULL)
  tempo<-logger_data()
  if(input$filter_length){
    tempo<-filter_data_length(tempo,input$signal_length)
  }
  if(input$filter_one_freq){
    tempo<-filter_data_freq(tempo,input$single_freq,input$freq_error,input$center_freq)
  }
  if(input$filter_freq){
    tempo<-filter_data_freq(tempo,freqs()[["freq"]],input$freq_error,input$center_freq,freqs()[["label"]])
  }
  if (input$filter_strength){
    tempo<-filter_signal_strength(tempo,input$signal_strength)
  }
  if (input$filter_bw){
    tempo<-filter_signal_bandwidth(tempo,input$signal_bw)
  }
  if(input$filter_freq&&input$filter_one_freq){
    return(NULL)
  }
  validate(
    need(nrow(tempo)[1]>0, "Oh no, there is no data to plot! Did you filter it all out?")
  )
  return(tempo)
} )


output$facet <- renderPlot({
  
  if(is.null(filtered_data()))
    return(NULL)
  if(input$filter_freq){
    plot_time_signal(filtered_data(),TRUE)
  }
  else{
    plot_time_signal(filtered_data(),input$filter_one_freq)
  }
  
})

output$histo <- renderPlot({
  if(is.null(filtered_data()))
    return(NULL)
  ggplot(filtered_data()) + geom_histogram(aes(freq),bins=200)
})

output$histo_length <- renderPlot({
  require("ggplot2")
  if(is.null(logger_data())){
    return(NULL)
  }
  
  ggplot(filtered_data()) + geom_histogram(aes(duration),bins= 100)
})

output$histo_strength <- renderPlot({
  if (is.null(filtered_data()))
    return(NULL)
  
  ggplot(filtered_data()) + geom_histogram(aes(strength),bins= 200)
})

output$histo_bandwidth<- renderPlot({
  if (is.null(filtered_data()))
    return(NULL)
  
  ggplot(filtered_data()) + geom_histogram(aes(bw),bins= 200)
})


output$total_counts<-renderText({
  if (is.null(logger_data()))
    return(NULL)
  
  return(paste("Number of observations in plot",dim(filtered_data())[1],"of total", dim(logger_data())[1]))
})

output$downloadData <- downloadHandler(
  filename = function() {
    paste0("frequencies-", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv2(freqs(), file)
  }
)