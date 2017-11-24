############ srvTabGraph.R ############

observe({
  if (!(is.null(logger_data()) ||
        is.null(antennae_data()) || 
        is.null(freqs())
      ))
    js$enableTab("Filter")
  else
    js$disableTab("Filter")
})

# Change upon data input
observe({
  validate(
    need(logger_data(), "Please provide file with antennae specifications.")
  )
  rec_names <- c("all",as.character(unique(logger_data()$receiver)))
  updateSelectInput(session, "input_select_receiver", label = "Select Receiver", choices = rec_names, selected = "all")
})

observe({
  validate(
    need(logger_data(), "Please provide file with antennae specifications.")
  )
  min_date<-min(logger_data()$timestamp)-1
  max_date<-max(logger_data()$timestamp)+1
  updateSliderInput(session, "slider_datetime",min=min_date,max=max_date,value = c(min_date,max_date) )
})

observe({
  validate(
    need(freqs(), "Please provide frequency information in data tab.")
  )
  updateSelectInput(session, "choose_tag", label = "Select Tag", choices = c("all",freqs()[["label"]]))
})

observe({
  updateSliderInput(session, "choose_single_data_set",min=1,max=nrow(filtered_data()), value = 1)
})

output$single_freq_num_input <- renderUI(
  if(input$filter_one_freq){
    numericInput("single_freq", "", value = 150175)
  }
)

plot_time_signal <- function(data, multifilter){
  
  p<-ggplot(data) + geom_point(aes(timestamp, strength, color=receiver), size=I(0.8)) + labs(x="Time", y = "Signal Strength")
  if(multifilter){
    p + facet_wrap(~ data$freq_tag)
  }
  else{
    p
  }
}


# applying filters
filtered_data <- reactive({
  if (is.null(logger_data()))
    return(NULL)
  tempo<-logger_data()
  
  #filter receivers
  if(!any(input$input_select_receiver=="all")){
    tempo<-subset(tempo,tempo$receiver==input$input_select_receiver)
  }
  #filter date/time
  tempo<-subset(tempo, (tempo$timestamp>=input$slider_datetime[1])&(tempo$timestamp<=input$slider_datetime[2]) )
  
  if(input$filter_length){
    tempo<-filter_data_length(tempo,input$signal_length)
  }
  if(input$filter_one_freq){
    tempo<-filter_data_freq(tempo,input$single_freq,input$freq_error,input$center_freq)
  }
  if(input$filter_freq){
    tempo<-filter_data_freq(tempo,freqs()[["freq"]],input$freq_error,input$center_freq,freqs()[["label"]])
  }
  if(input$filter_strength){
    tempo<-filter_signal_strength(tempo,input$signal_strength)
  }
  if(input$filter_bw){
    tempo<-filter_signal_bandwidth(tempo,input$signal_bw)
  }
  if(input$filter_freq&&input$filter_one_freq){
    return(NULL)
  }
  if(input$choose_tag!="all"){
    tempo<-subset(tempo,tempo$freq_tag==input$choose_tag)
  }
  if(input$activate_single_data){
    if(!is.null(tempo)){
      tempo <- tempo[order(tempo$timestamp),]
    }

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
  ggplot(filtered_data()) + geom_histogram(aes(freq),bins=200)+ scale_y_log10()
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