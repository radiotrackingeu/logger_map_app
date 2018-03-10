############ srvTabGraph.R ############

observe({
  if (!(is.null(logger_data())
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
  
  p<-ggplot(data) + geom_point(aes(timestamp, max_signal, color=receiver), size=I(0.8)) + labs(x="Time", y = "Signal Strength")
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
    tempo<-filter_data_freq(tempo,input$single_freq,input$freq_error,input$center_freq,paste0(input$single_freq/1000," MHz"))
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
  if(input$filter_interval){
    tempo<-filter_data_time_interval(tempo,input$signal_interval)
  }
  if(input$filter_freq&&input$filter_one_freq){
    return(NULL)
  }
  if(input$choose_tag!="all"&& !is.null(input$choose_tag) && input$choose_tag!=""){
    tempo<-subset(tempo,tempo$freq_tag==input$choose_tag)
  }
  if(input$correct_signal_strength){
    tempo[tempo$receiver=="flugplatz_0",]$max_signal<-tempo[tempo$receiver=="flugplatz_0",]$max_signal-0
    tempo[tempo$receiver=="flugplatz_90",]$max_signal<-tempo[tempo$receiver=="flugplatz_90",]$max_signal-6
    tempo[tempo$receiver=="flugplatz_180",]$max_signal<-tempo[tempo$receiver=="flugplatz_180",]$max_signal-9.5
    tempo[tempo$receiver=="flugplatz_270",]$max_signal<-tempo[tempo$receiver=="flugplatz_270",]$max_signal-15.5
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
  ggplot(filtered_data()) + geom_histogram(aes(signal_freq),bins=200)+ scale_y_log10()
})

output$histo_length <- renderPlot({
  if(is.null(logger_data())){
    return(NULL)
  }
  
  ggplot(filtered_data()) + geom_histogram(aes(duration),bins= 100)
})

output$histo_strength <- renderPlot({
  if (is.null(filtered_data()))
    return(NULL)
  
  ggplot(filtered_data()) + geom_histogram(aes(max_signal),bins= 200)
})

output$histo_bandwidth<- renderPlot({
  if (is.null(filtered_data()))
    return(NULL)
  
  ggplot(filtered_data()) + geom_histogram(aes(signal_bw),bins= 200)
})

output$angle<- renderDataTable({
  if (is.null(filtered_data()))
    return(NULL)
  tmp_angles<-NULL
  tmp<-subset(filtered_data(), filtered_data()$timestamp>max(filtered_data()$timestamp)-20)
  tmp_new<-list_data_time_receiver(tmp)
  print(tmp_new)
  tmp_new$angle<-NA
  for(i in 1:nrow(tmp_new)){
    tmp_sort<-data.frame(receiver=names(tmp_new[,3:ncol(tmp_new)]),max_signal=as.numeric(tmp_new[i,3:ncol(tmp_new)]))    
    tmp_sort<-tmp_sort[order(tmp_sort$max_signal, decreasing = TRUE, na.last=NA),]
    print(tmp_sort)
    if(nrow(tmp_sort)>1){
      tmp_w1<-subset(antennae_data(),receiver==tmp_sort$receiver[1])$orientation[1]
      tmp_w2<-subset(antennae_data(),receiver==tmp_sort$receiver[2])$orientation[1]
      tmp_new$angle[i]<-calc_angle(tmp_sort$max_signal[1],tmp_sort$max_signal[2],tmp_w1,tmp_w2,60,60)
    }
  }
  tmp_new
})


output$total_counts<-renderText({
  if (is.null(logger_data()))
    return(NULL)
  
  return(paste("Number of observations in plot",dim(filtered_data())[1],"of total", dim(logger_data())[1]))
})

calc_angle <- function(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b){
  #condition: a<b and angle not bigger then 180°
  half_gain_dBm<-3
  slope_a<-(-2)*half_gain_dBm/oe_winkel_a
  slope_b<-2*half_gain_dBm/oe_winkel_b
  inter_a<-slope_a*angle_a*(-1)
  inter_b<-angle_b*slope_b*(-1)
  dif_angle=(sig_a-sig_b-inter_a+inter_b)/(slope_a-slope_b)
  if(dif_angle<0){
    dif_angle<-dif_angle+360
  }
  return(dif_angle)
}

# angle_a must be smaller then angle_b
get_angle_linear <- function(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b){
  #first the one with the smaller angle
  #take the smaller angle between the two antennas
  if(angle_a<angle_b){
    if(angle_b-angle_a>180){
      result<-calc_angle(sig_b, sig_a, angle_b, angle_a, oe_winkel_b, oe_winkel_a)+180
    }else{
      result<-calc_angle(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b)
    }
  }else
  {
    if(angle_b-angle_a<(-180)){
      result<-calc_angle(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b)+180
    }else{
      result<-calc_angle(sig_b, sig_a, angle_b, angle_a, oe_winkel_b, oe_winkel_a)
    }
  }
  return(result)
}

list_data_time_receiver <- function(data){
  #find for each receiver
  list_of_receivers<-unique(data$receiver)
  #and each frequency
  list_of_frequencies<-unique(data$freq_tag)
  #signals which appear betweeen time_distance[1] and time_distance[2] seconds
  return_tmp<-NULL
  for(k in list_of_frequencies){
    print(k)
    for(t in min(data$timestamp):max(data$timestamp)){
      t<-as.POSIXct(t,origin='1970-01-01')
      timeslot_data <- subset(data,data$timestamp>=t&data$timestamp<t+1)
      tmp<-data.frame(time=t, freq=k)
      for(i in list_of_receivers){
        one_receiver <- subset(timeslot_data,receiver==i)
        sig<-mean(one_receiver$max_signal)
        tmp[[i]]<-sig
      }
      #First a second column never NA - so excluded
      if(!all(is.na(tmp[3:ncol(tmp)]))){
        return_tmp<-rbind(return_tmp,tmp)
      }
    }
  }
  return(return_tmp)
}