############ srvTabGraph.R ############

observe({
  if (!(is.null(logger_data())
      ))
    js$enableTab("Filter")
  else
    js$disableTab("Filter")
})

#create tag list for each receiver
correction_list<-reactive({
  if(is.null(antennae_data())){
    return(NULL)
  }
  correction_tag_list <- tagList()
  for(i in unique(antennae_data()$receiver)){
    tmp<-numericInput(paste0("corr_", i),paste0("Correction Factor for ", i),0)
    correction_tag_list <- tagAppendChildren(correction_tag_list,tmp)
  }
  return(correction_tag_list)
})

output$correction_list <- renderUI({
  correction_list()
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

observeEvent(input$filter_freq,
             {
  validate(
    need(filtered_data(), "Please provide frequency information in data tab.")
  )
  updateSelectInput(session, "choose_tag", label = "Select Tag", choices = c("all",unique(filtered_data()$freq_tag)))
})

observe({
  updateSliderInput(session, "choose_single_data_set",min=1,max=nrow(filtered_data()), value = 1)
})

output$single_freq_num_input <- renderUI(
  if(input$filter_one_freq){
    numericInput("single_freq", "", value = 150175)
  }
)

output$multi_freq_tag_input <- renderUI(
  if(input$filter_freq){
    selectInput("choose_tag", "Choose Tag", choices = NULL)
  }
)

plot_time_signal <- function(data, multifilter){
  
  p<-ggplot(data) +
    geom_point(aes(timestamp, max_signal, color=receiver), size=I(0.8)) +
    labs(x="Time", y = "Signal Strength") +
    scale_x_datetime(labels = function(x) format(x, "%H:%M:%S")) + ylim(0,100)
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
    for(i in unique(antennae_data()$receiver)){
      tempo[tempo$receiver==i,]$max_signal<-tempo[tempo$receiver==i,]$max_signal+input[[paste0("corr_",i)]]
    }
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

output$total_counts<-renderText({
  if (is.null(logger_data()))
    return(NULL)
  
  return(paste("Number of observations in plot",dim(filtered_data())[1],"of total", dim(logger_data())[1]))
})

# calculate time match and DoA #1
output$angle<- renderDataTable({
  if(is.null(angle_linear()))
    return(NULL)
  angle_linear()[order(angle_linear()$time,decreasing=TRUE),]
})

# output DoA plot
output$doa_plot <- renderPlot({
  if(is.null(doa_data()))
    return(NULL)
  ggplot() + geom_point(mapping=aes(x=angle_linear()$time,y=angle_linear()$angle,ymin=0,ymax=359,colour=angle_linear()$freq)) #+ geom_point(mapping=aes(x=doa_data()$time,y=doa_data()$angle,ymin=0,ymax=359),col="blue")
  #
})

# calculate time match and DoA #2
doa_data<- reactive({
  if (is.null(filtered_data()))
    return(NULL)
  tmp_angles<-NULL
  #tmp<-subset(filtered_data(), filtered_data()$timestamp>max(filtered_data()$timestamp)-20) # nur die letzten 20 sec??
  tmp<-filtered_data()
  tmp_new<-list_data_time_receiver(tmp)
  tmp_new$angle<-NA
  for(i in 1:nrow(tmp_new)){
    tmp_sort<-data.frame(receiver=names(tmp_new[,3:ncol(tmp_new)]),max_signal=as.numeric(tmp_new[i,3:ncol(tmp_new)]))    
    tmp_sort<-tmp_sort[order(tmp_sort$max_signal, decreasing = TRUE, na.last=NA),]
    if(nrow(tmp_sort)>1){
      tmp_front<-subset(antennae_data(),receiver==tmp_sort$receiver[1])
      
      tmp_angle_front<-tmp_front$orientation[1]
      tmp_angle_left<-tmp_angle_front-90
      if (tmp_angle_left<0)
        tmp_angle_left<-tmp_angle_left+360
      tmp_angle_right<-tmp_angle_front+90
      if (tmp_angle_right>360)
        tmp_angle_right<-tmp_angle_right-360
      tmp_angle_rear<-tmp_angle_front+180
      if(tmp_angle_rear>360)
        tmp_angle_rear<-tmp_angle_rear-360
      
      rx_front<-tmp_front$receiver[1]
      rx_left<-subset(antennae_data(),orientation==tmp_angle_left)$receiver[1]
      rx_right<-subset(antennae_data(),orientation==tmp_angle_right)$receiver[1]
      rx_rear<-subset(antennae_data(),orientation==tmp_angle_rear)$receiver[1]
      
      sig_front<-subset(tmp_sort,receiver==rx_front)$max_signal
      sig_left<-subset(tmp_sort,receiver==rx_left)$max_signal
      if(length(sig_left)==0){sig_left<-0}
      sig_right<-subset(tmp_sort,receiver==rx_right)$max_signal
      if(length(sig_right)==0){sig_right<-0}
      sig_rear<-subset(tmp_sort,receiver==rx_rear)$max_signal
      if(length(sig_rear)==0){
        sig_rear<-0
      }
      
      if(sig_left>sig_right){
        tmp_new$angle[i]<-get_angle_linear(sig_left,sig_front,tmp_angle_left,tmp_angle_front,60,60)
      }
      else{
        tmp_new$angle[i]<-get_angle_linear(sig_front,sig_right,tmp_angle_front,tmp_angle_right,60,60)
      }
      
      "if(sig_left!=0&&sig_right!=0){
        tmp_new$angle[i]<-calc_angle(sig_left,sig_right,tmp_angle_left,tmp_angle_right,60,60)
      }else{
        if(sig_left!=0){
          tmp_new$angle[i]<-calc_angle(sig_left,sig_front,tmp_angle_left,tmp_angle_front,60,60)
        }
        if(sig_right!=0){
          tmp_new$angle[i]<-calc_angle(sig_front,sig_right,tmp_angle_front,tmp_angle_right,60,60)
        }
      }"
    }
  }
  tmp_new
})

angle_linear<-reactive({
  if (is.null(filtered_data()))
    return(NULL)
  tmp_angles<-NULL
  #tmp<-subset(filtered_data(), filtered_data()$timestamp>max(filtered_data()$timestamp)-20)
  tmp<-filtered_data()
  tmp_new<-list_data_time_receiver(tmp)
  tmp_new$angle<-NA
  for(i in 1:nrow(tmp_new)){
    tmp_sort<-data.frame(receiver=names(tmp_new[,3:ncol(tmp_new)]),max_signal=as.numeric(tmp_new[i,3:ncol(tmp_new)]))    
    tmp_sort<-tmp_sort[order(tmp_sort$max_signal, decreasing = TRUE, na.last=NA),]
    if(nrow(tmp_sort)>1){
      tmp_w1<-subset(antennae_data(),receiver==tmp_sort$receiver[1])$orientation[1]
      tmp_w2<-subset(antennae_data(),receiver==tmp_sort$receiver[2])$orientation[1]
      tmp_new$angle[i]<-get_angle_linear(tmp_sort$max_signal[1],tmp_sort$max_signal[2],tmp_w1,tmp_w2,60,60)
    }
  }
  tmp_new
})

# linear approximation 
calc_angle_linear <- function(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b){
  #condition: a<b and angle not bigger then 180
  #half_gain_dBm<-3
  slope_a<--input$dBLoss/input$angle_sep #(-2)*half_gain_dBm/oe_winkel_a
  slope_b<- input$dBLoss/input$angle_sep#2*half_gain_dBm/oe_winkel_b
  inter_a<-slope_a*angle_a*(-1)
  inter_b<-angle_b*slope_b*(-1)
  dif_angle=(sig_a-sig_b-inter_a+inter_b)/(slope_a-slope_b)
  if(dif_angle<0){
    dif_angle<-dif_angle+360
  }
  return(dif_angle)
}

#angle_a must be smaller then angle_b
get_angle_linear <- function(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b){
  #first the one with the smaller angle
  #take the smaller angle between the two antennas
  if(angle_a<angle_b){
    if(angle_b-angle_a>180){
      result<-calc_angle_linear(sig_b, sig_a, angle_b, angle_a, oe_winkel_b, oe_winkel_a)+180
    }else{
      result<-calc_angle_linear(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b)
    }
  }else
  {
    if(angle_b-angle_a<(-180)){
      result<-calc_angle_linear(sig_a, sig_b, angle_a, angle_b, oe_winkel_a, oe_winkel_b)+180
    }else{
      result<-calc_angle_linear(sig_b, sig_a, angle_b, angle_a, oe_winkel_b, oe_winkel_a)
    }
  }
  return(result)
}

#time match between receiver signals
list_data_time_receiver <- function(data){
  #find for each receiver
  list_of_receivers<-unique(data$receiver)
  #and each frequency
  list_of_frequencies<-unique(data$freq_tag)
  #signals which appear betweeen time_distance[1] and time_distance[2] seconds
  return_tmp<-NULL
  for(k in list_of_frequencies){
    print(k)
    for(t in seq(min(data$timestamp),max(data$timestamp),3)){
      t<-as.POSIXct(t, tz = "UTC", origin='1970-01-01')
      timeslot_data <- subset(data,data$timestamp>=t&data$timestamp<t+3)
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