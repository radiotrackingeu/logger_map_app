############ filters.R ############
filter_data_length <- function(data,pulse_length){
  return(subset(data, (data$duration>(pulse_length[1])) & (data$duration<(pulse_length[2]))))
}

filter_signal_bandwidth <- function(data,pulse_bandwidth){
  return(subset(data, (data$bw>(pulse_bandwidth[1])) & (data$bw<(pulse_bandwidth[2]))))
}

filter_signal_strength <- function(data,pulse_strength){
  data$strength[data$strength==-1000]<- (-60) # old rtlsdrdetect outputs for some reason -1000
  return(subset(data, (data$strength>pulse_strength[1]) &(data$strength<pulse_strength[2]) ))
}

filter_data_freq <- function(data,freq,freq_error,mid_freq,freq_labels = NULL){
  freq_sorted<-NULL
  for(i in freq){
    tmp<-subset(data, (data$freq>(i-freq_error)) & (data$freq<(i+freq_error)))
    if(nrow(tmp)>0){
      if(is.null(freq_labels)){
        tmp$freq_tag<-paste0(as.character((i+mid_freq)/1000),"MHz")
      }
      else
      {
        tmp$freq_tag<-freq_labels[which(i==freq)[1]]
      }
      freq_sorted<-rbind.data.frame(freq_sorted, tmp)
    }
  }
  data<-freq_sorted
  
  freq_name<-paste0(as.character((freq)/1000),"MHz")
  #one_inv<-subset(data, (freq> low_freq) & (freq< high_freq) & (duration > low_pulse_len) & (duration < high_pulse_len) )
  
  #data$timestamp<-as.POSIXct(paste(data$Date, data$, "%Y-%m-%d %H:%M:%S", tz="UTC")
  return(data)
}