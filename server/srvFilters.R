############ filters.R ############
filter_data_length <- function(data,pulse_length){
  return(subset(data, (data$duration>(pulse_length[1])) & (data$duration<(pulse_length[2]))))
}

filter_data_time_interval <- function(data,time_distance){
  #find for each receiver
  list_of_receivers<-unique(data$receiver)
  #and each frequency
  list_of_frequencies<-unique(data$signal_freq_tag)
  #signals which appear betweeen time_distance[1] and time_distance[2] seconds
  return_tmp<-NULL
  for(i in list_of_receivers){
    tmp1 <- subset(data,receiver==i)
    for(k in list_of_frequencies){
      tmp2<-subset(tmp1,freq_tag==k)
      tmp2<-tmp2[order(tmp2$timestamp),]
      #calcualte the time distance between the impulses
      if(nrow(tmp2)>1){
        td<-diff(tmp2$timestamp)
        if(attr(td,"units")=="secs"){
          td<-c(td,td[length(td)])
          tmp2<-cbind(tmp2,td=td)
          test<-tmp2$td>time_distance[1]&tmp2$td<time_distance[2]
          i<-1
          while(i<length(test)-1){
            if(!test[i+1]&&test[i]){
              test[i+1]<-TRUE
              i<-i+1
            }
            i<-i+1
          }
          tmp2<-tmp2[test,]
          return_tmp<-rbind(tmp2,return_tmp)
        }
      }
    }
  }
  return(return_tmp)
}

filter_signal_bandwidth <- function(data,pulse_bandwidth){
  return(subset(data, (data$singal_bw>(pulse_bandwidth[1])) & (data$signal_bw<(pulse_bandwidth[2]))))
}

filter_signal_strength <- function(data,pulse_strength){
  data$max_signal[data$max_signal==-1000]<- (-60) # old rtlsdrdetect outputs for some reason -1000
  return(subset(data, (data$max_signal>pulse_strength[1]) &(data$max_signal<pulse_strength[2]) ))
}

filter_data_freq <- function(data,freq,freq_error,mid_freq,freq_labels = NULL){
  freq_sorted<-NULL
  for(i in freq){
    tmp<-subset(data, (data$signal_freq>(i-freq_error)) & (data$signal_freq<(i+freq_error)))
    if(nrow(tmp)>0){
      if(is.null(freq_labels)){
        tmp$freq_tag<-paste0((i+mid_freq/1000)," MHz")
      }
      else
      {
        tmp$freq_tag<-freq_labels[which(i==freq)[1]]
      }
      freq_sorted<-rbind.data.frame(freq_sorted, tmp)
    }
  }
  return(freq_sorted)
}