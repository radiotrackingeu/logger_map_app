############ FileIO ############
options(shiny.maxRequestSize=3000*1024^2)
#reads and reformats data from file. Adds col timestamp with posix-style readable time
read_logger_data <- function(filepath){

  lines_to_skip <- findHeader(filepath) #skip meta data in files
  mid_freq <- findMidFreq(filepath) # find center frequency of tuner

  data<-read.csv2(filepath, skip=lines_to_skip,stringsAsFactors = FALSE, dec = ".")
  data$timestamp<-as.POSIXct(data$time, "%Y-%m-%d %H:%M:%S", tz="UTC")
  data$freq<-(data$freq+mid_freq)/1000
  return(data)
}

findHeader <- function(file) {
  tmp<-readLines(file, n=30)
  n<-grep("time;duration;freq;bw;strength;", tmp)-1
  return(n)
}

findMidFreq <- function(file) {
  tmp<-readLines(file, n=30)
  n<-grep("Tuned to", tmp)
  MidFreq<-as.numeric(gsub("[a-z,A-Z,., ]","",tmp[n]))
  return(MidFreq)
}


