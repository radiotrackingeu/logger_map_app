############ FileIO ############

#reads and reformats data from file. Adds col timestamp with posix-style readable time
read_logger_data <- function(filepath,lines_to_skip=-1,mid_freq=150100){
  if (lines_to_skip<0) {
    lines_to_skip <- findHeader(filepath)
    if (is.null(lines_to_skip)) return(NULL)
    updateNumericInput(session,"lines_to_skip", "Lines in File to skip", lines_to_skip)
    print(paste("Found headers in line",lines_to_skip))
  }
  data<-read.csv2(filepath, skip=lines_to_skip,stringsAsFactors = FALSE, dec = ".")
  data$duration<-as.numeric(data$duration) # Duration
  data$strength<-as.numeric(data$strength) #HighLevel
  data$freq<-as.numeric(data$freq)/1000 #RelFreq1
  #data<-na.omit(data)
  data$timestamp<-as.POSIXct(data$time, "%Y-%m-%d %H:%M:%S", tz="UTC")
  data$freq<-data$freq+mid_freq
  #data<-na.omit(data)
  return(data)
}

findHeader <- function(file) {
  tmp<-readLines(file, n=30)
  n<-grep("time;duration;freq;bw;strength;", tmp)-1  
}
