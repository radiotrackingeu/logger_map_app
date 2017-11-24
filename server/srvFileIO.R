############ FileIO ############
options(shiny.maxRequestSize = 3000 * 1024 ^ 2)
#reads and reformats data from file. Adds col timestamp with posix-style readable time
read_logger_data <- function(filepath) {
  lines_to_skip <- findHeader(filepath) #skip meta data in files
  if (lines_to_skip < 0) return(NULL)
  mid_freq <- findMidFreq(filepath) # find center frequency of tuner
  if (mid_freq < 0) return(NULL)
  tryCatch({
    print('srvFileIO::read_logger_data says')
    print(paste('path:',filepath,'lines',lines_to_skip))
    data <-
      read.csv2(
        filepath,
        skip = lines_to_skip,
        stringsAsFactors = FALSE,
        dec = "."
      )
    data$timestamp <-
      as.POSIXct(data$time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    data$freq <- (data$freq + mid_freq) / 1000
    
  }, warning = function(w) {
    
  }, error = function(e) {
    validate(need(
      "",
      "An error occurred while parsing the file. Please check all settings."
    ))
    data <- NULL
    
  }, finally = {
    return(data)
  })
}

findHeader <- function(file) {
  n<--1
  tryCatch(
    {
      tmp <- readLines(file, n = 30)
      n <- grep("time;duration;freq;bw;strength", tmp) - 1
      if (length(n)==0) n<--1
    }, warning = function(w) {
      n<--1
    }, error = function(e) {
      n<--1
    }, finally = {
      return(n)
    }
  )
}

findMidFreq <- function(file) {
  MidFreq<--1
  tryCatch({
    tmp <- readLines(file, n = 30)
    n <- grep("Tuned to", tmp)
    MidFreq <- as.numeric(gsub("[a-z,A-Z,., ]", "", tmp[n]))
  }, warning = function(w) {
    MidFreq<--1
  }, error = function(e) {
    MidFreq<--1
  }, finally = {
    return(MidFreq)
    
  }
  )
}
