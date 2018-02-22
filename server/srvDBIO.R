############ DBIO ############

# filter data and save it to new or existing databases using a csv files with filter settings

filter_data_and_save <- function(filepath_db,filepath_filterSettings, receiver){
  # open db
  con <- dbConnect(RSQLite::SQLite(),filepath_db)
  # import data
  data<- dbReadTable(con,receiver)
  # read file with filter settings
  filterSettings<-read.csv2(filepath_filterSettings,dec=".", stringsAsFactors = FALSE, row.names = NULL)
  # filter for characteristic length
  data<-filter_data_length(data,filterSettings$duration)
  # filter for bandwith
  data<-filter_signal_bandwith(data,filterSettings$bandwith)
  # filter for signal strength
  data<-filter_signal_strength(data,filterSettings$strength)
  #disconnect
  dbDisconnect(con)
}



output$download_filtered_data_sqlite <- downloadHandler(
    filename = function() {
      "filtered_data.sqlite"
    },
    content = function(file) {
      con <- dbConnect(RSQLite::SQLite(), file)
      if(!is.null(logger_data())){
        dbWriteTable(con,"rteu_logger_data",logger_data(),overwrite=TRUE)
      }
      if(!is.null(freqs())){
        dbWriteTable(con,"rteu_freqs",freqs(),overwrite=TRUE)
      }
      if(!is.null(antennae_data())){
        dbWriteTable(con,"rteu_antenna",antennae_data(),overwrite=TRUE)
      }
      dbDisconnect(con)
    }
    
  )
  
  

# read csv logger file to db
add_logger_file_to_db <- function(file,filepath_db,receiver_name=NULL){
  lines_to_skip<-findHeader(file)
  if (is.null(lines_to_skip)){
    print("Couldn't find the Headers in Logger file")
    return()
  } 
  MidFreq<-findMidFreq(file)
  if (is.null(MidFreq)){
    print("Couldn't find the Center Frequency in Logger file")
    return()
  } 
  
  data<-read.csv2(file, skip=lines_to_skip,stringsAsFactors = FALSE, dec = ".", skipNul = TRUE)
  
  if(!is.null(receiver_name)){
    data$receiver<-receiver_name
  }
  
  data$X<-NULL # bug in the log file - lines ends with ";"
  
  data$signal_freq<-(data$signal_freq+MidFreq)/1000 # Correct to abosult Frequency in kHz
  
  con = dbConnect(RSQLite::SQLite(), dbname=filepath_db)
  
  if(any(dbListTables(con)=="rteu_logger_data")){
    tmp_df<-dbReadTable(con,"rteu_logger_data")
    if(!any(names(tmp_df)=="receiver")){
      tmp_df$receiver<-"not_specified"
    }
    df<-unique(rbind(data,tmp_df))
    dbWriteTable(con, "rteu_logger_data", df, overwrite=TRUE)
  }
  else{
    dbWriteTable(con, "rteu_logger_data", data)
  }
  dbDisconnect(con)
}

add_multiple_logger_files_to_db <- function(files,filepath_db,receiver){
  for(f in files){
    add_logger_file_to_db(f,filepath_db,receiver)
  }
}

# reads all files in folder and adds found data to receiver table in db
get_all_logger_data_in_folder <-function(folder, file_db ,receiver){
  #get all file names first
  
  files <- list.files(folder)
  print(files)
  for(f in files){
    print(paste0(folder,f))
    add_logger_file_to_db(paste0(folder,f),file_db,receiver)
  }
  
}

# recursively reads all files in folder and adds them to db according to subfolder names.
# requires matching folder structure
get_logger_data_in_subfolders <-function(folder, file_db){
  #get all file names first
  
  folders <- list.dirs(folder, full.names = FALSE, recursive = FALSE)
  for(sf in folders){
    get_all_logger_data_in_folder(paste0(folder,"\\",sf,"\\"),file_db,sf)
    print(paste0(folder,"\\",sf,"\\"))
  }
}