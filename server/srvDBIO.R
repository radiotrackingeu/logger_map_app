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


# read csv logger file to db
add_logger_file_to_db <- function(file,filepath_db,receiver){
  lines_to_skip<-findHeaders(file)
  if (is.null(lines_to_skip)) return()
  data<-read.csv2(filepath, skip=lines_to_skip,stringsAsFactors = FALSE, dec = ".")
  data$X<-NULL
  con = dbConnect(RSQLite::SQLite(), dbname=filepath_db)
  if(any(dbListTables(con)==receiver)){
    tmp_df<-dbReadTable(con,receiver)
    df<-unique(rbind(data,tmp_df))
    dbWriteTable(con, paste0("receiver_",receiver), df, overwrite=TRUE)
  }
  else{
    dbWriteTable(con, paste0("receiver_",receiver), data)
  }
  dbDisconnect(con)
}

# reads all files in folder and adds found data to receiver table in db
get_all_logger_data_in_folder <-function(folder, file_db ,receiver){
  #get all file names first
  
  files <- list.files(folder)
  
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
    get_all_logger_data_in_folder(paste0(folder,sf,"/"),file_db,sf)
  }
}