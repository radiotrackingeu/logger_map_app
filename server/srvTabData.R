############ srvTabData.R ############

additional_input_fields <- reactive({
  input_tag_list <- tagList()
  logger_data_input_tag_csv <- fileInput(
    "data_logger_input_csv",
    "Logger data",
    multiple = FALSE,
    accept = NULL,
    width = NULL
  )
  logger_data_input_tag_sqlite <- fileInput(
    "data_logger_input_sqlite",
    "Logger data",
    multiple = FALSE,
    accept = NULL,
    width = NULL
  )  
  logger_data_input_receiver_tag <- textInput(
    "receiver_name_input",
    "Choose receiver name",
    value=tools::file_path_sans_ext(input$data_logger_input_csv)
  )
  logger_data_addButton_receiver_tag <- actionButton(
    "addButton",
    "Add"
  )
  antennae_input_tag <- fileInput(
    "data_position_input",
    "Upload antenna data",
    multiple = FALSE,
    accept = NULL,
    width = NULL
  )
  freq_input_tag <-       fileInput(
    "freq_file",
    "Upload frequency data",
    multiple = FALSE,
    accept = NULL,
    width = NULL
  )
  MySQL_input_host_tag <-  textInput(
    "MySQL_host",
    "Enter Host Name",
    "192.168.8.104"
  )
  MySQL_input_port_tag <-  numericInput(
    "MySQL_port",
    "Enter Port",
    3306
  )
  MySQL_input_user_tag <-  textInput(
    "MySQL_user",
    "Enter User Name",
    "rteu"
  )
  MySQL_input_pw_tag <-  passwordInput(
    "MySQL_pw",
    "Enter Password",
    "rteuv2!"
  )
  MySQL_input_action <- actionButton(
    "connect_to_db", 
    "Load Data"
  )
  MySQL_input_live <- checkboxInput(
    "update_auto", 
    "Live Data",
    FALSE
  )
  MySQL_input_live_filter <- checkboxInput(
    "update_auto_filter", 
    "Live Data Filter",
    FALSE
  )
  
  
  if (input$data_type_input == 'SQLite') {
    input_tag_list <-
      tagAppendChildren(input_tag_list, list(logger_data_input_tag_sqlite,logger_data_addButton_receiver_tag,antennae_input_tag, freq_input_tag))
  }
  if (input$data_type_input == 'CSV') {
    input_tag_list <-
      tagAppendChildren(input_tag_list, list(logger_data_input_tag_csv, logger_data_input_receiver_tag,logger_data_addButton_receiver_tag,antennae_input_tag, freq_input_tag))
  }
  if (input$data_type_input == 'MySQL') {
    input_tag_list <-
      tagAppendChildren(input_tag_list, list(MySQL_input_host_tag,MySQL_input_port_tag,MySQL_input_user_tag,MySQL_input_pw_tag,MySQL_input_action,freq_input_tag,logger_data_addButton_receiver_tag,MySQL_input_live,MySQL_input_live_filter))
  }
  return(input_tag_list)
})

output$data_tab_additional <- renderUI({
  additional_input_fields()
})

pool <- reactive({
  dbPool(
    drv = RMySQL::MySQL(),
    dbname = "rteu",
    host = input$MySQL_host,
    port = input$MySQL_port,
    username = input$MySQL_user,
    password = input$MySQL_pw
  )
})

onStop(function() {
  reactive(poolClose(pool()))
  print("DB Connections closed")
})

# check automatically every 2 seconds for new data
live_data <- reactivePoll(2000, session, 
  checkFunc = function() {
    if(is.null(input$connect_to_db))
      return(NULL)
    if(input$connect_to_db<1)
      return(NULL)
    dbGetQuery(pool(),"SELECT COUNT(*) FROM `signals` ")
  },
  valueFunc = function() {
    if(input$update_auto_filter){
      dbGetQuery(pool(),paste0("SELECT * FROM `signals` WHERE (`duration` >",input$signal_length[1],"&& `duration` < ",input$signal_length[2],") ORDER BY id DESC LIMIT 200"))
      }else{
        dbGetQuery(pool(),"SELECT * FROM `signals` ORDER BY id DESC LIMIT 200;")
      }
  }
)
data_sore <- reactiveValues()
# read data
get_data <- reactive({ 
  switch (input$data_type_input,
          CSV = {
            inFile <- input$data_logger_input_csv
            if (is.null(inFile))
              return(NULL)
            data <- read_logger_data(inFile$datapath)
            if (!('receiver' %in% colnames(data))) # reads data twice since the filename gets added afterwards
              data$receiver <- input$receiver_name_input
            if (is.null(data)) return(NULL)
          },
          SQLite = {
            inFile <- input$data_logger_input_sqlite
            if (is.null(inFile))
              return(NULL)
            # open db
            con <- dbConnect(RSQLite::SQLite(), inFile$datapath)
            if(!dbExistsTable(con, "rteu_logger_data")) {
              print("wrong sqlite db selected")
              dbDisconnect(con)
              return(NULL)
            }
            data <- dbReadTable(con, "rteu_logger_data")
            data$timestamp <- as.POSIXct(data$timestamp, tz = "UTC", origin = "1960-10-01")
            dbDisconnect(con)
          },
          MySQL = {
            if(is.null(input$connect_to_db))
              return(NULL)
            if(input$connect_to_db<1)
              return(NULL)
            # open db
            con<-pool()
            if (!dbExistsTable(con, "runs")&&!dbExistsTable(con, "signals")) {
              print("wrong connection or db corrupted")
              return(NULL)
            }
            if(!input$update_auto){
              data <- dbReadTable(con, "signals")
            }
            else{
              data<-live_data()
            }
            runs <- dbReadTable(con, "runs")
            data$timestamp <- as.POSIXct(data$timestamp, tz = "UTC")

            #merge data and get receicer name
            data<-merge(data,runs, by.x="run", by.y="id", all.x = FALSE, all.y = FALSE)
            data$signal_freq<-(data$signal_freq+data$center_freq)/1000
            data$receiver<-substrLeft(data$device,17)
            data<-data[c("max_signal","signal_bw","signal_freq","duration","samples","timestamp","receiver")]
            
          }
  )
  data
})

observeEvent(input$addButton,{
  data_sore$data<-unique.data.frame(rbind(get_data(),data_sore$data))
})

logger_data <- reactive({
  if(input$data_type_input=="MySQL"&&!is.null(input$connect_to_db)){
    if(input$update_auto){
      data_sore$data<-get_data()
    }
  }
  data_sore$data
})


freqs <- reactive({
  inFile <- input$freq_file
  switch (input$data_type_input,
          CSV = {
            if (is.null(inFile))
              return(NULL)
              data <- read.csv2(inFile$datapath, stringsAsFactors = FALSE)
          },
          SQLite = {
            if (is.null(input$data_logger_input_sqlite))
              return(NULL)
            con <- dbConnect(RSQLite::SQLite(), input$data_logger_input_sqlite$datapath)
            if (dbExistsTable(con, "rteu_freqs")) {
              data <- dbReadTable(con, "rteu_freqs")
            }
            dbDisconnect(con)
          },
          MySQL = {
            if (is.null(inFile))
              return(NULL)
            data <- read.csv2(inFile$datapath, stringsAsFactors = FALSE)
          }
  )
  data
})



# read project meta file: locations
antennae_data <- reactive({
  inFile <- input$data_position_input
  switch (input$data_type_input,
          CSV = {
            if (is.null(inFile))
              return(NULL)
            data <-
              read.csv2(
                input$data_position_input$datapath,
                dec = ".",
                stringsAsFactors = FALSE,
                row.names = NULL
              )
          },
          SQLite = {
            if (is.null(input$data_logger_input_sqlite))
              return(NULL)
            con <- dbConnect(RSQLite::SQLite(), input$data_logger_input_sqlite$datapath)
            if (dbExistsTable(con, "rteu_antenna")) {
              data <- dbReadTable(con, "rteu_antenna")
            }
            dbDisconnect(con)
          },
          MySQL = {
            if(is.null(input$connect_to_db))
              return(NULL)
            if(input$connect_to_db<1)
              return(NULL)
            # open db
            # con <- dbConnect(MySQL(), user=input$MySQL_user, password=input$MySQL_pw, dbname='rteu', host=input$MySQL_host)
            con <- pool()
            if (!dbExistsTable(con, "runs")&&!dbExistsTable(con, "signals")) {
              print("wrong connection or db corrupted")
              return(NULL)
            }
            data <- dbReadTable(con, "runs")
            data$receiver<-substrLeft(data$device,17)
            #renaming of variables
          }
  )
  data
})

output$data_tab_logger_table <- renderDataTable({
  validate(need(logger_data(), "Please provide logger data file."))
  logger_data()
}, options = list(pageLength = 10))

output$data_tab_get_data_table <- renderDataTable({
  validate(need(get_data(), "Please provide logger data file."))
  get_data()
}, options = list(pageLength = 10))

output$data_tab_freq_table <- renderDataTable({
  validate(need(freqs(), "Please provide antennae data file."))
  freqs()
}, options = list(pageLength = 10))

output$data_tab_antennae_table <- renderDataTable({
  validate(need(antennae_data(), "Please provide frequencies data file."))
  antennae_data()
}, options = list(pageLength = 10))

substrLeft <- function(x, n){
  sapply(x, function(xx)
    substr(xx, 0, nchar(xx)-n)
  )
}
