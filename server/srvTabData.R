############ srvTabData.R ############

observeEvent(input$data_type_input, {
  # print(paste('selected',input$data_type_input))
    reset('data_logger_input')
})

additional_input_fields <- reactive({
  input_tag_list <- tagList()
  logger_data_input_tag <- fileInput(
    "data_logger_input",
    "Upload Logger Data",
    multiple = FALSE,
    accept = NULL,
    width = NULL
  )
  antennae_input_tag <- fileInput(
    "data_position_input",
    "Upload Position Data",
    multiple = FALSE,
    accept = NULL,
    width = NULL
  )
  freq_input_tag <-       fileInput(
    "freq_file",
    "Upload Frequency Data",
    multiple = FALSE,
    accept = NULL,
    width = NULL
  )
  MySQL_input_host_tag <-  textInput(
    "MySQL_host",
    "Enter Host Name",
    "192.168.1.1"
  )
  MySQL_input_user_tag <-  textInput(
    "MySQL_user",
    "Enter User Name",
    "root"
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
  
  
  if (input$data_type_input == 'SQLite') {
    input_tag_list <-
      tagAppendChildren(input_tag_list, list(logger_data_input_tag))
  }
  if (input$data_type_input == 'CSV') {
    input_tag_list <-
      tagAppendChildren(input_tag_list, list(logger_data_input_tag, antennae_input_tag, freq_input_tag))
  }
  if (input$data_type_input == 'MySQL') {
    input_tag_list <-
      tagAppendChildren(input_tag_list, list(MySQL_input_host_tag,MySQL_input_user_tag,MySQL_input_pw_tag,MySQL_input_action,freq_input_tag,MySQL_input_live))
  }
  else {
    if (!is.null(logger_data()) && is.null(antennae_data())) {
      input_tag_list <- tagAppendChild(input_tag_list, antennae_input_tag)
    }
    if (!is.null(logger_data()) && is.null(freqs())) {
      input_tag_list <- tagAppendChild(input_tag_list, freq_input_tag)
    }
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
    username = input$MySQL_user,
    password = input$MySQL_pw
  )
  })

live_data <- reactivePoll(2000, session, 
                          checkFunc = function() {
                            if(is.null(input$connect_to_db))
                              return(NULL)
                            if(input$connect_to_db<1)
                              return(NULL)
                            dbGetQuery(pool(),"SELECT COUNT(*) FROM `signals` ")
                          },
                          valueFunc = function() {
                            #dbReadTable(pool(), "signals")
                            dbGetQuery(pool(),"SELECT * FROM `signals` ORDER BY timestamp DESC LIMIT 50;")
                          }
                          )


# read data
logger_data <- reactive({ 

  inFile <- input$data_logger_input
  switch (input$data_type_input,
          CSV = {
            if (is.null(inFile))
              return(NULL)
            data <- read_logger_data(inFile$datapath)
            if (is.null(data)) return(NULL)
            if (!('receiver' %in% colnames(data)))
              data$receiver <- "not_specified"
          },
          SQLite = {
            if (is.null(inFile))
              return(NULL)
            # open db
            con <- dbConnect(RSQLite::SQLite(), inFile$datapath)
            if (!dbExistsTable(con, "rteu_logger_data")) {
              print("wrong sqlite db selected")
              dbDisconnect(con)
              return(NULL)
            }
            data <- dbReadTable(con, "rteu_logger_data")
            data$timestamp <-
              #as.POSIXct(data$time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
              as.POSIXct(data$time, tz = "UTC")
            dbDisconnect(con)
          },
          MySQL = {
            if(is.null(input$connect_to_db))
              return(NULL)
            if(input$connect_to_db<1)
              return(NULL)
            # open db
            #con <- dbConnect(MySQL(), user=input$MySQL_user, password=input$MySQL_pw, dbname='rteu', host=input$MySQL_host)
            con<-pool()
            if (!dbExistsTable(con, "runs")&&!dbExistsTable(con, "signals")) {
              print("wrong connection or db corrupted")
              #dbDisconnect(con)
              return(NULL)
            }
            print(input$MySQL_input_live)
            if(!input$update_auto){
              data <- dbReadTable(con, "signals")
            }
            else{
              data<-live_data()
            }
            runs <- dbReadTable(con, "runs")
            data$timestamp <- as.POSIXct(data$timestamp, tz = "UTC")
            #renaming of variables
            
            data$freq<-data$signal_freq
            data$bw<-data$signal_bw
            data$strength<-data$max_signal
            
            #get receicer name
            data<-merge(data,runs, by.x="run", by.y="id", all.x = FALSE, all.y = FALSE)
            data$receiver<-substrLeft(data$device,17)
            data$freq<-(data$signal_freq+runs$center_freq)/1000
            
            #dbDisconnect(con)
          }
          )
  data
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
            if (is.null(input$data_logger_input))
              return(NULL)
            con <- dbConnect(RSQLite::SQLite(), input$data_logger_input$datapath)
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
            if (is.null(input$data_logger_input))
              return(NULL)
            con <- dbConnect(RSQLite::SQLite(), input$data_logger_input$datapath)
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
              dbDisconnect(con)
              return(NULL)
            }
            data <- dbReadTable(con, "runs")
            data$Receiver<-substrLeft(data$device,16)
            data$Long<-data$pos_x
            data$Lat<-data$pos_y
            data$Direction<-data$orientation
            data$Beamwidth<-data$beam_width
            data$Gain<-data$gain
            #renaming of variables
            
            data$freq<-data$signal_freq
            data$bw<-data$signal_bw
            data$strength<-data$max_signal
            data$receiver<-"missing"
            
            #dbDisconnect(con)
          }
          
  )
  data
})

output$data_tab_logger_table <- renderDataTable({
  validate(need(logger_data(), "Please provide logger data file."))
  logger_data()[1:50,]
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
