############ srvTabData.R ############

# read data
logger_data<- reactive({
  inFile <- input$data_logger_input
  if (is.null(inFile))
    return(NULL)
  switch (input$data_type_input, 
          CSV={ data <- read_logger_data(inFile$datapath,input$lines_to_skip,input$center_freq)},
          SQL={   # open db
            con <- dbConnect(RSQLite::SQLite(),inFile$datapath)
            data<-NULL
            # data$receiver <- 0
            for(t in dbListTables(con)) {
              data <- rbind(data,cbind(dbReadTable(con,t),receiver=t))
              # data$receiver <- c(t)
            }
            data$timestamp<-as.POSIXct(data$time, "%Y-%m-%d %H:%M:%S", tz="UTC")
            dbDisconnect(con)
            })
  data
})

# read project meta file: locations
antennae_data <- reactive({
  if (is.null(input$data_position_input)) return(NULL)
  read.csv2(input$data_position_input$datapath, dec=".", stringsAsFactors = FALSE, row.names = NULL)
  
})
