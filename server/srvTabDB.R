############ srvTabData.R ############

# read data to db
logger_data<- reactive({
  inFile <- input$data_logger_input
  if (is.null(inFile))
    return(NULL)
  print(inFile)
  switch (input$data_type_input, 
          CSV={ data <- read_logger_data(inFile$datapath)},
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
