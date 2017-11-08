############ srvTabData.R ############

# read data
logger_data<- reactive({
  inFile <- input$data_logger_input
  if (is.null(inFile))
    return(NULL)
  switch (input$data_type_input, 
          CSV={ 
            data <- read_logger_data(inFile$datapath)
            data$receiver <- "not_specified"
          },
          SQL={   # open db
            con <- dbConnect(RSQLite::SQLite(),inFile$datapath)
            data <- dbReadTable(con,"rteu_logger_data")
            data$timestamp<-as.POSIXct(data$time, "%Y-%m-%d %H:%M:%S", tz="UTC")
            dbDisconnect(con)
            })
  data
})

freqs <- reactive({
    inFile <- input$freq_file
    if(is.null(inFile)) return(NULL)
    return(read.csv2(inFile$datapath,stringsAsFactors = FALSE))
})

# read project meta file: locations
antennae_data <- reactive({
  if(is.null(input$data_position_input)) return(NULL)
  read.csv2(input$data_position_input$datapath, dec=".", stringsAsFactors = FALSE, row.names = NULL)
})


