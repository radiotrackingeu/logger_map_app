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
            for(t in dbListTables(con)) {
              data <- rbind(data,dbReadTable(con,t))
              #data$receiver <- t
            }
            dbDisconnect(con)
            })
  data
})

# read project meta file: locations
antennae_data <- reactive({
  if (is.null(input$data_position_input)) return(NULL)
  read.csv2(input$data_position_input$datapath, dec=".", stringsAsFactors = FALSE, row.names = NULL)
  
})
