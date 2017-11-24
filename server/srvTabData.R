############ srvTabData.R ############

observeEvent(input$data_type_input, {
  # print(paste('selected',input$data_type_input))
  reset('data_logger_input')
})

additional_input_fields <- reactive({
  input_tag_list <- tagList()
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
  if (input$data_type_input == 'CSV') {
    input_tag_list <-
      tagAppendChildren(input_tag_list, list(antennae_input_tag, freq_input_tag))
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

# read data
logger_data <- reactive({
  inFile <- input$data_logger_input
  if (is.null(inFile))
    return(NULL)
  switch (input$data_type_input,
          CSV = {
            data <- read_logger_data(inFile$datapath)
            if (is.null(data)) return(NULL)
            if (!('receiver' %in% colnames(data)))
              data$receiver <- "not_specified"
          },
          SQL = {
            # open db
            con <- dbConnect(RSQLite::SQLite(), inFile$datapath)
            if (!dbExistsTable(con, "rteu_logger_data")) {
              print("wrong sqlite db selected")
              dbDisconnect(con)
              return(NULL)
            }
            data <- dbReadTable(con, "rteu_logger_data")
            data$timestamp <-
              as.POSIXct(data$time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
            dbDisconnect(con)
          })
  data
})

freqs <- reactive({
  data <- NULL
  if (is.null(input$data_logger_input))
    return(NULL)
  
  if (input$data_type_input == 'SQL') {
    con <- dbConnect(RSQLite::SQLite(), input$data_logger_input$datapath)
    if (dbExistsTable(con, "rteu_freqs")) {
      data <- dbReadTable(con, "rteu_freqs")
    }
    dbDisconnect(con)
  }
  if (!is.null(data))
    return(data)
  
  inFile <- input$freq_file
  if (is.null(inFile))
    return(NULL)
  tryCatch({
    data <- read.csv2(inFile$datapath, stringsAsFactors = FALSE)
    print('frequencies read')
  }, error = function(e) {
    validate(need(
      "",
      "An error occurred while parsing the file. Please check all settings."
    ))
    data <- NULL
  }, finally = {
    return(data)
  })
})

# read project meta file: locations
antennae_data <- reactive({
  data <- NULL
  if (!is.null(input$data_logger_input) && input$data_type_input=='SQL') {
    con <- dbConnect(RSQLite::SQLite(), input$data_logger_input$datapath)
    if (dbExistsTable(con, "rteu_antenna")) {
      data <- dbReadTable(con, "rteu_antenna")
    }
    dbDisconnect(con)
  }
  else{
    if (is.null(input$data_position_input))
      return(NULL)
    data <-
      read.csv2(
        input$data_position_input$datapath,
        dec = ".",
        stringsAsFactors = FALSE,
        row.names = NULL
      )
  }
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
