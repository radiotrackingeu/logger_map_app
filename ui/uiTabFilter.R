############ tabGraph.R ############
tabPanel("Filter",
         fluidRow(
           column(9,
           column(4,
                  checkboxInput("filter_length",strong("Signal length"),value = FALSE),
                  sliderInput("signal_length",
                              "",
                              min = 0.0005,
                              max = 0.050,
                              value = c(0.01,0.03))
           ),
           column(4,
                  checkboxInput("filter_strength",strong("Signal Strength"),value = FALSE),
                  sliderInput("signal_strength",
                              "",
                              min = -65,
                              max = 10,
                              value = c(-55,-5))
           ),
           column(4,
                  checkboxInput("filter_bw",strong("Bandwith"),value = FALSE),
                  sliderInput("signal_bw",
                              "",
                              #"Bandwidth:",
                              min = 500,
                              max = 50000,
                              value = c(2000,8000))
           ),
           tabsetPanel(type = "tabs",
                       tabPanel("Frequency",
                                plotOutput("histo")
                       ),
                       tabPanel("Duration",
                                plotOutput("histo_length")
                       ),
                       tabPanel("Signal Strength",
                                plotOutput("histo_strength")
                       ),
                       tabPanel("Signal Bandwidth",
                                plotOutput("histo_bandwidth")
                       ),
                       tabPanel("Results",plotOutput("facet"))
           )
           ),
           column(3,
                  checkboxInput("filter_one_freq",strong("Single Frequency kHz"),value = FALSE),
                  uiOutput("single_freq_num_input"),
                  checkboxInput("filter_freq",strong("Multiple Frequency Filter"),value = FALSE),
                  sliderInput("freq_error",
                              "Frequency Error:",
                              min = 1,
                              max = 30,
                              value = 5),
                  selectInput("input_select_receiver", "Select Receiver/s", choices =NULL, multiple = TRUE, selectize = TRUE),
                  textOutput("total_counts"),
                  downloadButton("download_filtered_data_sqlite")
                  )
             
           ),
         column(1),
         column(10,
                sliderInput("slider_datetime", "Date & Time:", 
                            min=as.POSIXlt("2017-09-24 00:00:00", "UTC"),
                            max=as.POSIXlt("2017-09-29 23:59:59", "UTC"),
                            value=c(as.POSIXlt("2017-09-24 00:00:00", "UTC"),
                                    as.POSIXlt("2017-09-29 23:59:59", "UTC")
                            ),
                            timezone = "UTC",
                            dragRange = TRUE,
                            animate = TRUE,
                            width = "100%")),
         column(1)
         
         )
