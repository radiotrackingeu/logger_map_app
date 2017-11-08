############ tabData.R ############
tabPanel("Input",
         sidebarLayout(
           sidebarPanel(
           radioButtons("data_type_input", choices=c("CSV", "SQL"), label = "Please select input type", selected = "SQL"),
           fileInput("data_logger_input", "Upload Logger Data", multiple = FALSE, accept = NULL, width = NULL),
           fileInput("data_position_input", "Upload Position Data", multiple = FALSE, accept = NULL, width = NULL),
           fileInput("freq_file", "Upload Frequency Data", multiple = FALSE, accept = NULL, width = NULL)
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Logger Data",
                 dataTableOutput("data_tab_logger_table")
               ),
               tabPanel("Antennae",
                        dataTableOutput("data_tab_antennae_table")
                 
               ),
               tabPanel("Frequencies",
                        dataTableOutput("data_tab_freq_table")
                 
               )
             )
           )
          )
)
