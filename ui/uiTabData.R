############ tabData.R ############
tabPanel("Input",
         sidebarLayout(
           sidebarPanel(
             radioButtons(
               "data_type_input",
               choices = c("SQLite", "CSV","MySQL"),
               label = "Please select input type",
               selected = "SQLite"
             ),
             uiOutput('data_tab_additional')
           ),
           mainPanel(tabsetPanel(
             tabPanel("Logger Data",
                      dataTableOutput("data_tab_logger_table")),
             tabPanel("Antennae",
                      dataTableOutput("data_tab_antennae_table")),
             tabPanel("Frequencies",
                      dataTableOutput("data_tab_freq_table"))
           ))
         ))
