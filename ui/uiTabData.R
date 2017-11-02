############ tabData.R ############
tabPanel("CSV",
         fluidPage(
           radioButtons("data_type_input", choices=c("CSV", "SQL"), label = "Please select input type"),
           fileInput("data_logger_input", "Upload Logger Data", multiple = FALSE, accept = NULL, width = NULL),
           fileInput("data_position_input", "Upload Position Data", multiple = FALSE, accept = NULL, width = NULL)
         )
)
