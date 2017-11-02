############ tabData.R ############
tabPanel("DB",
         fluidPage(
           radioButtons("data_type_input", choices=c("new", "existing"), label = "Please select input type"),
           fileInput("data_db_input", "Select Database", multiple = FALSE, accept = NULL, width = NULL),
           fileInput("data_multiple_logger", "Select logger files", multiple = TRUE, accept = NULL, width = NULL),
           textInput("receiver_name", "Name of Receiver", value = "Rec_1"),
           actionButton("add_to_db","Add files to db")
         )
)
