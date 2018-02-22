shinyjs::disable("dl")

output$download_filtered_data_csv <- downloadHandler(
  
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    "filtered_data.csv"
  },
  
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
    write.csv2(filtered_data(),file,row.names = FALSE)
  }
)