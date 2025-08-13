source_without_messages <- function(file) {
  tryCatch({
    suppressWarnings(suppressMessages(source(file)))
  }, error = function(e) {
    error_file <- file.path(outputFolder, "study_results", paste0(basename(file), "_error.txt"))
    error_message <- paste0("Error in file: ", file, "\n", 
                           "Error message: ", e$message, "\n",
                           "Timestamp: ", Sys.time(), "\n",
                           "Call stack: ", paste(capture.output(traceback()), collapse = "\n"))
    writeLines(error_message, error_file)
    message("Error occurred while sourcing ", file, ". Error details written to ", error_file, ". Please continue with other scripts but report to study team.")
  })
}
