#' Create an error report HTML file
#'
#' @param file Output file path
#' @param error_msg Error message
#' @param dataset_title Dataset title
#'
#' @keywords internal
#'
create_error_report <- function(file, error_msg, dataset_title = "Unknown") {

  logr_msg("Creating error report", level = "INFO")

  error_html <- paste0(
    '<!DOCTYPE html>
    <html>
    <head>
      <title>Report Generation Error</title>
      <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .error { color: #dc3545; background: #f8d7da; padding: 20px; border-radius: 5px; }
        .info { color: #0c5460; background: #d1ecf1; padding: 15px; border-radius: 5px; margin-top: 20px; }
      </style>
    </head>
    <body>
      <h1>Report Generation Failed</h1>
      <div class="error">
        <h3>Error Details:</h3>
        <p>', error_msg, '</p>
      </div>
      <div class="info">
        <h3>Dataset:</h3>
        <p>', dataset_title, '</p>
        <h3>Timestamp:</h3>
        <p>', Sys.time(), '</p>
      </div>
      <p>Please try again or contact support if the problem persists.</p>
    </body>
    </html>'
  )

  writeLines(error_html, file)
}
