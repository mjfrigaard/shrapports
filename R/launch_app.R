#' Launch the Tidy Tuesday Explorer App
#'
#' This function launches the Shiny application for exploring Tidy Tuesday
#' datasets.
#'
#' @param ... Additional arguments passed to shinyApp()
#' @param log_file Optional path to log file for application logs
#' @param json Logical, whether to use JSON logging format
#'
#' @import shiny
#'
#' @return A Shiny app object
#'
#' @export
#'
launch_app <- function(..., log_file = NULL, json = FALSE) {

  # initialize logging
  if (!is.null(log_file)) {
    logr_msg("Application starting with file logging enabled",
             level = "INFO", log_file = log_file, json = json)
  } else {
    logr_msg("Application starting", level = "INFO")
  }

  tryCatch({
    # check data availability
    if (!exists("all_tt_combined")) {
      logr_msg("Loading required data objects", level = "INFO")
      data("all_tt_combined", package = "shrapports", envir = .GlobalEnv)
    }

    logr_msg("Data objects verified", level = "SUCCESS")

    # Create and launch app
    app <- shinyApp(
      ui = app_ui(),
      server = app_server,
      ...
    )

    logr_msg("Shiny app object created successfully", level = "SUCCESS")

    return(app)

  }, error = function(e) {
    logr_msg(paste("Failed to launch application:", e$message),
             level = "FATAL", log_file = log_file, json = json)

    stop("Application failed to launch: ", e$message)
  })
}
