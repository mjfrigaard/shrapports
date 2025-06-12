#' Application Server - Enhanced Parameter Handling
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @export
#'
app_server <- function(input, output, session) {

  logr_msg("Initializing app server", level = "INFO")

  # Add session info logging
  logr_msg(paste("Session started for user:", session$user), level = "DEBUG")

  tryCatch({
    # initialize modules
    var_input_result <- mod_var_input_server("var_input")
    # return the data and title
    selected_data <- var_input_result$data
    dataset_title <- var_input_result$dataset_title

    logr_msg("Variable input module initialized", level = "DEBUG")

    # reactive values for report
    viz_result <- mod_viz_server("viz", selected_data)
    logr_msg("Visualization module initialized", level = "DEBUG")

    mod_table_server("table", selected_data)
    logr_msg("Table module initialized", level = "DEBUG")

    # Initialize report module with proper reactive expressions
    mod_report_server("report", selected_data, viz_result, dataset_title)
    logr_msg("Report module initialized", level = "DEBUG")

    logr_msg("All modules successfully initialized", level = "SUCCESS")

    # Add session end logging
    session$onSessionEnded(function() {
      logr_msg(paste("Session ended for user:", session$user), level = "INFO")
    })

  }, error = function(e) {
    logr_msg(paste("Critical error in app server initialization:", e$message),
             level = "FATAL")

    # Show user-friendly error
    showNotification(
      "Application failed to initialize. Please refresh the page.",
      type = "error",
      duration = NULL
    )
  })

     output$dev <- renderPrint({
      vals <- reactiveValuesToList(x = input, all.names = TRUE)
      str(vals)
    })

}
