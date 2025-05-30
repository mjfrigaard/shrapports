#' Application Server
#'
#' Defines the server logic for the modular Tidy Tuesday app.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @export
app_server <- function(input, output, session) {

  logr_msg("Initializing app server", level = "INFO")

  # Add session info logging
  logr_msg(paste("Session started for user:", session$user), level = "DEBUG")

  tryCatch({
    # initialize modules

# observe({
#   browser()

    selected_data <- mod_var_input_server("var_input")

    logr_msg("Variable input module initialized", level = "DEBUG")

    # Get reactive values for report
    viz_module <- mod_viz_server(
      id = "viz",
      data = selected_data
      )
# })

    logr_msg("Visualization module initialized", level = "DEBUG")

    mod_table_server(
      id = "table",
      data = selected_data
      )
    logr_msg("Table module initialized", level = "DEBUG")

    # reactive expressions for report module
    selected_plot_type <- reactive({
      # This will need to be passed from the viz module
      # For now, we'll use a default
      "type"
    })

    dataset_title <- reactive({
      # Get the selected dataset title from the input module
      if (!is.null(selected_data()) && length(selected_data()) > 0) {
        # Try to get from the data or use a default
        "Selected TidyTuesday Dataset"
      } else {
        NULL
      }
    })

    # Initialize report module
    mod_report_server("report", selected_data, selected_plot_type, dataset_title)
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
}
