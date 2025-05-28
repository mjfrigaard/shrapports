#' Report Module UI
#'
#' @param id The module ID
#'
#' @return A UI element
#'
#' @export
#'
report_ui <- function(id) {
  ns <- NS(id)
  downloadButton(
    outputId = ns("download_report"),
    label = "Download Report")
}

#' Report Module Server
#'
#' @param id The module ID
#' @param dataset_name_reactive Reactive expression returning the dataset name
#' @param data_reactive Reactive expression returning the current dataset
#' @param x_var_reactive Reactive expression returning the x variable name
#' @param y_var_reactive Reactive expression returning the y variable name
#' @param color_var_reactive Reactive expression returning the color variable name
#' @param table_data Reactive expression returning the summary table data
#'
#' @return A Shiny server module
#'
#' @export
#'
report_server <- function(id, dataset_name_reactive, data_reactive, x_var_reactive,
                          y_var_reactive, color_var_reactive = NULL, table_data = NULL) {
  moduleServer(id, function(input, output, session) {

    # Report download handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste(dataset_name_reactive(), "-report-", Sys.Date(), ".html", sep = "")
      },
      content = function(file) {
        # Create a temporary Rmd file
        tempReport <- file.path(tempdir(), "report.Rmd")
        # Get the template
        template_path <- system.file("rmd", "report_template.Rmd", package = "shrapports")
        # Copy the template to a temporary file
        file.copy(template_path, tempReport, overwrite = TRUE)

        # Parameters to pass to the Rmd document
        params <- list(
          dataset_name = dataset_name_reactive(),
          current_data = data_reactive(),
          x_variable = x_var_reactive(),
          y_variable = y_var_reactive(),
          color_variable = if (!is.null(color_var_reactive)) color_var_reactive() else NULL,
          summary_data = if (!is.null(table_data)) table_data() else NULL
        )

        # Show a progress notification
        withProgress(message = 'Generating report...', {
          incProgress(0.3, detail = "Processing data")
          # Render the report
          rmarkdown::render(tempReport,
                            output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv()))
          incProgress(0.7, detail = "Finalizing report")
        })
      }
    )
  })
}
