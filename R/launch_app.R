#' Launch the shrapports application
#'
#' This function starts the Shiny application for generating
#' downloadable reports from data visualizations.
#'
#' @export
#'
#' @import shiny
#'
#' @examples
#' \dontrun{
#' launch_app()
#' }
launch_app <- function() {
  shinyApp(app_ui(), app_server)
}
