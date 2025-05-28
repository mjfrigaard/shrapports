#' The application UI
#'
#' @return A Shiny UI
#'
#' @export
#'
app_ui <- function() {
  bslib::page_sidebar(
    title = tags$strong(
      tags$code("shrapports"),
      tags$em("- Data Explorer with Downloadable Report")
      ),
    sidebar = bslib::sidebar(
      width = "320px",
      var_input_ui("vars"),
      report_ui("report")
    ),
    bslib::card(
      bslib::card_header(
        em("Select a dataset, then download the report.")
      ),
      bslib::card_body(
        fillable = TRUE,
        vis_ui("visualization")
      ),
      bslib::card(
        bslib::card_body(
          fillable = TRUE,
          table_ui("table")
        )
      )
    )
  )
}
