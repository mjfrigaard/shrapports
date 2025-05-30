#' Application UI
#'
#' Constructs the user interface using `bslib` components.
#'
#' @return A UI definition for a Shiny app
#'
#' @export
app_ui <- function() {
  bslib::page_sidebar(
    title = "shrapports (a #TidyTuesday explorer)",
    sidebar = bslib::sidebar(
      mod_var_input_ui("var_input"),
      mod_report_ui("report")
    ),
    bslib::navset_tab(
      bslib::nav_panel("Inspect",
        mod_viz_ui("viz")
        ),
      bslib::nav_panel("Preview",
        mod_table_ui("table")
        )
    )
  )
}
