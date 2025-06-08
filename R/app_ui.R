#' Application UI
#'
#' Constructs the user interface using `bslib` components.
#'
#' @return A UI definition for a Shiny app
#'
#' @export
app_ui <- function() {
  bslib::page_sidebar(
    title = h4(strong("shrapports"), "(",em("explore #TidyTuesday data"), ")"),
    sidebar = bslib::sidebar(
      padding = c(12,12,12,12),
      width = "280px",
      mod_var_input_ui("var_input"),
      tags$hr(),
      mod_report_ui("report")
    ),
    bslib::navset_tab(
      bslib::nav_panel("Visualization",
          br(),
          strong("Reactive values from ",
              code("app_server()")
            ),
          verbatimTextOutput("dev"),
          br(),
          mod_viz_ui("viz")
        ),
      bslib::nav_panel("Data Table",
          mod_table_ui("table")
        )
    )
  )
}
