#' The application server logic
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @return A Shiny server function
#'
#' @export
#'
app_server <- function(input, output, session) {

  # Variable input module ----
  var_inputs <- var_input_server("vars")

  # initialize modules ----
  vis_output <- vis_server(id = "visualization",
                          data_reactive = var_inputs$data,
                          x_var_reactive = var_inputs$x_var,
                          y_var_reactive = var_inputs$y_var,
                          color_var_reactive = var_inputs$color_var)

  table_output <- table_server(id = "table",
                              data_reactive = var_inputs$data,
                              x_var_reactive = var_inputs$x_var,
                              y_var_reactive = var_inputs$y_var,
                              color_var_reactive = var_inputs$color_var)

  # report generation ----
  report_server(id = "report",
               dataset_name_reactive = var_inputs$dataset_name,
               data_reactive = var_inputs$data,
               x_var_reactive = var_inputs$x_var,
               y_var_reactive = var_inputs$y_var,
               color_var_reactive = var_inputs$color_var,
               table_data = table_output$summary_data)
}
