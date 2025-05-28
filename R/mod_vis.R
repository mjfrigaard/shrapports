#' Visualization Module UI
#'
#' @param id The module ID
#'
#'
#' @return A UI element
#' @export
vis_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Visualization"),
    bslib::card_body(
      plotOutput(ns("plot"))
    )
  )
}

#' Visualization Module Server
#'
#' @param id The module ID
#' @param data_reactive Reactive expression returning the dataset
#' @param x_var_reactive Reactive expression returning the x variable
#' @param y_var_reactive Reactive expression returning the y variable
#' @param color_var_reactive Reactive expression returning the color variable
#'
#'
#' @return A Shiny server module
#' @export
vis_server <- function(id, data_reactive, x_var_reactive, y_var_reactive, color_var_reactive = NULL) {
  moduleServer(id, function(input, output, session) {

    # scatter plot
    output$plot <- renderPlot({
      req(data_reactive(), x_var_reactive(), y_var_reactive())

      plot <- ggplot2::ggplot(data_reactive(),
                            ggplot2::aes_string(
                              x = x_var_reactive(),
                              y = y_var_reactive()
                            ))

      # color aesthetic if a color variable is provided
      if (!is.null(color_var_reactive())) {
        plot <- plot +
          ggplot2::geom_point(ggplot2::aes_string(color = color_var_reactive()),
                             alpha = 0.7, size = 2) +
          ggplot2::labs(color = stringr::str_to_title(color_var_reactive()))
      } else {
        plot <- plot + ggplot2::geom_point(alpha = 0.7, size = 2)
      }

      # labels and theme
      plot <- plot +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste(y_var_reactive(), "vs", x_var_reactive()),
          x = x_var_reactive(),
          y = y_var_reactive(),
          color = color_var_reactive())

      # return plot
      plot
    })

    # return plot output ----
    return(reactive(output$plot))
  })
}
