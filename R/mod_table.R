#' Table Module UI
#'
#' @param id The module ID
#'
#' @return A UI element
#'
#' @export
#'
table_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Data Summary"),
    bslib::card_body(
      gt::gt_output(ns("table"))
    )
  )
}

#' Table Module Server
#'
#' @param id The module ID
#' @param data_reactive Reactive expression returning the dataset
#' @param x_var_reactive Reactive expression returning the x variable
#' @param y_var_reactive Reactive expression returning the y variable
#' @param color_var_reactive Reactive expression returning the color variable
#'
#' @return A Shiny server module
#'
#' @export
#'
table_server <- function(id, data_reactive, x_var_reactive, y_var_reactive, color_var_reactive = NULL) {
  moduleServer(id, function(input, output, session) {

    # Create summary table for scatter plot
    summary_data <- reactive({
      req(data_reactive(), x_var_reactive(), y_var_reactive())

      df <- data_reactive()
      x_var <- x_var_reactive()
      y_var <- y_var_reactive()

      # Calculate overall correlation
      x_data <- df[[x_var]]
      y_data <- df[[y_var]]
      overall_cor <- cor(x_data, y_data, use = "pairwise.complete.obs")

      result <- data.frame(
        Group = "Overall",
        N = sum(!is.na(x_data) & !is.na(y_data)),
        Mean_X = mean(x_data, na.rm = TRUE),
        SD_X = sd(x_data, na.rm = TRUE),
        Mean_Y = mean(y_data, na.rm = TRUE),
        SD_Y = sd(y_data, na.rm = TRUE),
        Correlation = overall_cor
      )

      # If we have a color variable, add group-specific summaries
      if (!is.null(color_var_reactive())) {
        color_var <- color_var_reactive()

        # Check if the color variable exists in the data
        if (color_var %in% names(df)) {
          group_vals <- unique(df[[color_var]])

          for (grp in group_vals) {
            # Filter data for this group
            grp_data <- df[df[[color_var]] == grp, ]
            grp_x <- grp_data[[x_var]]
            grp_y <- grp_data[[y_var]]

            # Calculate correlation for this group
            grp_cor <- cor(grp_x, grp_y, use = "pairwise.complete.obs")

            # Add to results
            result <- rbind(result, data.frame(
              Group = as.character(grp),
              N = sum(!is.na(grp_x) & !is.na(grp_y)),
              Mean_X = mean(grp_x, na.rm = TRUE),
              SD_X = sd(grp_x, na.rm = TRUE),
              Mean_Y = mean(grp_y, na.rm = TRUE),
              SD_Y = sd(grp_y, na.rm = TRUE),
              Correlation = grp_cor
            ))
          }
        }
      }

      return(result)
    })

    # Generate table
    output$table <- gt::render_gt({
      req(summary_data())

      gt::gt(summary_data()) |>
        gt::tab_header(
          title = paste("Correlation Analysis:", y_var_reactive(), "vs", x_var_reactive())
        ) |>
        gt::fmt_number(
          columns = c("Mean_X", "SD_X", "Mean_Y", "SD_Y", "Correlation"),
          decimals = 3
        ) |>
        gt::cols_label(
          Group = if (!is.null(color_var_reactive())) stringr::str_to_title(color_var_reactive()) else "Group",
          Mean_X = paste("Mean", x_var_reactive()),
          SD_X = paste("SD", x_var_reactive()),
          Mean_Y = paste("Mean", y_var_reactive()),
          SD_Y = paste("SD", y_var_reactive()),
          Correlation = "Correlation (r)"
        ) |>
        gt::tab_footnote(
          footnote = "Pearson correlation coefficient",
          locations = gt::cells_column_labels(columns = "Correlation")
        )
    })

    # Return the table data for use in report generation
    return(list(
      table_output = reactive(output$table),
      summary_data = summary_data
    ))
  })
}
