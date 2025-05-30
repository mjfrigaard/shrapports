#' Table UI Module (reactable)
#'
#' @param id Namespace ID
#'
#' @return UI element for data table output using reactable
#' @export
mod_table_ui <- function(id) {
  ns <- NS(id)
  logr_msg("Initializing table UI module", level = "DEBUG")

  tryCatch({
    bslib::card(
      bslib::card_header("Data Preview"),
      bslib::card_body(
        reactable::reactableOutput(ns("table"))
      )
    )
  }, error = function(e) {
    logr_msg(paste("Error creating table UI:", e$message), level = "ERROR")

    bslib::card(
      bslib::card_header("Table Error"),
      bslib::card_body(
        h4("Error loading table interface", class = "text-danger")
      )
    )
  })
}

#' Table Server Module (reactable)
#'
#' @param id Module ID
#' @param data A reactive expression returning the dataset list
#'
#' @export
mod_table_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    logr_msg("Initializing table server module", level = "DEBUG")

    output$table <- reactable::renderReactable({

      tryCatch({
        req(data())

        if (length(data()) == 0) {
          logr_msg("No data available for table rendering", level = "WARN")
          return(NULL)
        }

        df_list <- data()

        # If multiple datasets, show the first one but indicate which file
        first_df <- df_list[[1]]
        first_name <- names(df_list)[1]

        logr_msg(paste("Rendering table for file:", first_name,
                      "with", nrow(first_df), "rows and",
                      ncol(first_df), "columns"), level = "INFO")

        if (!is.data.frame(first_df)) {
          logr_msg("First dataset is not a data frame", level = "ERROR")
          return(NULL)
        }

        # Limit to first 1000 rows for performance
        display_df <- if (nrow(first_df) > 1000) {
          logr_msg("Large dataset detected - limiting to first 1000 rows", level = "INFO")
          head(first_df, 1000)
        } else {
          first_df
        }

        # Create subtitle showing which file and if there are more
        subtitle <- if (length(df_list) > 1) {
          paste("Showing:", first_name, "(", length(df_list), "files total )")
        } else {
          paste("File:", first_name)
        }

        reactable::reactable(
          display_df,
          searchable = TRUE,
          sortable = TRUE,
          pagination = TRUE,
          defaultPageSize = 20,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(5, 10, 20, 50),
          highlight = TRUE,
          bordered = TRUE,
          striped = TRUE,
          resizable = TRUE,
          wrap = FALSE,
          defaultColDef = reactable::colDef(
            minWidth = 100,
            headerStyle = list(background = "#f7f7f8")
          )
        )

      }, error = function(e) {
        logr_msg(paste("Error rendering table:", e$message), level = "ERROR")

        # Return an empty reactable with error message
        reactable::reactable(
          data.frame(Error = paste("Failed to load data:", e$message)),
          searchable = FALSE,
          sortable = FALSE,
          pagination = FALSE
        )
      })
    })
  })
}
