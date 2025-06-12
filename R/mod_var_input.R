#' Variable Input UI Module
#'
#' @param id Namespace ID
#'
#' @return UI elements for dataset title selection
#'
#' @export
#'
mod_var_input_ui <- function(id) {
  ns <- NS(id)
  logr_msg("Initializing variable input UI module", level = "DEBUG")
  tryCatch({
      choices <- unique(all_tt_combined$title)
      logr_msg(paste("Available dataset choices:", length(choices)), level = "INFO")

      tagList(
        selectInput(
          inputId = ns("dataset_title"),
          label = strong("Choose Dataset Title:"),
          choices = choices,
          selected = "Moore’s Law"
        ),
        code("reactiveValuesToList()"),
        verbatimTextOutput(ns("vals"))
      )
    },
    error = function(e) {
      logr_msg(paste("Error creating variable input UI:", e$message), level = "ERROR")

      # return minimal UI with error message
      tagList(
        h4("Error loading dataset choices", class = "text-danger"),
        p("Please check the data availability.")
      )
    }
  )
}

#' Variable Input Server Module - Enhanced with Dataset Title
#'
#' @param id Module ID
#'
#' @return A list with reactive expressions for both data and dataset title
#'
#' @export
#'
mod_var_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    logr_msg("Initializing variable input server module", level = "DEBUG")

    data <- reactive({
      req(input$dataset_title)
      logr_msg(paste("User selected dataset:", input$dataset_title), level = "INFO")

      tryCatch({
        result <- get_tt_data(input$dataset_title)
        if (length(result) == 0) {
          logr_msg("Empty dataset returned", level = "WARN")
          showNotification("No data available for selected dataset",
                         type = "warning", duration = 5)
        } else {
          logr_msg("Dataset successfully loaded in reactive", level = "SUCCESS")
        }
        return(result)

      }, error = function(e) {
        logr_msg(paste("Error in data reactive:", e$message), level = "ERROR")
        showNotification(paste("Error loading data:", e$message),
                       type = "error", duration = 10)
        return(list())
      })
    }) |>
      bindEvent(input$dataset_title)

    # print names of datasets from title
    output$vals <- renderPrint({
      req(data())
        title <- attr(data(), "clean_title")
        ds_names <- names(data())
        glue::glue("The {title} and {ds_names}")
    }) |>
      bindEvent(input$dataset_title)

    # return both data and dataset title
    return(
      list(
      data = data,
      dataset_title = reactive({ input$dataset_title })
        )
      )
  })
}
