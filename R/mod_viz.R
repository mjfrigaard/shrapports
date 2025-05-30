#' Visualization UI Module (inspectdf-compatible)
#'
#' @param id Namespace ID
#'
#' @return UI output for one or more inspectdf plots
#'
#' @export
mod_viz_ui <- function(id) {
  ns <- NS(id)

  logr_msg("Initializing visualization UI module", level = "DEBUG")

  tryCatch({
    tagList(
      selectInput(ns("plot_type"),
                  label = "Choose plot type",
                  choices = c("Variable Types" = "type",
                            "Missing Data" = "na",
                            "Correlations" = "cor",
                            "Numeric Variables" = "num",
                            "Categorical Variables" = "cat"),
                  selected = "type"),
      uiOutput(ns("plot_ui"))
    )
  }, error = function(e) {
    logr_msg(paste("Error creating visualization UI:", e$message), level = "ERROR")

    tagList(
      h4("Error loading visualization interface", class = "text-danger"),
      p("Please refresh the page.")
    )
  })
}

#' Visualization Server Module (inspectdf-compatible) - Enhanced
#'
#' @param id Module ID
#' @param data A reactive expression returning the dataset list
#'
#' @return A reactive value containing the current plot type
#'
#' @export
mod_viz_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    logr_msg("Initializing visualization server module", level = "DEBUG")

    # return the current plot type for use in other modules
    current_plot_type <- reactive({
      input$plot_type
    })

# observe({
 # browser()

    output$plot_ui <- renderUI({

      tryCatch({
        req(data())

        if (length(data()) == 0) {
          logr_msg("No data available for plotting", level = "WARN")
          return(
            bslib::card(
              bslib::card_header("No Data Available"),
              bslib::card_body(
                h4("Please select a dataset to view visualizations.",
                   class = "text-muted text-center")
              )
            )
          )
        }

        logr_msg(paste("Generating plot UI for plot type:", input$plot_type), level = "INFO")

        plots <- inspect_plot(ttd = data(), plot = input$plot_type)

        if (is.null(plots)) {
          logr_msg("No plot generated - returning message", level = "WARN")
          return(
            bslib::card(
              bslib::card_header("Plot Not Available"),
              bslib::card_body(
                h4("No plot available for this selection.",
                   class = "text-muted text-center")
              )
            )
          )
        }

        if (inherits(plots, "gg")) {
          logr_msg("Single ggplot detected", level = "DEBUG")
          bslib::card(
            bslib::card_body(
              plotOutput(session$ns("main_plot"), height = "500px")
            )
          )
        } else if (is.list(plots) && all(purrr::map_lgl(plots, inherits, "gg"))) {
          logr_msg(paste("Multiple plots detected:", length(plots)), level = "DEBUG")

          plot_outputs <- lapply(seq_along(plots), function(i) {
            plotname <- paste0("plot_", i)
            bslib::card(
              bslib::card_header(paste("Plot", i)),
              bslib::card_body(
                plotOutput(session$ns(plotname), height = "400px")
              )
            )
          })
          do.call(tagList, plot_outputs)
        } else {
          logr_msg("Invalid plot format detected", level = "ERROR")
          bslib::card(
            bslib::card_header("Error"),
            bslib::card_body(
              h4("Invalid plot format.", class = "text-danger")
            )
          )
        }

      }, error = function(e) {
        logr_msg(paste("Error in plot UI rendering:", e$message), level = "ERROR")

        bslib::card(
          bslib::card_header("Visualization Error"),
          bslib::card_body(
            h4("Error generating visualization", class = "text-danger"),
            p(paste("Details:", e$message), class = "text-muted small")
          )
        )
      })
    })

# })

    observe({

      tryCatch({
        req(data())

        if (length(data()) == 0) {
          logr_msg("No data available for plot rendering", level = "WARN")
          return()
        }

        plots <- inspect_plot(ttd = data(), plot = input$plot_type)

        if (inherits(plots, "gg")) {
          logr_msg("Rendering single plot", level = "DEBUG")
          output$main_plot <- renderPlot({
            plots
          }, res = 96)
        } else if (is.list(plots)) {
          logr_msg(paste("Rendering", length(plots), "plots"), level = "DEBUG")

          for (i in seq_along(plots)) {
            local({
              plotname <- paste0("plot_", i)
              plot_obj <- plots[[i]]
              output[[plotname]] <- renderPlot({
                plot_obj
              }, res = 96)
            })
          }
        }

      }, error = function(e) {
        logr_msg(paste("Error in plot rendering observer:", e$message), level = "ERROR")

        # Create error plot output
        output$main_plot <- renderPlot({
          ggplot2::ggplot() +
            ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5,
                                          label = paste("Error:", e$message)),
                              size = 6, color = "red") +
            ggplot2::theme_void() +
            ggplot2::xlim(0, 1) +
            ggplot2::ylim(0, 1)
        })
      })
    })

    # Return the plot type reactive for use in other modules
    return(current_plot_type)
  })
}
