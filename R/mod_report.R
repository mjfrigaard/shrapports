#' Report UI Module with Format Selection
#'
#' @param id Namespace ID
#'
#' @return UI elements for report generation with format options
#' @export
mod_report_ui <- function(id) {
  ns <- NS(id)

  logr_msg("Initializing report UI module", level = "DEBUG")

  tryCatch({
    bslib::card(
      bslib::card_header(
        tags$div(
          class = "d-flex justify-content-between align-items-center",
          tags$h5("Download Report", class = "mb-0"),
          tags$i(class = "bi bi-file-earmark-text", style = "font-size: 1.2rem;")
        )
      ),
      bslib::card_body(
        tags$p(
          "Generate a comprehensive report containing visualizations and data preview.",
          class = "text-muted mb-3"
        ),

        # Format selection
        selectInput(
          ns("report_format"),
          "Report Format:",
          choices = list(
            "R Markdown" = "rmarkdown",
            "Quarto" = "quarto"
          ),
          selected = "rmarkdown",
          width = "100%"
        ),

        # Format description
        uiOutput(ns("format_description")),

        tags$hr(),

        # Download button
        tags$div(
          class = "d-grid",
          downloadButton(
            ns("download_report"),
            "Download Report",
            class = "btn btn-primary",
            icon = icon("download")
          )
        ),

        tags$hr(),

        # Help text
        tags$small(
          class = "text-muted",
          "Report includes: data visualization, summary statistics, data preview, and variable information."
        )
      )
    )
  }, error = function(e) {
    logr_msg(paste("Error creating report UI:", e$message), level = "ERROR")

    bslib::card(
      bslib::card_header("Report Error"),
      bslib::card_body(
        h4("Error loading report interface", class = "text-danger"),
        p("Please refresh the page.")
      )
    )
  })
}

#' Report Server Module with Enhanced Quarto Error Handling
#'
#' @param id Module ID
#' @param data A reactive expression returning the dataset list
#' @param selected_plot_type A reactive expression returning the selected plot type
#' @param dataset_title A reactive expression returning the dataset title
#'
#' @export
mod_report_server <- function(id, data, selected_plot_type, dataset_title) {
  moduleServer(id, function(input, output, session) {

    logr_msg("Initializing report server module", level = "DEBUG")

    # Format description reactive
    output$format_description <- renderUI({
      req(input$report_format)

      # Check Quarto availability for better user guidance
      quarto_status <- if (input$report_format == "quarto") {
        if (quarto_available()) {
          list(available = TRUE, message = "Quarto is available and ready to use.")
        } else {
          list(available = FALSE, message = "Quarto not detected. Will fall back to R Markdown if selected.")
        }
      } else {
        list(available = TRUE, message = "")
      }

      description <- switch(input$report_format,
        "rmarkdown" = list(
          icon = "bi-file-earmark-code",
          title = "R Markdown",
          desc = "Traditional R Markdown report with Bootstrap theme. Compatible with all R installations.",
          note = "Recommended for maximum compatibility."
        ),
        "quarto" = list(
          icon = "bi-file-earmark-richtext",
          title = "Quarto",
          desc = "Modern scientific publishing format with enhanced features and interactivity.",
          note = quarto_status$message
        )
      )

      alert_class <- if (input$report_format == "quarto" && !quarto_status$available) {
        "alert-warning"
      } else {
        "alert-light"
      }

      tags$div(
        class = paste("alert border-start border-primary border-3 py-2", alert_class),
        tags$div(
          class = "d-flex align-items-center mb-1",
          tags$i(class = paste("bi", description$icon, "me-2")),
          tags$strong(description$title)
        ),
        tags$p(description$desc, class = "mb-1 small"),
        tags$small(description$note, class = "text-muted")
      )
    })

    # Download handler with enhanced error handling
    output$download_report <- downloadHandler(
      filename = function() {
        tryCatch({
          # Get actual dataset title from the data
          current_data <- data()
          actual_title <- if (!is.null(current_data) && length(current_data) > 0) {
            # Try to get from variable input module or use first dataset name
            first_dataset_name <- names(current_data)[1]
            if (!is.null(first_dataset_name) && first_dataset_name != "") {
              gsub("\\.csv$", "", first_dataset_name)  # Remove .csv extension
            } else {
              "tidytuesday_dataset"
            }
          } else {
            "tidytuesday_dataset"
          }

          # Clean the title for filename
          clean_title <- gsub("[^A-Za-z0-9_-]", "_", actual_title)

          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          format_suffix <- switch(input$report_format,
            "rmarkdown" = "rmd",
            "quarto" = "qto"
          )

          filename <- paste0(clean_title, "_", format_suffix, "_", timestamp, ".html")

          logr_msg(paste("Generated report filename:", filename), level = "DEBUG")
          return(filename)

        }, error = function(e) {
          logr_msg(paste("Error generating filename:", e$message), level = "ERROR")
          return("tidytuesday_report.html")
        })
      },

      content = function(file) {
        tryCatch({
          logr_msg(paste("Starting", input$report_format, "report generation"), level = "INFO")

          # Validate inputs
          current_data <- data()
          if (is.null(current_data) || length(current_data) == 0) {
            logr_msg("No data available for report generation", level = "ERROR")
            stop("No data available. Please select a dataset first.")
          }

          # Enhanced format checking for Quarto
          if (input$report_format == "quarto") {
            if (!quarto_available()) {
              logr_msg("Quarto not available, falling back to R Markdown", level = "WARN")
              showNotification(
                "Quarto not available. Generating R Markdown report instead.",
                type = "warning",
                duration = 5
              )
              actual_format <- "rmarkdown"
            } else {
              logr_msg("Quarto available, proceeding with Quarto rendering", level = "INFO")
              actual_format <- "quarto"
            }
          } else {
            actual_format <- input$report_format
          }

          # Show progress notification
          progress_msg <- paste("Generating", tools::toTitleCase(actual_format), "report...")
          showNotification(
            progress_msg,
            type = "message",
            duration = 8,
            id = "report_progress"
          )

          # Get current parameters with proper dataset title
          current_plot_type <- selected_plot_type() %||% "type"

          # Extract actual dataset title from data or use fallback
          actual_dataset_title <- if (!is.null(current_data) && length(current_data) > 0) {
            # Get first dataset name and clean it
            first_name <- names(current_data)[1]
            if (!is.null(first_name) && first_name != "") {
              # Remove .csv extension and format nicely
              clean_name <- gsub("\\.csv$", "", first_name)
              tools::toTitleCase(gsub("_", " ", clean_name))
            } else {
              "TidyTuesday Dataset"
            }
          } else {
            "TidyTuesday Dataset"
          }

          logr_msg(paste("Report parameters - Dataset:", actual_dataset_title,
                        "Plot type:", current_plot_type,
                        "Format:", actual_format), level = "INFO")

          # Generate plots for the report
          plots <- tryCatch({
            shrapports::inspect_plot(ttd = current_data, plot = current_plot_type)
          }, error = function(e) {
            logr_msg(paste("Error generating plots for report:", e$message), level = "WARN")
            NULL
          })

          # Prepare parameters for the report
          params <- list(
            dataset_title = actual_dataset_title,
            title = paste("TidyTuesday Report:", actual_dataset_title),
            data_list = current_data,
            plots = plots,
            plot_type = current_plot_type
          )

          # Log parameter summary
          logr_msg("Parameters prepared:", level = "DEBUG")
          logr_msg(paste("  - dataset_title:", params$dataset_title), level = "DEBUG")
          logr_msg(paste("  - data_list length:", length(params$data_list)), level = "DEBUG")
          logr_msg(paste("  - plots class:", class(params$plots)), level = "DEBUG")

          # Render the report with enhanced error handling
          tryCatch({
            render_report(
              format = actual_format,
              output_file = file,
              params = params
            )
          }, error = function(render_error) {
            logr_msg(paste("Render error:", render_error$message), level = "ERROR")

            # If Quarto fails, try R Markdown as ultimate fallback
            if (actual_format == "quarto") {
              logr_msg("Quarto failed, trying R Markdown fallback", level = "WARN")
              render_report(
                format = "rmarkdown",
                output_file = file,
                params = params
              )
              showNotification(
                "Quarto failed, generated R Markdown report instead.",
                type = "warning",
                duration = 5
              )
            } else {
              stop(render_error)
            }
          })

          logr_msg("Report generation completed successfully", level = "SUCCESS")

          # Remove progress notification and show success
          removeNotification("report_progress")
          showNotification(
            paste(tools::toTitleCase(actual_format), "report generated successfully!"),
            type = "message",
            duration = 3
          )

        }, error = function(e) {
          error_msg <- paste("Failed to generate report:", e$message)
          logr_msg(error_msg, level = "ERROR")

          # Remove progress notification
          removeNotification("report_progress")

          # Show error notification
          showNotification(
            paste("Report generation failed:", e$message),
            type = "error",
            duration = 10
          )

          # Create error report
          create_error_report(file, error_msg,
                            actual_dataset_title %||% "Unknown",
                            input$report_format)
        })
      }
    )
  })
}
