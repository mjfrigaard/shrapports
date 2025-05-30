#' Report UI Module
#'
#' @param id Namespace ID
#'
#' @return UI elements for report generation
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
          tags$p("Generate a comprehensive HTML report containing visualizations and data preview.",
            class = "text-muted mb-3"
          ),
          tags$div(
            class = "d-grid",
            downloadButton(
              ns("download_report"),
              "Download Report",
              class = "btn btn-primary",
              icon = icon("file")
            )
          ),
          tags$hr(),
          tags$small(
            class = "text-muted",
            "Report includes: data visualization, summary statistics, data preview, and variable information."
          )
        )
      )
    },
    error = function(e) {
      logr_msg(paste("Error creating report UI:", e$message), level = "ERROR")
      bslib::card(
        bslib::card_header("Report Error"),
        bslib::card_body(
          h4("Error loading report interface", class = "text-danger"),
          p("Please refresh the page.")
        )
      )
    }
  )
}

#' Report Server Module
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

    output$download_report <- downloadHandler(
      filename = function() {
        tryCatch({
            title <- if (!is.null(dataset_title()) && dataset_title() != "") {
              gsub("[^A-Za-z0-9_-]", "_", dataset_title())
            } else {
              "tidytuesday_report"
            }

            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            filename <- paste0(title, "_", timestamp, ".html")

            logr_msg(paste("Generated report filename:", filename), level = "DEBUG")
            return(filename)
          },
          error = function(e) {
            logr_msg(paste("Error generating filename:", e$message), level = "ERROR")
            return("tidytuesday_report.html")
          })
      },
      content = function(file) {
        tryCatch({
            logr_msg("Starting report generation", level = "INFO")

            # validate inputs
            if (is.null(data()) || length(data()) == 0) {
              logr_msg("No data available for report generation", level = "ERROR")
              stop("No data available. Please select a dataset first.")
            }

            # progress notification
            showNotification(
              "Generating report... This may take a few moments.",
              type = "message",
              duration = 5
            )

            # current plot type and data
            current_plot_type <- if (!is.null(selected_plot_type())) {
              selected_plot_type()
            } else {
              "type"
            }

            current_dataset_title <- if (!is.null(dataset_title())) {
              dataset_title()
            } else {
              "TidyTuesday Dataset"
            }

            logr_msg(paste(
              "Report parameters - Dataset:", current_dataset_title,
              "Plot type:", current_plot_type
            ), level = "INFO")

            # make report plots
            plots <- tryCatch({
                shrapports::inspect_plot(ttd = data(),
                  plot = current_plot_type)
              },
              error = function(e) {
                logr_msg(paste("Error generating plots for report:", e$message),
                  level = "WARN")
                NULL
              })

            # find template
            template_path <- system.file("rmd", "report_template.Rmd",
              package = "shrapports"
            )

            if (template_path == "" || !file.exists(template_path)) {
              logr_msg("Template file not found, using fallback", level = "WARN")

              # Create a temporary template if the package template is missing
              template_path <- tempfile(fileext = ".Rmd")
              create_fallback_template(template_path)
            }

            logr_msg(paste("Using template:", template_path), level = "DEBUG")

            # Prepare params for report
            params <- list(
              dataset_title = current_dataset_title,
              title = paste("TidyTuesday Report:", current_dataset_title),
              data_list = data(),
              plots = plots,
              plot_type = current_plot_type
            )

            logr_msg("Rendering R Markdown report", level = "INFO")

            # Render the report
            rmarkdown::render(
              input = template_path,
              output_file = file,
              params = params,
              envir = new.env(),
              quiet = TRUE
            )

            logr_msg("Report generation completed successfully", level = "SUCCESS")

            # Show success notification
            showNotification(
              "Report generated successfully!",
              type = "message",
              duration = 3
            )
          },
          error = function(e) {
            error_msg <- paste("Failed to generate report:", e$message)
            logr_msg(error_msg, level = "ERROR")

            # Show error notification
            showNotification(
              paste("Report generation failed:", e$message),
              type = "error",
              duration = 10
            )

            # Create a simple error HTML file
            create_error_report(file, error_msg, current_dataset_title)
          })
      })
  })
}
