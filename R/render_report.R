#' Render Report Based on Format Type
#'
#' A utility function that renders either R Markdown or Quarto reports
#' based on the specified format.
#'
#' @param format Character string: "rmarkdown" or "quarto"
#' @param output_file Path to the output file
#' @param params List of parameters to pass to the report
#' @param template_path Optional custom template path
#'
#' @return Path to the rendered file
#'
#' @keywords internal
#'
render_report <- function(format = c("rmarkdown", "quarto"),
                          output_file,
                          params = list(),
                          template_path = NULL) {
  format <- match.arg(format)

  logr_msg(paste("Starting", format, "report rendering"),
    level = "INFO")

  logr_msg(paste("Parameters:", paste(names(params), collapse = ", ")),
    level = "DEBUG")

  tryCatch({
      # Get appropriate template
      if (is.null(template_path)) {
        template_path <- get_template_path(format)
      }

      logr_msg(paste("Using template:", template_path), level = "DEBUG")

      # Validate template exists
      if (!file.exists(template_path)) {
        logr_msg("Template not found, creating fallback", level = "WARN")
        template_path <- create_fallback_template(format)
      }

      # Validate and clean parameters
      params <- validate_and_clean_params(params)

      # Render based on format
      if (format == "rmarkdown") {
        render_rmarkdown_report(template_path, output_file, params)
      } else {
        render_quarto_report(template_path, output_file, params)
      }

      logr_msg(paste("Successfully rendered", format, "report"), level = "SUCCESS")
      return(output_file)
    },
    error = function(e) {
      logr_msg(paste("Error rendering", format, "report:", e$message), level = "ERROR")

      # Create error report as fallback
      create_error_report(output_file, e$message, params$dataset_title %||% "Unknown", format)
      return(output_file)
    })
}

#' Enhanced Quarto availability check
#' @return Logical indicating if Quarto is available
#' @keywords internal
quarto_available <- function() {

  # Method 1: Check for quarto R package
  if (requireNamespace("quarto", quietly = TRUE)) {
    tryCatch({
      quarto::quarto_version()
      logr_msg("Quarto R package available", level = "DEBUG")
      return(TRUE)
    }, error = function(e) {
      logr_msg("Quarto R package found but not functional", level = "DEBUG")
    })
  }

  # Method 2: Check for system quarto
  tryCatch({
    result <- system2("quarto", args = "--version",
                     stdout = TRUE, stderr = TRUE, timeout = 5)
    if (!is.null(result) && length(result) > 0) {
      logr_msg(paste("System Quarto version:", result[1]), level = "DEBUG")
      return(TRUE)
    }
  }, error = function(e) {
    logr_msg("System Quarto not available", level = "DEBUG")
  })

  # Method 3: Check common installation paths
  quarto_paths <- c(
    "/usr/local/bin/quarto",
    "/usr/bin/quarto",
    "C:/Program Files/Quarto/bin/quarto.exe",
    "C:/Users/*/AppData/Local/Programs/Quarto/bin/quarto.exe"
  )

  for (path in quarto_paths) {
    if (file.exists(path)) {
      logr_msg(paste("Found Quarto at:", path), level = "DEBUG")
      return(TRUE)
    }
  }

  logr_msg("Quarto not available", level = "DEBUG")
  return(FALSE)
}


#' Alternative Quarto Rendering using rmarkdown
#' @keywords internal
render_quarto_alternative <- function(template_path, output_file, params) {

  logr_msg("Using alternative Quarto rendering method", level = "INFO")

  # Create a modified template for rmarkdown compatibility
  temp_rmd <- tempfile(fileext = ".Rmd")

  # Read the Quarto template and convert to R Markdown
  qmd_content <- readLines(template_path)

  # Convert Quarto-specific syntax to R Markdown
  rmd_content <- convert_qmd_to_rmd(qmd_content)

  # Write modified content
  writeLines(rmd_content, temp_rmd)

  # Render with rmarkdown
  rmarkdown::render(
    input = temp_rmd,
    output_file = output_file,
    params = params,
    envir = new.env(),
    quiet = TRUE
  )

  # Clean up
  unlink(temp_rmd)
}

#' Render Quarto Report with Enhanced Error Handling
#'
#' @param template_path Path to qmd template
#' @param output_file Output file path
#' @param params Report parameters
#'
#' @keywords internal
render_quarto_report <- function(template_path, output_file, params) {

  logr_msg("Rendering Quarto report", level = "DEBUG")
  logr_msg(paste("Template:", template_path), level = "DEBUG")
  logr_msg(paste("Output:", output_file), level = "DEBUG")

  # Check if quarto is available
  if (!quarto_available()) {
    stop("Quarto is not available. Please install Quarto or use R Markdown format.")
  }

  # Validate parameters for Quarto
  params <- validate_quarto_params(params)

  tryCatch({
    # Method 1: Try using the quarto R package
    if (requireNamespace("quarto", quietly = TRUE)) {
      logr_msg("Using quarto R package", level = "DEBUG")

      # Create temporary directory for rendering
      temp_dir <- tempdir()
      temp_qmd <- file.path(temp_dir, "temp_report.qmd")

      # Copy template to temp location
      file.copy(template_path, temp_qmd, overwrite = TRUE)

      # Create params file
      params_file <- file.path(temp_dir, "params.yml")
      yaml_content <- yaml::as.yaml(list(params = params))
      writeLines(yaml_content, params_file)

      # Render with quarto package
      quarto::quarto_render(
        input = temp_qmd,
        output_file = basename(output_file),
        # output_dir = dirname(output_file),
        execute_params = params,
        quiet = TRUE
      )

      # Clean up
      unlink(c(temp_qmd, params_file))

    } else {
      # Method 2: System call fallback
      render_quarto_system_call(template_path, output_file, params)
    }

    logr_msg("Quarto rendering completed successfully", level = "SUCCESS")

  }, error = function(e) {
    logr_msg(paste("Quarto rendering error:", e$message), level = "ERROR")

    # Try fallback method
    if (grepl("quarto", e$message, ignore.case = TRUE)) {
      logr_msg("Trying alternative rendering method", level = "INFO")
      render_quarto_alternative(template_path, output_file, params)
    } else {
      stop(e)
    }
  })
}


#' System call method for Quarto rendering
#'
#' @param template_path path to template
#' @param output_file report output
#'
#' @keywords internal
#'
render_quarto_system_call <- function(template_path, output_file, params) {

  logr_msg("Using system call for Quarto rendering", level = "DEBUG")

  # Create temporary parameter file
  temp_dir <- tempdir()
  param_file <- file.path(temp_dir, "params.yml")

  # Write parameters as YAML
  param_yaml <- c(
    "params:",
    paste0("  dataset_title: \"", params$dataset_title, "\""),
    paste0("  title: \"", params$title, "\""),
    paste0("  plot_type: \"", params$plot_type, "\"")
  )

  writeLines(param_yaml, param_file)

  # Build quarto command
  cmd_args <- c(
    "render",
    template_path,
    "--output", output_file,
    "--execute-params", param_file
  )

  # Execute system call
  result <- system2("quarto", args = cmd_args,
                   stdout = TRUE, stderr = TRUE)

  # Check for errors
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    stop(paste("Quarto system call failed:", paste(result, collapse = "\n")))
  }

  # Clean up
  unlink(param_file)
}

#' Validate Quarto Parameters
#'
#' @param params Parameter list
#'
#' @return Validated parameter list
#'
#' @keywords internal
validate_quarto_params <- function(params) {

  logr_msg("Validating Quarto parameters", level = "DEBUG")

  # Ensure all required parameters exist with proper types
  validated_params <- list(
    dataset_title = as.character(params$dataset_title %||% "TidyTuesday Dataset"),
    title = as.character(params$title %||% "TidyTuesday Report"),
    data_list = if (is.list(params$data_list)) params$data_list else list(),
    plots = params$plots,  # Can be NULL
    plot_type = as.character(params$plot_type %||% "type")
  )

  # Clean dataset title for Quarto
  validated_params$dataset_title <- gsub('["\']', '', validated_params$dataset_title)
  validated_params$title <- gsub('["\']', '', validated_params$title)

  logr_msg(paste("Validated dataset_title:", validated_params$dataset_title), level = "DEBUG")
  logr_msg(paste("Validated data_list length:", length(validated_params$data_list)), level = "DEBUG")

  return(validated_params)
}

#' Validate and Clean Report Parameters
#'
#' @param params List of parameters
#'
#' @return Cleaned and validated parameter list
#'
#' @keywords internal
#'
validate_and_clean_params <- function(params) {

  logr_msg("Validating and cleaning report parameters", level = "DEBUG")

  # Ensure required parameters exist
  default_params <- list(
    dataset_title = "TidyTuesday Dataset",
    title = "TidyTuesday Data Report",
    data_list = list(),
    plots = NULL,
    plot_type = "type"
  )

  # Merge with defaults
  for (param_name in names(default_params)) {
    if (is.null(params[[param_name]])) {
      params[[param_name]] <- default_params[[param_name]]
      logr_msg(paste("Using default for parameter:", param_name), level = "DEBUG")
    }
  }

  # Validate data_list
  if (!is.list(params$data_list)) {
    logr_msg("data_list is not a list, converting", level = "WARN")
    params$data_list <- list(params$data_list)
  }

  # Ensure data_list has proper names
  if (length(params$data_list) > 0 && is.null(names(params$data_list))) {
    logr_msg("Adding default names to data_list", level = "DEBUG")
    names(params$data_list) <- paste0("dataset_", seq_along(params$data_list))
  }

  # Clean dataset title
  if (is.character(params$dataset_title) && length(params$dataset_title) > 0) {
    # Remove any problematic characters for the title
    params$dataset_title <- gsub("[\"'`]", "", params$dataset_title)
  }

  # Log final parameters
  logr_msg(paste("Final dataset_title:", params$dataset_title), level = "DEBUG")
  logr_msg(paste("Number of datasets:", length(params$data_list)), level = "DEBUG")
  if (length(params$data_list) > 0) {
    logr_msg(paste("Dataset names:", paste(names(params$data_list), collapse = ", ")), level = "DEBUG")
  }

  return(params)
}


#' Render R Markdown Report
#'
#' @param template_path Path to Rmd template
#' @param output_file Output file path
#' @param params Report parameters
#'
#' @keywords internal
#'
render_rmarkdown_report <- function(template_path, output_file, params) {

  logr_msg("Rendering R Markdown report", level = "DEBUG")
  logr_msg(paste("Output file:", output_file), level = "DEBUG")

  # Create a clean environment for rendering
  render_env <- new.env(parent = globalenv())

  # Add required libraries to environment
  render_env$library <- library
  render_env$require <- require

  # Log parameter details before rendering
  logr_msg(paste("Rendering with dataset_title:", params$dataset_title), level = "DEBUG")
  logr_msg(paste("Rendering with title:", params$title), level = "DEBUG")

  rmarkdown::render(
    input = template_path,
    output_file = output_file,
    params = params,
    envir = render_env,
    quiet = FALSE,  # Change to FALSE for debugging
    clean = TRUE
  )
}


#' Create Enhanced Error Report
#'
#' @param file Output file path
#' @param error_msg Error message
#' @param dataset_title Dataset title
#' @param format Report format
#'
#' @keywords internal
#'
create_error_report <- function(file, error_msg, dataset_title = "Unknown", format = "rmarkdown") {

  logr_msg(paste("Creating error report for", format), level = "INFO")

  error_html <- paste0(
    '<!DOCTYPE html>
    <html>
    <head>
      <title>Report Generation Error</title>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
      <style>
        body { margin: 40px; }
        .error-container { max-width: 800px; margin: 0 auto; }
      </style>
    </head>
    <body>
      <div class="error-container">
        <h1 class="text-danger"><i class="bi bi-exclamation-triangle"></i> Report Generation Failed</h1>

        <div class="alert alert-danger" role="alert">
          <h4 class="alert-heading">Error Details</h4>
          <p class="mb-0">', error_msg, '</p>
        </div>

        <div class="card mt-4">
          <div class="card-header">
            <h5 class="mb-0">Report Information</h5>
          </div>
          <div class="card-body">
            <dl class="row">
              <dt class="col-sm-3">Dataset:</dt>
              <dd class="col-sm-9">', dataset_title, '</dd>

              <dt class="col-sm-3">Format:</dt>
              <dd class="col-sm-9">', tools::toTitleCase(format), '</dd>

              <dt class="col-sm-3">Timestamp:</dt>
              <dd class="col-sm-9">', Sys.time(), '</dd>
            </dl>
          </div>
        </div>

        <div class="alert alert-info mt-4" role="alert">
          <h6 class="alert-heading">Troubleshooting Tips</h6>
          <ul class="mb-0">
            <li>Try selecting a different dataset</li>
            <li>Try a different report format</li>
            <li>Check your internet connection</li>
            <li>Contact support if the problem persists</li>
          </ul>
        </div>
      </div>
    </body>
    </html>'
  )

  writeLines(error_html, file)
}

#' Convert Quarto syntax to R Markdown
#'
#' @keywords internal
#'
convert_qmd_to_rmd <- function(qmd_lines) {

  # Basic conversions for compatibility
  rmd_lines <- qmd_lines

  # Convert Quarto chunk options to R Markdown
  rmd_lines <- gsub("#\\| ", "", rmd_lines)
  rmd_lines <- gsub("output: asis", "results='asis'", rmd_lines)
  rmd_lines <- gsub("include: false", "include=FALSE", rmd_lines)

  # Convert Quarto callouts to simple formatting
  rmd_lines <- gsub("::: \\{.callout-note\\}", "<div class='alert alert-info'>", rmd_lines)
  rmd_lines <- gsub("::: \\{.callout-tip\\}", "<div class='alert alert-success'>", rmd_lines)
  rmd_lines <- gsub(":::", "</div>", rmd_lines)

  # Update YAML header for R Markdown
  yaml_start <- which(rmd_lines == "---")[1]
  yaml_end <- which(rmd_lines == "---")[2]

  if (!is.na(yaml_start) && !is.na(yaml_end)) {
    new_yaml <- c(
      "---",
      "title: \"TidyTuesday Report\"",
      "output:",
      "  html_document:",
      "    theme: cosmo",
      "    toc: true",
      "    code_folding: hide",
      "params:",
      "  dataset_title: \"Dataset\"",
      "  title: \"TidyTuesday Report\"",
      "  data_list: NULL",
      "  plots: NULL",
      "  plot_type: \"type\"",
      "---"
    )

    rmd_lines <- c(new_yaml, rmd_lines[(yaml_end + 1):length(rmd_lines)])
  }

  return(rmd_lines)
}

#' Create Fallback Template
#'
#' @param format Report format
#'
#' @return Path to created template
#'
#' @keywords internal
#'
create_fallback_template <- function(format = c("rmarkdown", "quarto")) {

  format <- match.arg(format)
  logr_msg(paste("Creating fallback", format, "template"), level = "INFO")

  if (format == "rmarkdown") {
    return(create_fallback_rmd_template())
  } else {
    return(create_fallback_qmd_template())
  }
}


#' Create Fallback R Markdown Template
#'
#' @keywords internal
#'
create_fallback_rmd_template <- function() {

  template_path <- tempfile(fileext = ".Rmd")

  template_content <- '---
title: "`r params$title`"
output:
  html_document:
    theme: bootstrap
    toc: true
params:
  dataset_title: "Dataset"
  title: "TidyTuesday Report"
  data_list: NULL
  plots: NULL
  plot_type: "type"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(gt)
library(dplyr)
```

# `r params$dataset_title`

This is a basic R Markdown report for the TidyTuesday dataset: **`r params$dataset_title`**

## Data Overview

```{r data-info, results="asis"}
if (!is.null(params$data_list) && length(params$data_list) > 0) {
  first_df <- params$data_list[[1]]
  dataset_names <- names(params$data_list)

  cat("**Number of datasets:**", length(params$data_list), "\\n\\n")
  cat("**Dataset names:**", paste(dataset_names, collapse = ", "), "\\n\\n")
  cat("**Main dataset dimensions:**", nrow(first_df), "rows ×", ncol(first_df), "columns\\n\\n")

  # Show basic summary
  knitr::kable(head(first_df, 5), caption = "Data Preview")
} else {
  cat("No data available for preview.\\n")
}
```

## Visualization

```{r plots, fig.width=10, fig.height=6}
if (!is.null(params$plots) && inherits(params$plots, "gg")) {
  print(params$plots)
} else {
  cat("No visualization available.")
}
```
'

  writeLines(template_content, template_path)
  return(template_path)
}

#' Create Fallback Quarto Template
#'
#' @keywords internal
#'
create_fallback_qmd_template <- function() {

  template_path <- tempfile(fileext = ".qmd")

  template_content <- '---
title: "{{< meta params.title >}}"
format: html
params:
  dataset_title: "Dataset"
  title: "TidyTuesday Report"
  data_list: NULL
  plots: NULL
  plot_type: "type"
execute:
  echo: false
  warning: false
  message: false
---

```{r setup}
library(ggplot2)
library(gt)
library(dplyr)
```

# {{< meta params.dataset_title >}}

This is a basic Quarto report for the TidyTuesday dataset.

```{r data-info}
#| results: asis
if (!is.null(params$data_list) && length(params$data_list) > 0) {
  first_df <- params$data_list[[1]]
  cat("Dataset dimensions:", nrow(first_df), "rows ×", ncol(first_df), "columns\\n")

  # Show basic summary
  knitr::kable(head(first_df, 5), caption = "Data Preview")
}
```

```{r plots}
if (!is.null(params$plots) && inherits(params$plots, "gg")) {
  print(params$plots)
}
```
'

  writeLines(template_content, template_path)
  return(template_path)
}

#' Get Template Path for Format
#'
#' @param format Report format ("rmarkdown" or "quarto")
#'
#' @return Path to template file
#'
#' @keywords internal
#'
get_template_path <- function(format) {

  template_file <- switch(format,
    "rmarkdown" = "report_template.Rmd",
    "quarto" = "report_template.qmd"
  )

  template_dir <- switch(format,
    "rmarkdown" = "rmarkdown",
    "quarto" = "quarto"
  )

  system.file(template_dir, template_file, package = "shrapports")
}
