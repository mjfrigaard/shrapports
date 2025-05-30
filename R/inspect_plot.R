#' Generate an inspectdf Visualization from TidyTuesday Data
#'
#' Based on a `ttd` object returned by `get_tt_data()`, this function determines
#' how to apply `inspectdf` plotting functions depending on the number and types
#' of datasets returned.
#'
#' @param ttd A list returned by `get_tt_data()`.
#' @param plot A string specifying which plot to produce:
#'   `"type"`, `"na"`, `"cor"`, `"num"`, or `"cat"`.
#'
#' @return A `ggplot2` plot, or a list of plots (for categorical and numeric
#'     checks).
#'
#' @examples
#' ttd <- get_tt_data("Netflix Titles")
#' inspect_plot(ttd, plot = "type")
#'
#' ttd2 <- get_tt_data("Space Launches")
#' inspect_plot(ttd2, plot = "num")
#'
#' @export
#'
inspect_plot <- function(ttd, plot = c("type", "na", "cor", "num", "cat")) {
  plot <- match.arg(plot)
  stopifnot(is.list(ttd), length(ttd) >= 1)

  ttd_nms <- names(ttd)
  ttd <- setNames(ttd, ttd_nms)

  n_datasets <- length(ttd_nms)

  get_plot_title <- function(prefix, names) {
    if (length(names) == 1) {
      paste(prefix, "for", names[1])
    } else {
      paste(prefix, "for", names[1], "&", names[2])
    }
  }

  # TYPE ----
  if (plot == "type") {
    logr_msg("Generating inspect_types() plot", level = "INFO")
    p <- inspectdf::inspect_types(
      df1 = ttd[[ttd_nms[1]]],
      df2 = if (n_datasets >= 2) ttd[[ttd_nms[2]]] else NULL
    ) |>
      inspectdf::show_plot(text_labels = TRUE) +
      ggplot2::labs(title = get_plot_title("Variable types", ttd_nms[1:min(2, n_datasets)]))
    return(p)
  }

  # NA ----
  if (plot == "na") {
    logr_msg("Generating inspect_na() plot", level = "INFO")
    p <- inspectdf::inspect_na(
      df1 = ttd[[ttd_nms[1]]],
      df2 = if (n_datasets >= 2) ttd[[ttd_nms[2]]] else NULL
    ) |>
      inspectdf::show_plot(text_labels = TRUE) +
      ggplot2::labs(title = get_plot_title("Missing data", ttd_nms[1:min(2, n_datasets)]))
    return(p)
  }

  # CORRELATION ----
  if (plot == "cor") {
    logr_msg("Generating inspect_cor() plot", level = "INFO")

    has_min_two_numeric <- function(df) {
      num_df <- dplyr::select(df, tidyselect::where(is.numeric))
      ncol(num_df) >= 2
    }

    ## more than 2 datasets ----
    if (n_datasets > 2) {
      logr_msg("More than two datasets - using first only", level = "INFO")
      df <- ttd[[ttd_nms[1]]]
      if (!has_min_two_numeric(df)) {
        logr_msg(paste("Dataset",
                 ttd_nms[1], "does not have at least 2 numeric columns for correlation"),
                 level = "ERROR")
        return(NULL)
      }
      return(
        inspectdf::inspect_cor(df1 = df) |>
          inspectdf::show_plot(text_labels = TRUE) +
          ggplot2::labs(title = paste("Correlation for", ttd_nms[1]))
      )
    }

    ## exactly 2 datasets ----
    if (n_datasets == 2) {
      plots <- list()
      for (name in ttd_nms) {
        df <- ttd[[name]]
        if (has_min_two_numeric(df)) {
          p <- inspectdf::inspect_cor(df) |>
            inspectdf::show_plot(text_labels = TRUE) +
            ggplot2::labs(title = paste("Correlation for", name))
          plots[[name]] <- p
        } else {
          logr_msg(paste("Dataset", name, "does not have at least 2 numeric columns"), level = "WARN")
        }
      }
      return(plots)
    }

    ## single dataset -----
    df <- ttd[[ttd_nms[1]]]
    if (has_min_two_numeric(df)) {
      return(
        inspectdf::inspect_cor(df1 = df) |>
          inspectdf::show_plot(text_labels = TRUE) +
          ggplot2::labs(title = paste("Correlation matrix for", ttd_nms[1]))
      )
    } else {
      logr_msg(paste("Dataset", ttd_nms[1], "does not have at least 2 numeric columns"), level = "ERROR")
      return(NULL)
    }
  }


  # CATEGORICAL ----
  if (plot == "cat") {
    logr_msg("Generating inspect_cat() plot", level = "INFO")
    # Try both datasets if same columns, otherwise show separately
    if (n_datasets == 2) {
      cols1 <- colnames(ttd[[ttd_nms[1]]])
      cols2 <- colnames(ttd[[ttd_nms[2]]])
      if (identical(sort(cols1), sort(cols2))) {
        logr_msg("Using both datasets in inspect_cat()", level = "DEBUG")
        return(
          inspectdf::inspect_cat(ttd[[ttd_nms[1]]], ttd[[ttd_nms[2]]]) |>
            inspectdf::show_plot(text_labels = TRUE) +
            ggplot2::labs(title = get_plot_title("Categorical columns", ttd_nms))
        )
      } else {
        logr_msg("Column mismatch, plotting separately", level = "DEBUG")
        plots <- lapply(ttd_nms, function(name) {
          inspectdf::inspect_cat(ttd[[name]]) |>
            inspectdf::show_plot(text_labels = TRUE) +
            ggplot2::labs(title = paste("Categorical columns in", name))
        })
        return(plots)
      }
    } else {
      return(
        inspectdf::inspect_cat(ttd[[ttd_nms[1]]]) |>
          inspectdf::show_plot(text_labels = TRUE) +
          ggplot2::labs(title = paste("Categorical columns in", ttd_nms[1]))
      )
    }
  }

  # NUMERIC ----
  if (plot == "num") {
    logr_msg("Generating inspect_num() plot", level = "INFO")

    has_numeric <- function(df) {
      any(purrr::map_lgl(df, is.numeric))
    }

    ## more than 2 datasets  ----
    if (n_datasets > 2) {
      logr_msg("More than two datasets - using first only", level = "INFO")
      if (!has_numeric(ttd[[ttd_nms[1]]])) {
        logr_msg(paste("No numeric columns in", ttd_nms[1]), level = "WARN")
        return(NULL)
      }
      return(
        inspectdf::inspect_num(ttd[[ttd_nms[1]]]) |>
          inspectdf::show_plot(text_labels = TRUE) +
          ggplot2::labs(title = paste("Numerical variables in", ttd_nms[1]))
      )
    }

    ## exactly 2 datasets  ----
    if (n_datasets == 2) {
      plots <- list()
      for (name in ttd_nms) {
        df <- ttd[[name]]
        if (has_numeric(df)) {
          p <- inspectdf::inspect_num(df) |>
            inspectdf::show_plot(text_labels = TRUE) +
            ggplot2::labs(title = paste("Numerical variables in", name))
          plots[[name]] <- p
        } else {
          logr_msg(paste("No numeric columns in", name), level = "WARN")
        }
      }
      return(plots)
    }

    ## single dataset  ----
    if (has_numeric(ttd[[ttd_nms[1]]])) {
      return(
        inspectdf::inspect_num(ttd[[ttd_nms[1]]]) |>
          inspectdf::show_plot(text_labels = TRUE) +
          ggplot2::labs(title = paste("Numerical variables in", ttd_nms[1]))
      )
    } else {
      logr_msg(paste("No numeric columns in", ttd_nms[1]), level = "WARN")
      return(NULL)
    }
  }

  logr_msg("Unknown plot type requested", level = "ERROR")
  return(NULL)
}
