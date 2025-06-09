#' Inspect and Plot TidyTuesday Data
#'
#' A comprehensive function that analyzes TidyTuesday datasets and generates
#' appropriate inspection plots based on data characteristics and number of datasets.
#'
#' @param ttd A list of data frames from [get_tt_data()]
#' @param plot Character vector specifying which plots to generate. Options include:
#'   - `"types"` - Column type distributions
#'   - `"mem"` - Memory usage analysis
#'   - `"na"` - Missing value analysis
#'   - `"cor"` - Correlation analysis (numeric columns)
#'   - `"imb"` - Feature imbalance (categorical columns)
#'   - `"num"` - Numeric column summaries
#'   - `"cat"` - Categorical column summaries
#'   - `"all"` - Generate all available plots (default)
#'
#' @return Plots are displayed; function is called for side effects
#'
#' @export
#'
#' @examples
#' ttd <- get_tt_data("Moore’s Law")
#'
#' # Generate all plots
#' inspect_plot(ttd)
#'
#' # Generate only specific plots
#' inspect_plot(ttd, plot = c("types", "mem"))
#'
#' # Generate single plot type
#' inspect_plot(ttd, plot = "cor")
#'
inspect_plot <- function(ttd, plot = "all") {

  # Validate plot argument
  valid_plots <- c("types", "mem", "na", "cor", "imb", "num", "cat", "all")
  if (!all(plot %in% valid_plots)) {
    invalid_plots <- plot[!plot %in% valid_plots]
    stop(paste("Invalid plot type(s):", paste(invalid_plots, collapse = ", "),
               "\nValid options are:", paste(valid_plots, collapse = ", ")))
  }

  # If "all" is specified, use all plot types
  if ("all" %in% plot) {
    plot <- c("types", "mem", "na", "cor", "imb", "num", "cat")
  }


  ## ttd_length() ----
  ### determine number of datasets in ttd list ----
  num_datasets <- ttd_length(ttd)
  logr_msg(paste("Number of datasets:", num_datasets), level = "INFO")

  ### determine name of the datasets in the ttd list ----
  dataset_names <- names(ttd)
  logr_msg(paste("Dataset names:", paste(dataset_names, collapse = ", ")), level = "INFO")

  ### check_col_types() -----
  #### determine column types and intersections -----
  num_cols <- check_col_types(ttd, type = "num")
  cat_cols <- check_col_types(ttd, type = "cat")
  log_cols <- check_col_types(ttd, type = "log")
  date_cols <- check_col_types(ttd, type = "date")
  list_cols <- check_col_types(ttd, type = "list")

  # Log column analysis using functional programming
  purrr::iwalk(num_cols, ~ {
    if (length(.x) > 0 && .x[1] != 0) {
      logr_msg(paste("Numeric columns in", .y, ":", length(.x)), level = "DEBUG")
    }
  })

  purrr::iwalk(cat_cols, ~ {
    if (length(.x) > 0 && .x[1] != 0) {
      logr_msg(paste("Categorical columns in", .y, ":", length(.x)), level = "DEBUG")
    }
  })

  ## \-- PLOT EXECUTION FUNS ----
  ### \--- TYPES ----
  execute_inspect_types <- function() {
    logr_msg("Starting inspect_types analysis",
      level = "INFO")

    ### 1 dataset ----
    if (num_datasets == 1) {
      logr_msg("Running inspect_types for single dataset",
        level = "DEBUG")

      print(inspectdf::inspect_types(
            df1 = ttd[[1]],
            df2 = NULL,
            compare_index = FALSE) |>
            inspectdf::show_plot(text_labels = TRUE))

    ### 2 datasets ----
    } else if (num_datasets == 2) {
    #### get_intersecting_cols() ----
      intersecting_columns <- get_intersecting_cols(ttd)

      if (length(intersecting_columns) > 0) {
        logr_msg(paste("Found intersecting columns:",
          paste(intersecting_columns, collapse = ", ")),
          level = "DEBUG")
        print(
          inspectdf::inspect_types(df1 = ttd[[1]][intersecting_columns],
                                   df2 = ttd[[2]][intersecting_columns],
                                   compare_index = FALSE) |>
              inspectdf::show_plot(text_labels = TRUE)
          )
      } else {
        logr_msg("No intersecting columns found, analyzing separately", level = "DEBUG")

        purrr::walk(seq_along(ttd)[1:2], ~ {
          print(
            inspectdf::inspect_types(
            df1 = ttd[[.x]],
            df2 = NULL,
            compare_index = FALSE) |>
                inspectdf::show_plot(text_labels = TRUE)
            )
        })
      }

    ### >= 3 datasets ----
    } else if (num_datasets >= 3) {
      logr_msg("Running inspect_types for multiple datasets", level = "DEBUG")
      purrr::iwalk(ttd, ~ {
        if (is.data.frame(.x)) {
          logr_msg(paste("Analyzing dataset", .y), level = "DEBUG")
          print(
            inspectdf::inspect_types(
              df1 = .x,
              df2 = NULL,
              compare_index = FALSE) |>
              inspectdf::show_plot(text_labels = TRUE)
            )
        }
      })
    }
  }

  ### \--- MEMORY --------------------------------------------------------
  execute_inspect_mem <- function() {

    logr_msg("Starting inspect_mem analysis", level = "INFO")

    #### 1 dataset --------------------------------------------------------
    if (num_datasets == 1) {
      logr_msg("Running inspect_mem for single dataset", level = "DEBUG")
      print(
        inspectdf::inspect_mem(
          df1 = ttd[[1]],
          df2 = NULL) |>
            inspectdf::show_plot(text_labels = TRUE)
        )
    #### 2 datasets --------------------------------------------------------
    } else if (num_datasets == 2) {
      logr_msg("Running inspect_mem for two datasets", level = "DEBUG")
      print(
        inspectdf::inspect_mem(
          df1 = ttd[[1]],
          df2 = ttd[[2]]) |>
            inspectdf::show_plot(text_labels = TRUE)
        )
    #### >= 3 datasets ----
    } else if (num_datasets >= 3) {
      logr_msg("Running inspect_mem for multiple datasets", level = "DEBUG")
      purrr::iwalk(ttd, ~ {
        if (is.data.frame(.x)) {
          logr_msg(paste("Analyzing memory for dataset", .y), level = "DEBUG")
          print(
            inspectdf::inspect_mem(
              df1 = .x,
              df2 = NULL) |>
                inspectdf::show_plot(text_labels = TRUE))
        }
      })
    }
  }

  ### \--- MISSING ----
  execute_inspect_na <- function() {

    logr_msg("Starting inspect_na analysis", level = "INFO")

    #### 1 dataset ----
    if (num_datasets == 1) {

      logr_msg("Running inspect_na for single dataset", level = "DEBUG")

      print(inspectdf::inspect_na(
          df1 = ttd[[1]],
          df2 = NULL) |>
            inspectdf::show_plot(text_labels = TRUE))

      #### 2 datasets ----------
    } else if (num_datasets == 2) {
      logr_msg("Running inspect_na for two datasets", level = "DEBUG")
      print(
        inspectdf::inspect_na(
        df1 = ttd[[1]],
        df2 = ttd[[2]]) |>
            inspectdf::show_plot(text_labels = TRUE))

      #### >= 3 datasets ------------
    } else if (num_datasets >= 3) {
      logr_msg("Running inspect_na for multiple datasets", level = "DEBUG")
      purrr::iwalk(ttd, ~ {
        if (is.data.frame(.x)) {
          logr_msg(paste("Analyzing missing values for dataset", .y), level = "DEBUG")
          print(
            inspectdf::inspect_na(
                  df1 = .x,
                  df2 = NULL) |>
            inspectdf::show_plot(text_labels = TRUE))
        }
      })
    }
  }

  ### \--- CORRELATION ----
  execute_inspect_cor <- function() {
    logr_msg("Starting inspect_cor analysis", level = "INFO")

    ### 1 dataset ----
    if (num_datasets == 1) {
      ### has_min_cols() ----
      if (has_min_cols(num_cols, 1, min_count = 2)) {
        logr_msg("Running inspect_cor for single dataset with sufficient numeric columns",
          level = "DEBUG")
        print(inspectdf::inspect_cor(
          df1 = ttd[[1]],
          df2 = NULL,
          method = "pearson",
          with_col = NULL,
          alpha = 0.05) |>
              inspectdf::show_plot(text_labels = TRUE))
      } else {
        logr_msg("Insufficient numeric columns for correlation analysis", level = "WARN")
      }

      ### 2 datasets ----
    } else if (num_datasets == 2) {
      ####has_min_cols() ----
      if (has_min_cols(num_cols, 1) && has_min_cols(num_cols, 2)) {
        logr_msg("Running inspect_cor for two datasets with numeric columns", level = "DEBUG")
        print(
          inspectdf::inspect_cor(
              df1 = ttd[[1]],
              df2 = ttd[[2]],
              method = "pearson",
              with_col = NULL,
              alpha = 0.05) |>
              inspectdf::show_plot(text_labels = TRUE)
          )
      } else {
        logr_msg("One or both datasets lack sufficient numeric columns for correlation",
          level = "WARN")
      }

      ### >= 3 datasets ----
    } else if (num_datasets >= 3) {
      logr_msg("Running inspect_cor for multiple datasets", level = "DEBUG")
      purrr::iwalk(ttd, ~ {
        idx <- which(names(ttd) == .y)
        #### has_min_cols() ----
        if (is.data.frame(.x) && has_min_cols(num_cols, idx, min_count = 2)) {
          logr_msg(paste("Analyzing correlations for dataset", .y), level = "DEBUG")
          print(
            inspectdf::inspect_cor(
              df1 = .x,
              df2 = NULL,
              method = "pearson",
              with_col = NULL,
              alpha = 0.05) |>
              inspectdf::show_plot(text_labels = TRUE))
        }
      })
    }
  }

  ### \--- IMBALANCES ----
  execute_inspect_imb <- function() {
    logr_msg("Starting inspect_imb analysis", level = "INFO")

    ### 1 dataset ----
    if (num_datasets == 1) {
      logr_msg("Running inspect_imb for single dataset", level = "DEBUG")
      print(
        inspectdf::inspect_imb(
            df1 = ttd[[1]],
            df2 = NULL,
            include_na = FALSE) |>
            inspectdf::show_plot(text_labels = TRUE)
        )

      ### 2 datasets ----
    } else if (num_datasets == 2) {
      logr_msg("Running inspect_imb for two datasets", level = "DEBUG")
      print(
        inspectdf::inspect_imb(
                df1 = ttd[[1]],
                df2 = ttd[[2]],
                include_na = FALSE) |>
            inspectdf::show_plot(text_labels = TRUE)
        )

      #### >= 3 datasets ----
    } else if (num_datasets >= 3) {
      logr_msg("Running inspect_imb for multiple datasets", level = "DEBUG")
      purrr::iwalk(ttd, ~ {
        if (is.data.frame(.x)) {
          logr_msg(paste("Analyzing imbalance for dataset", .y), level = "DEBUG")
          print(
            inspectdf::inspect_imb(
              df1 = .x,
              df2 = NULL,
              include_na = FALSE) |>
            inspectdf::show_plot(text_labels = TRUE))
        }
      })
    }
  }

  ### \--- NUMERIC ----
  execute_inspect_num <- function() {
    logr_msg("Starting inspect_num analysis", level = "INFO")

    ### 1 dataset ----
    if (num_datasets == 1) {
      ### has_min_cols() ----
      if (has_min_cols(num_cols, 1)) {
        logr_msg("Running inspect_num for single dataset with numeric columns",
          level = "DEBUG")
        print(
          inspectdf::inspect_num(
                df1 = ttd[[1]],
                df2 = NULL,
                breaks = 20,
                include_int = TRUE) |>
              inspectdf::show_plot(text_labels = TRUE)
          )
      } else {
        logr_msg("No numeric columns found for numerical analysis", level = "WARN")
      }
    ### 2 datasets ----
    } else if (num_datasets == 2) {
      #### has_min_cols() ----
      if (has_min_cols(num_cols, 1) && has_min_cols(num_cols, 2)) {
        logr_msg("Running inspect_num for two datasets with numeric columns", level = "DEBUG")
        print(
          inspectdf::inspect_num(
                  df1 = ttd[[1]],
                  df2 = ttd[[2]],
                  breaks = 20,
                  include_int = TRUE) |>
              inspectdf::show_plot(text_labels = TRUE))

      } else {
        # Check which dataset has numeric columns and analyze that one
        #### has_min_cols() ----
        datasets_with_num <- purrr::map_lgl(seq_along(ttd)[1:2], ~ has_min_cols(num_cols, .x))
        purrr::walk(which(datasets_with_num), ~ {
          logr_msg(paste("Running inspect_num for dataset", .x), level = "DEBUG")
          print(
            inspectdf::inspect_num(
                  df1 = ttd[[.x]],
                  df2 = NULL,
                  breaks = 20,
                  include_int = TRUE) |>
                inspectdf::show_plot(text_labels = TRUE))
        })

        if (!any(datasets_with_num)) {
          logr_msg("No datasets with numeric columns found", level = "WARN")
        }
      }

    ### >= 3 datasets ----
    } else if (num_datasets >= 3) {

      logr_msg("Running inspect_num for multiple datasets", level = "DEBUG")
      purrr::iwalk(ttd, ~ {
        idx <- which(names(ttd) == .y)
        #### has_min_cols() ----
        if (is.data.frame(.x) && has_min_cols(num_cols, idx)) {
          logr_msg(paste("Analyzing numeric data for dataset", .y), level = "DEBUG")

          print(
            inspectdf::inspect_num(
              df1 = .x,
              df2 = NULL,
              breaks = 20,
              include_int = TRUE) |>
              inspectdf::show_plot(text_labels = TRUE))
        }
      })
    }
  }

  ### \--- CATEGORICAL ----
  execute_inspect_cat <- function() {

    logr_msg("Starting inspect_cat analysis", level = "INFO")

    ### 1 dataset ----
    if (num_datasets == 1) {

      if (has_min_cols(cat_cols, 1)) {

        logr_msg("Running inspect_cat for single dataset with categorical columns",
          level = "DEBUG")
        print(
            inspectdf::inspect_cat(
              df1 = ttd[[1]],
              df2 = NULL,
              include_int = FALSE) |>
              inspectdf::show_plot(text_labels = TRUE)
          )

      } else {
        logr_msg("No categorical columns found for categorical analysis", level = "WARN")
      }

      ### 2 datasets ----
    } else if (num_datasets == 2) {

      # Check for intersecting categorical columns
      intersecting_cat_cols <- character(0)

      if (has_min_cols(cat_cols, 1) && has_min_cols(cat_cols, 2)) {
        intersecting_cat_cols <- intersect(cat_cols[[1]], cat_cols[[2]])
      }

      if (length(intersecting_cat_cols) > 0) {

        logr_msg(paste("Found intersecting categorical columns:",
            paste(intersecting_cat_cols, collapse = ", ")), level = "DEBUG")
        #### get_intersecting_cols() ----
        intersecting_columns <- get_intersecting_cols(ttd)

        if (length(intersecting_columns) > 0) {
          print(
            inspectdf::inspect_cat(
              df1 = ttd[[1]][intersecting_columns],
              df2 = ttd[[2]][intersecting_columns],
              include_int = FALSE) |>
                inspectdf::show_plot(text_labels = TRUE)
            )
        }
      } else {

        # run on datasets with categorical columns
        datasets_with_cat <- map_lgl(seq_along(ttd)[1:2], ~ has_min_cols(cat_cols, .x))
        purrr::walk(
          which(datasets_with_cat), ~ {
          logr_msg(paste("Running inspect_cat for dataset", .x), level = "DEBUG")
          print(
            inspectdf::inspect_cat(
              df1 = ttd[[.x]],
              df2 = NULL,
              include_int = FALSE) |>
                inspectdf::show_plot(text_labels = TRUE))
            }
          )
      }

      #### >= 3 datasets ----
    } else if (num_datasets >= 3) {

      logr_msg("Running inspect_cat for multiple datasets", level = "DEBUG")

      purrr::iwalk(ttd, ~ {
        idx <- which(names(ttd) == .y)
        if (is.data.frame(.x) && has_min_cols(cat_cols, idx)) {
          logr_msg(paste("Analyzing categorical data for dataset", .y), level = "DEBUG")
          print(
            inspectdf::inspect_cat(
              df1 = .x,
              df2 = NULL,
              include_int = FALSE) |>
                inspectdf::show_plot(text_labels = TRUE)
            )
        }
      })
    }
  }

  ##### list plot functions ----
  plot_functions <- list(
    "types" = execute_inspect_types,
    "mem" = execute_inspect_mem,
    "na" = execute_inspect_na,
    "cor" = execute_inspect_cor,
    "imb" = execute_inspect_imb,
    "num" = execute_inspect_num,
    "cat" = execute_inspect_cat
  )

  ##### execute requested plots ----
  purrr::walk(plot, ~ {
    if (.x %in% names(plot_functions)) {
      plot_functions[[.x]]()
    }
  })

  #### logging ----
  logr_msg("=== ANALYSIS SUMMARY ===", level = "INFO")
  logr_msg(paste("Total datasets analyzed:", num_datasets), level = "INFO")
  logr_msg(paste("Dataset names:", paste(dataset_names, collapse = ", ")), level = "INFO")
  logr_msg(paste("Plots generated:", paste(plot, collapse = ", ")), level = "INFO")

  #### log column type summaries -----
  purrr::iwalk(ttd, ~ {
    idx <- which(names(ttd) == .y)
    num_count <- if (has_min_cols(num_cols, idx)) length(num_cols[[idx]]) else 0
    cat_count <- if (has_min_cols(cat_cols, idx)) length(cat_cols[[idx]]) else 0
    log_count <- if (has_min_cols(log_cols, idx)) length(log_cols[[idx]]) else 0
    date_count <- if (has_min_cols(date_cols, idx)) length(date_cols[[idx]]) else 0
    list_count <- if (has_min_cols(list_cols, idx)) length(list_cols[[idx]]) else 0

    logr_msg(paste0(.y, " - Numeric: ", num_count,
                   ", Categorical: ", cat_count,
                   ", Logical: ", log_count,
                   ", Date: ", date_count,
                   ", List: ", list_count), level = "INFO")
  })

  logr_msg("Analysis complete", level = "SUCCESS")

  invisible(NULL)
}

#' Check if Dataset Has Minimum Required Columns
#'
#' Helper function to determine if a specific dataset has sufficient columns
#' of a given type for analysis.
#'
#' @param col_list A named list of column vectors (output from `check_col_types()`)
#' @param dataset_idx Integer index of the dataset to check
#' @param min_count Minimum number of columns required (default: 1)
#'
#' @return Logical indicating whether the dataset has sufficient columns
#'
#' @export
#'
#' @examples
#' ttd <- get_tt_data("Moore’s Law")
#' num_cols <- check_col_types(ttd, "num")
#' has_min_cols(num_cols, 1, min_count = 2)
#'
has_min_cols <- function(col_list, dataset_idx, min_count = 1) {
  if (dataset_idx <= length(col_list)) {
    cols <- col_list[[dataset_idx]]
    return(length(cols) >= min_count && cols[1] != 0)
  }
  return(FALSE)
}


#' Get Intersecting Column Names Between Two Datasets
#'
#' Helper function to find column names that exist in both datasets when
#' there are exactly two datasets in the
#' [TidyTuesday](https://github.com/rfordatascience/tidytuesday) data list.
#'
#' @param ttd A list of data frames from [get_tt_data()]
#'
#' @return A character vector of intersecting column names, or empty vector
#'   if no intersections or if not exactly 2 datasets
#'
#' @export
#'
#' @examples
#' ttd <- get_tt_data("Moore’s Law")
#' get_intersecting_cols(ttd)
#'
get_intersecting_cols <- function(ttd) {
  if (length(ttd) == 2) {
    intersect(names(ttd[[1]]), names(ttd[[2]]))
  } else {
    character(0)
  }
}

#' Check Column Types in TidyTuesday Data
#'
#' A wrapper function that checks for specific column types across all datasets
#' in a [TidyTuesday](https://github.com/rfordatascience/tidytuesday) data list.
#'
#' @param ttd A list of data frames from [get_tt_data()]
#' @param type Character string specifying the column type to check for.
#'   Options are:
#'  * `"num"` or `"numeric"` - numeric/integer columns
#'  * `"cat"` or `"character"` - character columns
#'  * `"log"` or `"logical"` - logical columns
#'  * `"list"` - list columns
#'  * `"date"` - date/datetime columns
#'
#' @return A named list where each element contains column names of the
#' specified type, or 0 if no columns of that type exist in that dataset
#'
#' @export
#'
#' @examples
#' ttd <- get_tt_data("Moore's Law")
#'
#' # Check for numeric columns
#' check_col_types(ttd, "num")
#'
#' # Check for character columns
#' check_col_types(ttd, "cat")
#'
#' # Check for logical columns
#' check_col_types(ttd, "log")
#'
check_col_types <- function(ttd, type) {

  # Validate input
  if (!is.character(type) || length(type) != 1) {
    stop("'type' must be a single character string")
  }

  # Normalize type argument (case insensitive, allow multiple forms)
  type <- tolower(trimws(type))

  # Call appropriate function based on type
  result <- switch(type,
    "num" = check_ttd_num_cols(ttd),
    "numeric" = check_ttd_num_cols(ttd),
    "cat" = check_ttd_cat_cols(ttd),
    "character" = check_ttd_cat_cols(ttd),
    "log" = check_ttd_log_cols(ttd),
    "logical" = check_ttd_log_cols(ttd),
    "list" = check_ttd_list_cols(ttd),
    "date" = check_ttd_date_cols(ttd),
    {
      # Default case - invalid type
      valid_types <- c("num", "numeric", "cat", "character",
        "log", "logical", "list", "date")
      stop(paste("Invalid type:", type,
                 "\nValid options are:",
        paste(valid_types, collapse = ", ")))
    }
  )

  # Log what was checked
  logr_msg(paste("Checked for", type, "columns across",
                 length(result), "datasets"), level = "DEBUG")

  return(result)
}

#' Check for Numeric Columns in TidyTuesday Data
#'
#' Checks all datasets in a TidyTuesday data list for numeric columns.
#'
#' @param ttd A list of data frames from [get_tt_data()]
#'
#' @return A named list where each element contains column names of numeric type,
#'   or 0 if no numeric columns exist in that dataset
#'
#' @export
#'
check_ttd_num_cols <- function(ttd) {
  # Filter out non-data.frame elements
  data_frames <- ttd[sapply(ttd, is.data.frame)]

  if (length(data_frames) == 0) {
    return(list())
  }

  # Check each dataset for numeric columns
  result <- lapply(data_frames, function(df) {
    num_cols <- names(df)[sapply(df, function(x) is.numeric(x) || is.integer(x))]
    if (length(num_cols) == 0) 0 else num_cols
  })

  return(result)
}

#' Check for Categorical (Character) Columns in TidyTuesday Data
#'
#' Checks all datasets in a TidyTuesday data list for character columns.
#'
#' @param ttd A list of data frames from [get_tt_data()]
#'
#' @return A named list where each element contains column names of character type,
#'   or 0 if no character columns exist in that dataset
#'
#' @export
#'
check_ttd_cat_cols <- function(ttd) {
  # Filter out non-data.frame elements
  data_frames <- ttd[sapply(ttd, is.data.frame)]

  if (length(data_frames) == 0) {
    return(list())
  }

  # Check each dataset for character columns
  result <- lapply(data_frames, function(df) {
    cat_cols <- names(df)[sapply(df, is.character)]
    if (length(cat_cols) == 0) 0 else cat_cols
  })

  return(result)
}

#' Check for List Columns in TidyTuesday Data
#'
#' Checks all datasets in a TidyTuesday data list for list columns.
#'
#' @param ttd A list of data frames from [get_tt_data()]
#'
#' @return A named list where each element contains column names of list type,
#'   or 0 if no list columns exist in that dataset
#'
#' @export
#'
check_ttd_list_cols <- function(ttd) {
  # Filter out non-data.frame elements
  data_frames <- ttd[sapply(ttd, is.data.frame)]

  if (length(data_frames) == 0) {
    return(list())
  }

  # Check each dataset for list columns
  result <- lapply(data_frames, function(df) {
    list_cols <- names(df)[sapply(df, is.list)]
    if (length(list_cols) == 0) 0 else list_cols
  })

  return(result)
}

#' Check for Logical Columns in TidyTuesday Data
#'
#' Checks all datasets in a TidyTuesday data list for logical columns.
#'
#' @param ttd A list of data frames from [get_tt_data()]
#'
#' @return A named list where each element contains column names of logical type,
#'   or 0 if no logical columns exist in that dataset
#'
#' @export
#'
check_ttd_log_cols <- function(ttd) {
  # Filter out non-data.frame elements
  data_frames <- ttd[sapply(ttd, is.data.frame)]

  if (length(data_frames) == 0) {
    return(list())
  }

  # Check each dataset for logical columns
  result <- lapply(data_frames, function(df) {
    log_cols <- names(df)[sapply(df, is.logical)]
    if (length(log_cols) == 0) 0 else log_cols
  })

  return(result)
}

#' Check for Date Columns in TidyTuesday Data
#'
#' Checks all datasets in a TidyTuesday data list for date/datetime columns.
#'
#' @param ttd A list of data frames from [get_tt_data()]
#'
#' @return A named list where each element contains column names of date/datetime type,
#'   or 0 if no date columns exist in that dataset
#'
#' @export
#'
check_ttd_date_cols <- function(ttd) {
  # Filter out non-data.frame elements
  data_frames <- ttd[sapply(ttd, is.data.frame)]

  if (length(data_frames) == 0) {
    return(list())
  }

  # Check each dataset for date columns
  result <- lapply(data_frames, function(df) {
    date_cols <- names(df)[sapply(df, function(x) {
      inherits(x, "Date") ||
      inherits(x, "POSIXct") ||
      inherits(x, "POSIXlt") ||
      inherits(x, "POSIXt")
    })]
    if (length(date_cols) == 0) 0 else date_cols
  })

  return(result)
}


#' Determine length of TidyTuesday Data
#'
#' @param ttd A list of data frames from [get_tt_data()]
#'
#' @returns integer length of `ttd`
#'
#' @export
#'
#' @examples
#' ttd <- get_tt_data("Moore’s Law")
#' ttd_length(ttd)
#'
ttd_length <- function(ttd) {
  if (length(ttd) == 1) {
    nms <- names(ttd)
    logr_msg(paste0("One dataset: ", nms), level = "DEBUG")
    return(as.integer(length(ttd)))
  } else if (length(ttd) == 2) {
    nms <- paste(names(ttd), collapse = ", ")
    logr_msg(paste0("Two datasets: ", nms), level = "DEBUG")
    return(as.integer(length(ttd)))
  } else if (length(ttd) > 2) {
    nms <- paste(names(ttd), collapse = ", ")
    logr_msg(paste("Many datasets: ", nms), level = "DEBUG")
    return(as.integer(length(ttd)))
  } else {
    logr_msg("Can't determine datasets in ttd",
      level = "WARN")
    return(as.integer(0))
  }
}

