### \--- GET TIDY TUESDAY DATA WITH FILE NAMES --\ ----
#' Get TidyTuesday Data with File Names
#'
#' Retrieves TidyTuesday datasets based on the dataset title and returns them
#' with the original file names as list element names. Includes robust handling
#' of various edge cases and file name formats.
#'
#' @param title Character string of the dataset title
#' @param preserve_original_names Logical, whether to preserve original dataset
#'   names if file names are not available (default: FALSE)
#'
#' @return A named list containing the dataset(s) where names correspond to
#'   the original CSV file names from TidyTuesday. The list includes an attribute
#'   'clean_title' with the cleaned snake_case version of the dataset title.
#'
#' @export
#'
#' @examples
#' ttd <- get_tt_data("Moore’s Law")
#' str(ttd)
#' attr(ttd, "clean_title")  # "moores_law"
#'
#' ttd <- get_tt_data("House Price Index & Mortgage Rates")
#' str(ttd)
#' attr(ttd, "clean_title")  # "house_price_index_mortgage_rates"
#'
#' ttd <- get_tt_data("LEGO database")
#' str(ttd)
#' attr(ttd, "clean_title")  # "lego_database"
get_tt_data <- function(title, preserve_original_names = FALSE) {

  logr_msg(paste("Attempting to retrieve data for:", title), level = "INFO")

  tryCatch({
    # Check title exists
    if (!title %in% all_tt_combined$title) {
      logr_msg(paste("Dataset title not found:", title),
        level = "ERROR")
      stop("Dataset title not found in available datasets")
    }

    # Get corresponding year and week
    dataset_info <- all_tt_combined[all_tt_combined$title == title, ]
    year <- dataset_info$year[1]
    week <- dataset_info$week[1]

    logr_msg(paste("Found dataset info - Year:", year, "Week:", week),
      level = "DEBUG")

    # load data tidytuesdayR::tt_load() -----
    tt_data <- tidytuesdayR::tt_load(x = year, week = week, files = "All")

    logr_msg(paste("Successfully loaded data for:", title),
      level = "SUCCESS")
    logr_msg(paste("Number of datasets returned:", length(tt_data)),
      level = "INFO")

    ## extract_file_names()  -----
    ### extract file names from .tt attribute  -----
    file_names <- extract_file_names(tt_data, preserve_original_names)

    ## create_named_result()  ------
    ### create list with element names  ------
    result <- create_named_result(tt_data, file_names)

    ## extract_dataset_title() ----
    ### add clean_title attribute -----
    clean_title <- extract_dataset_title(title)
    attr(result, "clean_title") <- clean_title

    logr_msg(
      paste("Final result has",
        length(result), "datasets with names:",
          paste(names(result), collapse = ", ")), level = "INFO")

    logr_msg(paste("Clean title:", clean_title), level = "DEBUG")

    return(result)

  }, error = function(e) {
    logr_msg(
      paste("Error retrieving data for",
        title, ":", e$message), level = "ERROR")

    # return empty list with message
    result <- list()
    attr(result, "error") <- e$message

    ## ERROR extract_dataset_title() ----
    ### add clean_title even for errors ----
    tryCatch({
      clean_title <- extract_dataset_title(title)
      attr(result, "clean_title") <- clean_title
    }, error = function(e2) {
      attr(result, "clean_title") <- "unknown_title"
    })

    return(result)

  }, warning = function(w) {

    ## LOG WARNING ----
    logr_msg(paste("Warning while retrieving data for", title, ":", w$message),
             level = "WARN")
    # continue processing despite warnings
    NULL
  })
}

### \--- EXTRACT FILE NAMES --\ ----
#' Extract file names from TidyTuesday data object
#'
#' @param tt_data list of TidyTuesday data from [get_tt_data()]
#' @param preserve_original_names Logical, fallback to original names
#'
#' @return Character vector of file names with .gz extensions removed
#'
#' @export
#'
#' @examples
#' ttd <- get_tt_data("Moore’s Law")
#' extract_file_names(ttd)
#'
extract_file_names <- function(tt_data, preserve_original_names = FALSE) {
  # get file names from .tt attribute
  file_names <- attr(tt_data, ".tt")

  if (!is.null(file_names) && is.character(file_names)) {
    logr_msg(paste(
      "Extracted file names from .tt attribute:",
      paste(file_names, collapse = ", ")
    ), level = "DEBUG")

    # Ensure we have the same number of names as datasets
    if (length(file_names) == length(tt_data)) {
      # Remove .gz extension if present
      file_names <- gsub("\\.gz$", "", file_names)
      logr_msg(paste(
        "File names after .gz removal:",
        paste(file_names, collapse = ", ")
      ), level = "DEBUG")
      return(file_names)
    } else {
      logr_msg("Mismatch between .tt file names and number of datasets", level = "WARN")
    }
  }

  # get from .files attribute
  files_attr <- attr(tt_data, ".files")
  if (!is.null(files_attr) && is.data.frame(files_attr) && "data_files" %in% names(files_attr)) {
    file_names <- files_attr$data_files
    logr_msg(paste(
      "Using file names from .files attribute:",
      paste(file_names, collapse = ", ")
    ), level = "DEBUG")

    if (length(file_names) == length(tt_data)) {
      # Remove .gz extension if present
      file_names <- gsub("\\.gz$", "", file_names)
      logr_msg(paste(
        "File names after .gz removal:",
        paste(file_names, collapse = ", ")
      ), level = "DEBUG")
      return(file_names)
    }
  }

  # final fallback
  if (preserve_original_names && !is.null(names(tt_data))) {
    logr_msg("Using original dataset names as fallback", level = "INFO")
    fallback_names <- names(tt_data)
    # Remove .gz extension if present
    fallback_names <- gsub("\\.gz$", "", fallback_names)
    return(fallback_names)
  } else {
    # generic names
    generic_names <- paste0("dataset_", seq_along(tt_data), ".csv")
    logr_msg(
      paste(
        "Generated generic file names:",
          paste(generic_names, collapse = ", ")
          ), level = "WARN")
    return(generic_names)
  }
}

### \--- CREATE NAMED RESULT --\ ----
#' Create named result list from TidyTuesday data
#'
#' @param tt_data TidyTuesday data object
#' @param file_names Character vector of file names
#'
#' @return Named list of cleaned datasets
#'
#' @export
#'
#' @examples
#' ttd <- get_tt_data("Moore’s Law")
#' create_named_result(ttd, names(ttd))
#'
create_named_result <- function(tt_data, file_names) {
  # convert to list and set names
  result <- setNames(as.list(tt_data), file_names)

  # clean up each dataset
  result <- lapply(result, function(x) {
    if (is.data.frame(x)) {
      # remove tt-specific attributes
      attrs_to_remove <- c(".tt", ".files", ".readme", ".date")
      for (attr_name in attrs_to_remove) {
        attr(x, attr_name) <- NULL
      }

      # ensure it's still a proper data frame
      if (!inherits(x, "data.frame")) {
        x <- as.data.frame(x)
      }
    }
    return(x)
  })

  # remove class attributes
  class(result) <- "list"

  return(result)
}

### \--- GET TIDY TUESDAY TITLE METADATA --\ ----
#' Get Dataset Metadata
#'
#' Analyzes the column types across all datasets in a TidyTuesday data list
#' and returns a `tibble` with column type information.
#'
#' @param ttd A list of data frames from [get_tt_data()]
#'
#' @return A tibble with four columns:
#'  * `clean_title`: Clean title of data
#'  * `col_type`: Type of column (numeric, logical, character, list)
#'  * `dataset`: Name of the dataset
#'  * `col`: Column name (NA if no columns of that type exist)
#'
#'
#' @seealso [get_tt_data()]
#'
#' @export
#'
#' @examples
#' ttd <- get_tt_data("Moore’s Law")
#' meta <- get_tt_title_meta(ttd)
#' print(meta)
#'
get_tt_title_meta <- function(ttd) {
  # check if list
  if (!is.list(ttd)) {
    logr_msg(paste("Error: Input is not a list:", class(ttd)), level = "ERROR")
    stop("Input must be a list of data frames")
  }

  # filter to data.frame elements
  data_frames <- ttd[sapply(ttd, is.data.frame)]

  if (length(data_frames) == 0) {
    logr_msg("Error: No data frames found in list", level = "ERROR")
    stop("No data frames found in the input list")
  }

  logr_msg(paste("Datasets in list:", paste(names(data_frames), collapse = ", ")),
           level = "INFO")

  # define type_checks() -----
  # type checking functions
  type_checks <- list(
    numeric = function(x) is.numeric(x) || is.integer(x),
    logical = is.logical,
    character = is.character,
    list = is.list
  )

  ## get_cols_by_type() -----
  # get columns of a specific type from a data frame
  get_cols_by_type <- function(df, type_name, type_check_fn) {
    cols <- names(df)[sapply(df, type_check_fn)]
    if (length(cols) == 0) NA_character_ else cols
  }

  ### process_dataset() ----
  process_dataset <- function(dataset_name, df) {
    logr_msg(paste("Analyzing dataset:", dataset_name, "with", ncol(df), "columns"),
             level = "DEBUG")

    # get columns for types
    type_results <- Map(function(type_name, type_check_fn) {
      cols <- get_cols_by_type(df, type_name, type_check_fn)
      tibble::tibble(
        dataset = dataset_name,
        col_type = type_name,
        col = cols
      )
    }, names(type_checks), type_checks)

    # dataset_result ----
    # combine results for dataset
    dataset_result <- do.call(rbind, type_results)

    # log findings
    type_counts <- sapply(type_checks, function(fn) sum(sapply(df, fn)))

    logr_msg(paste("Dataset", dataset_name, "- Numeric:", type_counts[1],
                   "Logical:", type_counts[2],
                   "Character:", type_counts[3],
                   "List:", type_counts[4]), level = "DEBUG")

    return(dataset_result)
  }

  ## all_results ----
  # process all datasets
  all_results <- Map(process_dataset, names(data_frames), data_frames)

  # result ----
  # Combine all results
  result <- do.call(rbind, all_results)

  ## add clean_title ----
  result$clean_title <- attr(ttd, "clean_title")

  # reorder columns
  result <- result[, c("clean_title", "dataset", "col", "col_type")]

  # log summary
  logr_msg(paste("Created metadata tibble with", nrow(result), "rows covering",
                 length(unique(result$dataset)), "datasets"), level = "INFO")

  return(result)
}

### \--- CLEAN TITLE STREAM --\ ----
#' Clean Title String
#'
#' Internal function that performs the actual string cleaning operations.
#' This function handles various types of punctuation and formatting issues
#' commonly found in dataset titles.
#'
#' @param title Character string to clean
#'
#' @return Character string cleaned and converted to snake_case
#'
#' @details
#' The cleaning process follows these steps:
#'  * Remove or replace problematic characters
#'  *  Normalize whitespace
#'  *  Convert to snake_case
#'  *  Final cleanup of underscores
#'
#' @export
#'
clean_title_string <- function(title) {

  if (is.null(title) || !is.character(title) || length(title) == 0) {
    return("unknown_title")
  }

  # original title
  cleaned <- title

  # quotes and apostrophes
  # unicode escape sequences for special quote characters
  cleaned <- gsub('["\'`\u201C\u201D\u2018\u2019]', '', cleaned)  # Remove all types of quotes

  # contractions and possessives
  # convert common contractions (e.g., "D'oh" -> "Doh", "it's" -> "its")
  cleaned <- gsub("'([a-zA-Z])", "\\1", cleaned)  # Remove apostrophes before letters
  cleaned <- gsub("([a-zA-Z])'s\\b", "\\1s", cleaned)  # Convert possessives
  cleaned <- gsub("([a-zA-Z])'([a-zA-Z])", "\\1\\2", cleaned)  # Handle internal apostrophes

  # punctuation marks (note: escaped curly braces)
  cleaned <- gsub("[!@#$%^&*()+=\\[\\]\\{\\}|;:,.<>?/~`]", " ", cleaned)  # Replace with spaces

  # dashes and special separators
  cleaned <- gsub("[-–—]", " ", cleaned)  # Convert all types of dashes to spaces

  # special characters and symbols
  cleaned <- gsub("[©®™°§¶†‡•‰‱€£¥₹₽¢]", "", cleaned)  # Remove special symbols

  # normalize whitespace
  cleaned <- gsub("\\s+", " ", cleaned)  # Multiple spaces to single space
  cleaned <- trimws(cleaned)  # Remove leading/trailing whitespace

  # convert to snake_case using snakecase package
  cleaned <- snakecase::to_snake_case(cleaned)

  # final cleanup
  cleaned <- gsub("_+", "_", cleaned)  # Multiple underscores to single
  cleaned <- gsub("^_|_$", "", cleaned)  # Remove leading/trailing underscores

  # empty results
  # if (cleaned == "" || is.na(cleaned)) {
  #   cleaned <- "untitled_dataset"
  # }

  # make sure it starts with a letter (for valid R names)
  if (grepl("^[0-9]", cleaned)) {
    cleaned <- paste0("dataset_", cleaned)
  }

  return(cleaned)
}


### \--- EXTRACT DATASET TITLE --\ ----
#' Extract and Clean Dataset Title
#'
#' `extract_dataset_title()` takes a dataset title and returns a cleaned version suitable
#' for use as a filename or identifier. It handles various punctuation marks,
#' special characters, and formatting to create a consistent snake_case output.
#'
#' @param dataset_title Character string of the original dataset title
#'
#' @return Character string of the cleaned title in snake_case format
#'
#' @details
#' The function performs the following cleaning operations:
#'  * Removes quotes (single and double)
#'  * Removes apostrophes and converts contractions (e.g., "D'oh" becomes "Doh")
#'  * Removes punctuation marks (commas, periods, exclamation marks, etc.)
#'  * Removes parentheses and brackets
#'  * Removes dashes and converts to underscores
#'  * Converts to lowercase
#'  * Replaces multiple spaces with single spaces
#'  * Converts to snake_case format
#'  * Removes multiple consecutive underscores
#'
#' @examples
#' extract_dataset_title("Bring your own data from 2024!")
#' # Returns: "bring_your_own_data_from_2024"
#'
#' extract_dataset_title("Donuts, Data, and D'oh - A Deep Dive into The Simpsons")
#' # Returns: "donuts_data_and_doh_a_deep_dive_into_the_simpsons"
#'
#' extract_dataset_title("Moore's Law")
#'
#' extract_dataset_title("U.S. Wind Turbines (2018-2022)")
#'
#' @seealso
#' [snakecase::to_snake_case](https://cran.r-project.org/web/packages/snakecase/vignettes/introducing-the-snakecase-package.html) for the underlying `snake_case` conversion
#'
#' @export
#'
extract_dataset_title <- function(dataset_title) {

  # Validate input
  if (is.null(dataset_title) || !is.character(dataset_title) || length(dataset_title) == 0) {
    logr_msg("Invalid dataset_title provided",
      level = "ERROR")
    return("unknown_dataset")
  }

  logr_msg(paste("Extracting clean title for:", dataset_title),
    level = "DEBUG")

  tryCatch({

    if (dataset_title != "") {
      # Check if dataset exists in the metadata
      title_exists <- TRUE
    } else {
      logr_msg("Empty dataset_title provided", level = "WARN")
      return("empty_dataset")
    }

    if (title_exists) {
      logr_msg(paste("Dataset title found in metadata!", dataset_title), level = "SUCCESS")
      # Clean the title even if not found
      cleaned_title <- clean_title_string(dataset_title)
      return(cleaned_title)
    }

    # Get title from metadata and clean it
    title_tbl <- dplyr::inner_join(all_tt_data, all_tt_meta,
                                  by = c("year", "week")) |>
      dplyr::filter(title == dataset_title) |>
      dplyr::mutate(
        clean_title = clean_title_string(title)
      ) |>
      dplyr::select(clean_title)

    cleaned_result <- unique(title_tbl[['clean_title']])

    if (length(cleaned_result) == 0) {
      logr_msg("No clean title extracted, using fallback", level = "WARN")
      return(clean_title_string(dataset_title))
    }

    logr_msg(paste("Successfully extracted clean title:", cleaned_result[1]), level = "DEBUG")
    return(cleaned_result[1])

  }, error = function(e) {
    logr_msg(paste("Error extracting dataset title:", e$message), level = "ERROR")
    # Fallback to direct string cleaning
    return(clean_title_string(dataset_title))
  })
}
