#' Get TidyTuesday Data with File Names (Enhanced)
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
#'   the original CSV file names from TidyTuesday
#'
#' @export
#'
#' @examples
#' ttd <- get_tt_data("Powerlifting")
#' str(ttd)
#' ttd <- get_tt_data("SuperBowl Ads")
#' str(ttd)
#' ttd <- get_tt_data("Monster Movies")
#' str(ttd)
get_tt_data <- function(title, preserve_original_names = FALSE) {
  logr_msg(paste("Attempting to retrieve data for:", title), level = "INFO")

  tryCatch({
    # Check if title exists
    if (!title %in% all_tt_combined$title) {
      logr_msg(paste("Dataset title not found:", title), level = "ERROR")
      stop("Dataset title not found in available datasets")
    }

    # Get the corresponding year and week
    dataset_info <- all_tt_combined[all_tt_combined$title == title, ]
    year <- dataset_info$year[1]
    week <- dataset_info$week[1]

    # browser()

    logr_msg(paste("Found dataset info - Year:", year, "Week:", week), level = "DEBUG")

    # Get the data using tidytuesdayR
    tt_data <- tidytuesdayR::tt_load(x = year, week = week, files = "All")

    logr_msg(paste("Successfully loaded data for:", title), level = "SUCCESS")
    logr_msg(paste("Number of datasets returned:", length(tt_data)), level = "INFO")

    # Extract file names from the .tt attribute
    file_names <- extract_file_names(tt_data, preserve_original_names)

    # Create the result list with file names as element names
    result <- create_named_result(tt_data, file_names)

    logr_msg(paste("Final result has", length(result), "datasets with names:",
                  paste(names(result), collapse = ", ")), level = "INFO")

    return(result)

  }, error = function(e) {
    logr_msg(paste("Error retrieving data for", title, ":", e$message),
             level = "ERROR")

    # Return empty list with meaningful message
    result <- list()
    attr(result, "error") <- e$message
    return(result)

  }, warning = function(w) {
    logr_msg(paste("Warning while retrieving data for", title, ":", w$message),
             level = "WARN")

    # Continue processing despite warnings
    NULL
  })
}

#' Extract file names from TidyTuesday data object
#'
#' @param tt_data TidyTuesday data object
#' @param preserve_original_names Logical, fallback to original names
#'
#' @return Character vector of file names
#'
#' @keywords internal
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
      return(file_names)
    }
  }

  # final fallback
  if (preserve_original_names && !is.null(names(tt_data))) {
    logr_msg("Using original dataset names as fallback", level = "INFO")
    return(names(tt_data))
  } else {
    # generic names
    generic_names <- paste0("dataset_", seq_along(tt_data), ".csv")
    logr_msg(paste(
      "Generated generic file names:",
      paste(generic_names, collapse = ", ")
    ), level = "WARN")
    return(generic_names)
  }
}


#' Create named result list from TidyTuesday data
#'
#' @param tt_data TidyTuesday data object
#' @param file_names Character vector of file names
#'
#' @return Named list of cleaned datasets
#'
#' @keywords internal
#'
create_named_result <- function(tt_data, file_names) {
  # Convert to list and set names
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
