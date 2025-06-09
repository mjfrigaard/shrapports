#' Get Datasets with Sufficient Columns
#'
#' Returns indices of datasets that have at least the minimum required number
#' of columns of a specific type.
#'
#' @param col_list A named list of column vectors (output from
#'   `check_col_types()`)
#' @param min_count Integer minimum number of columns required (default: 1)
#'
#' @return Integer vector of dataset indices that meet the requirement
#'
#' @export
#'
#' @examples
#' ttd <- get_tt_data("Moore's Law")
#' num_cols <- check_col_types(ttd, "num")
#' get_valid_datasets(num_cols, min_count = 2)
#'
get_valid_datasets <- function(col_list, min_count = 1) {
  which(
    sapply(
      X = seq_along(col_list),
      FUN = function(i) has_min_cols(col_list, i, min_count))
    )
}
