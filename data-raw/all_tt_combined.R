## code to prepare `all_tt_combined` dataset goes here
load("data/all_tt_data.rda")
load("data/all_tt_meta.rda")
all_tt_combined <- dplyr::left_join(
  x = all_tt_meta,
  y = all_tt_data,
  by = c("year", "week")
) |>
  dplyr::select(
    title,
    clean_title,
    dataset_name,
    year,
    week,
    date,
    variables,
    observations,
    variable_details,
    source_title,
    article_title
  ) |>
  tidyr::unnest(variable_details)
usethis::use_data(all_tt_combined, overwrite = TRUE)
