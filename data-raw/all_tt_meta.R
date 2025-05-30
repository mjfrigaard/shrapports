## code to prepare `all_tt_meta` dataset goes here
# install.packages(c("tidyr", "ttmeta"))
library(ttmeta)
library(tidyr)
# load the all_tt_data from get_tt_tbl()
tt_data_25 <- ttmeta::get_tt_tbl(min_year = 2025L, max_year = this_year())
tt_meta_25 <- ttmeta::get_tt_datasets_metadata(tt_data_25)

tt_data_24 <- ttmeta::get_tt_tbl(min_year = 2024L, max_year = 2024L)
tt_meta_24 <- ttmeta::get_tt_datasets_metadata(tt_data_24)

tt_data_23 <- ttmeta::get_tt_tbl(min_year = 2023L, max_year = 2023L)
tt_meta_23 <- ttmeta::get_tt_datasets_metadata(tt_data_23)

tt_data_22 <- ttmeta::get_tt_tbl(min_year = 2022L, max_year = 2022L)
tt_meta_22 <- ttmeta::get_tt_datasets_metadata(tt_data_22)

tt_data_21 <- ttmeta::get_tt_tbl(min_year = 2021L, max_year = 2021L)
tt_meta_21 <- ttmeta::get_tt_datasets_metadata(tt_data_21)

tt_data_20 <- ttmeta::get_tt_tbl(min_year = 2020L, max_year = 2020L)
tt_meta_20 <- ttmeta::get_tt_datasets_metadata(tt_data_20)

tt_data_19 <- ttmeta::get_tt_tbl(min_year = 2019L, max_year = 2019L)
tt_meta_19 <- ttmeta::get_tt_datasets_metadata(tt_data_19)

tt_data_18 <- ttmeta::get_tt_tbl(min_year = 2018L, max_year = 2018L)
tt_meta_18 <- ttmeta::get_tt_datasets_metadata(tt_data_18)

all_tt_meta <- dplyr::bind_rows(
  tt_meta_25,
  tt_meta_24,
  tt_meta_23,
  tt_meta_22,
  tt_meta_21,
  tt_meta_20,
  tt_meta_19,
  tt_meta_18
)

usethis::use_data(all_tt_meta, overwrite = TRUE)
