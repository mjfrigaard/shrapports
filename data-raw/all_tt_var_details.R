## code to prepare `all_tt_var_details` dataset goes here
# install.packages("tidyr)
library(tidyr)
# load all metadata
load(file = "data/all_tt_meta.rda")
# unnest variable_details
all_tt_var_details <- tidyr::unnest(all_tt_meta, variable_details)
usethis::use_data(all_tt_var_details, overwrite = TRUE)
