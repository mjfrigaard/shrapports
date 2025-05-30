#' Create a fallback template when package template is missing
#'
#' @param template_path Path where to create the template
#'
#' @keywords internal
#'
create_fallback_template <- function(template_path) {

  logr_msg("Creating fallback template", level = "INFO")

  template_content <- '---
title: "`r params$title`"
output: html_document
params:
  dataset_title: "Dataset"
  title: "TidyTuesday Report"
  data_list: NULL
  plots: NULL
  plot_type: "type"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# `r params$dataset_title`

This is a basic report for the TidyTuesday dataset.

```{r}
if (!is.null(params$data_list) && length(params$data_list) > 0) {
  first_df <- params$data_list[[1]]
  cat("Dataset dimensions:", nrow(first_df), "rows ×", ncol(first_df), "columns\\n")

  # Show basic summary
  knitr::kable(head(first_df, 5), caption = "Data Preview")
}
```

```{r}
if (!is.null(params$plots) && inherits(params$plots, "gg")) {
  print(params$plots)
}
```
'

  writeLines(template_content, template_path)
}
