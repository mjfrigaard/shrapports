#' Test Dataset Title Cleaning
#'
#' A utility function to test the title cleaning functionality with
#' various edge cases and examples.
#'
#' @return A data frame showing original titles and their cleaned versions
#'
#' @examples
#' test_title_cleaning()
#'
#' @export
test_title_cleaning <- function() {

  test_titles <- c(
    "Bring your own data from 2024!",
    "Donuts, Data, and D'oh - A Deep Dive into The Simpsons",
    "Moore's Law",
    "U.S. Wind Turbines (2018-2022)",
    "It's Always Sunny in Philadelphia",
    "Bob's Burgers & Restaurant Data",
    "COVID-19: The Pandemic's Impact",
    "Taylor Swift's Albums & Songs",
    "NASA's Space Missions",
    "Women's World Cup 2023",
    "Pride & Prejudice Text Analysis",
    "R-Ladies: Global Chapter Events",
    "Lorem Ipsum... Text Generator!",
    "Data Science @ Work",
    "AI/ML in Healthcare",
    "50% Off: Black Friday Sales",
    "#TidyTuesday Community",
    "Star Wars: The Force Awakens",
    "Climate Change – Global Temperatures",
    "NYC Taxi & Uber Data"
  )

  cleaned_titles <- sapply(test_titles, clean_title_string, USE.NAMES = FALSE)

  result <- data.frame(
    Original = test_titles,
    Cleaned = cleaned_titles
  )
  return(result)
}
