#' Variable Input Module UI
#'
#' @param id The module ID
#'
#' @return A UI element
#'
#' @export
#'
var_input_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    min_height = "450px",
    bslib::card_header("Inputs"),
    bslib::card_body(
      selectInput(ns("dataset"), "Choose a dataset:",
        choices = c("storms", "penguins", "diamonds")
      ),
      conditionalPanel(
        condition = paste0("input['", ns("dataset"), "'] == 'storms'"),
        # storms x variable selection ----
        selectInput(ns("storms_x"), "X variable:",
          choices = c(
            "wind", "pressure"
          ),
          selected = "wind"
        ),
        # storms y variable selection ----
        selectInput(ns("storms_y"), "Y variable:",
          choices = c(
            "wind", "pressure"
          ),
          selected = "pressure"
        )
      ),
      conditionalPanel(
        # penguins x variable selection ----
        condition = paste0("input['", ns("dataset"), "'] == 'penguins'"),
        selectInput(ns("penguins_x"), "X variable:",
          choices = c(
            "bill_length_mm", "bill_depth_mm",
            "flipper_length_mm", "body_mass_g"
          ),
          selected = "body_mass_g"
        ),
        # penguins y variable selection ----
        selectInput(ns("penguins_y"), "Y variable:",
          choices = c(
            "bill_length_mm", "bill_depth_mm",
            "flipper_length_mm", "body_mass_g"
          ),
          selected = "flipper_length_mm"
        )
      ),
      conditionalPanel(
        # diamonds x variable selection ----
        condition = paste0("input['", ns("dataset"), "'] == 'diamonds'"),
        selectInput(ns("diamonds_x"), "X variable:",
          choices = c("carat", "depth", "table", "price"),
          selected = "carat"
        ),
        # diamonds y variable selection ----
        selectInput(ns("diamonds_y"), "Y variable:",
          choices = c("carat", "depth", "table", "price"),
          selected = "price"
        )
      )
    )
  )
}

#' Variable Input Module Server
#'
#' @param id The module ID
#'
#' @return A list of reactive values
#'
#' @export
var_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Set default values for y variables that differ from x variables
    observe({
      if (input$dataset == "storms") {
        # if x is wind, set y to pressure
        if (input$storms_x == "wind" && input$storms_y == "wind") {
          updateSelectInput(session, "storms_y", selected = "pressure")
        } else if (input$storms_x == "pressure" && input$storms_y == "pressure") {
          updateSelectInput(session, "storms_y", selected = "wind")
        }
      } else if (input$dataset == "penguins") {
        # if x is bill_length_mm, set y to bill_depth_mm
        if (input$penguins_x == "bill_length_mm" && input$penguins_y == "bill_length_mm") {
          updateSelectInput(session, "penguins_y", selected = "bill_depth_mm")
        } else if (input$penguins_x == "bill_depth_mm" && input$penguins_y == "bill_depth_mm") {
          updateSelectInput(session, "penguins_y", selected = "bill_length_mm")
        }
      } else if (input$dataset == "diamonds") {
        # if x is carat, set y to price
        if (input$diamonds_x == "carat" && input$diamonds_y == "carat") {
          updateSelectInput(session, "diamonds_y", selected = "price")
        } else if (input$diamonds_x == "price" && input$diamonds_y == "price") {
          updateSelectInput(session, "diamonds_y", selected = "carat")
        }
      }
    })

    # initialize datasets
    dataset <- reactive({
      switch(input$dataset,
        "storms" = dplyr::select(dplyr::storms, status, wind, pressure),
        "penguins" = palmerpenguins::penguins,
        # only 10000 rows for diamonds
        "diamonds" = dplyr::slice_sample(ggplot2::diamonds, n = 10000)
      )
    })

    # x variable selection
    x_var <- reactive({
      switch(input$dataset,
        "storms" = input$storms_x,
        "penguins" = input$penguins_x,
        "diamonds" = input$diamonds_x
      )
    })

    # y variable selection
    y_var <- reactive({
      switch(input$dataset,
        "storms" = input$storms_y,
        "penguins" = input$penguins_y,
        "diamonds" = input$diamonds_y
      )
    })

    # color variable
    color_var <- reactive({
      if (input$dataset == "penguins") {
        return("species")
      } else if (input$dataset == "diamonds") {
        return("cut")
      } else if (input$dataset == "storms") {
        return("status")
      } else {
        return(NULL)
      }
    })

    # return reactive values
    return(list(
      dataset_name = reactive(input$dataset),
      data = dataset,
      x_var = x_var,
      y_var = y_var,
      color_var = color_var
    ))
  })
}
