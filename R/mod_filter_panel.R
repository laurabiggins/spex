#' filter_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filter_panel_ui <- function(id, meta_field){
  
  ns <- NS(id)
  
  tabPanel(
    id,
    wellPanel(
      style = "padding: 0px",
      fluidRow(
        column(
          width = 4,
          wellPanel(  
            h3("INCLUDE"),
            style = "margin: 0px",
            checkboxGroupInput(
              inputId = ns("include_factors"),
              label = "",
              choices = meta_field,
              selected = meta_field
            )
          )
        ),
        column(
          width = 8,
          wellPanel(
            h3("Filter by value"),
            style = "margin: 0px",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = ns("factor"),
                  label   = "", 
                  choices = meta_field
                )
              ),
              column(
                width = 5,
                selectInput(
                  inputId = ns("comparator"), 
                  label   = "",
                  choices = c("greater than", "less than", "equal to")
                )  
              ),
              column(
                width = 3,
                numericInput(inputId = ns("filter_value"), "", 0)
              )  
            )
          )
        )  
      )
    )
  )  
}

