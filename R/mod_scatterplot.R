#' scatterplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scatterplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(id = ns("panel"),
              plotOutput(ns("plot"))
    )
  )
}
    
#' scatterplot Server Function
#'
#' @noRd 
# mod_scatterplot_server <- function(input, output, session){
#   ns <- session$ns
#  
# }

mod_scatterplot_server <- function(id, prefix = "") {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlot(plot(rnorm(200), rnorm(200)))
    }
  )
}

    
## To be copied in the UI
# mod_scatterplot_ui("scatterplot_ui_1")
    
## To be copied in the server
# callModule(mod_scatterplot_server, "scatterplot_ui_1")
 
