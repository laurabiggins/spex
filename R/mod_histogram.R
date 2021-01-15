#' histogram UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_histogram_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' histogram Server Function
#'
#' @noRd 
mod_histogram_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_histogram_ui("histogram_ui_1")
    
## To be copied in the server
# callModule(mod_histogram_server, "histogram_ui_1")
 
