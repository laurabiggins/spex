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
 
  )
}
    
#' scatterplot Server Function
#'
#' @noRd 
mod_scatterplot_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_scatterplot_ui("scatterplot_ui_1")
    
## To be copied in the server
# callModule(mod_scatterplot_server, "scatterplot_ui_1")
 
