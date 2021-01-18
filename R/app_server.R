#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  metadata <- golem::get_golem_options("metadata")
  meta_sum <- get_condition_summary(metadata) 
  
  output$meta_table <- DT::renderDataTable(dt_setup(metadata, n_rows = 20))
  
  output$meta_summary <- renderTable(meta_sum[[input$selected_condition]])
  
  dataset <- golem::get_golem_options("dataset")
  output$data_table <- DT::renderDataTable(dataset)

  mod_histogramServer("hist")
  
  mod_scatterplot_server("scatter")
  
  observeEvent(input$browser, browser())
}
