#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  metadata <- golem::get_golem_options("metadata")
  meta_sum <- get_condition_summary(metadata) 
  dataset <- golem::get_golem_options("dataset")
  
  output$meta_table <- DT::renderDataTable(dt_setup(metadata, n_rows = 20))
  
  output$meta_summary <- renderTable(meta_sum[[input$selected_condition]])
  
  output$data_table <- DT::renderDataTable(
    dt_setup(dataset, n_rows = 20, dom_opt = "ftlip", show_rownames = TRUE)
  )

  mod_histogramServer("hist")
  
  mod_scatterplot_server("scatter", dataset)
  
  observeEvent(input$browser, browser())
}
