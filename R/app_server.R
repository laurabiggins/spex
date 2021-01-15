#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  # 
  metadata <- metaFem
  meta_sum <- get_condition_summary(metadata) 
  
  output$meta_table <- DT::renderDataTable(metadata)
  
  output$meta_summary <- renderTable(meta_sum[[input$selected_condition]])
  
  dataset <- femExpression
  output$data_table <- DT::renderDataTable(dataset)

  # output$meta_table <- DT::renderDataTable(1:10)
  # 
  # output$meta_summary <- renderTable(40:50)
  
  #output$data_table <- DT::renderDataTable(1:20)

  mod_histogramServer("hist")
  
  mod_scatterplot_server("scatter")
}
