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
  sample_names <-golem::get_golem_options("sample_names")
  
  
  lapply(meta_sum, function(x) {
    prependTab(
      inputId = "nav_name", 
      select = TRUE,
      mod_filter_panel_ui(id = names(x)[1], meta_field = x[[1]]))
  })
  
  filtered_dataset <- reactiveVal(dataset)
  
  output$meta_table <- DT::renderDataTable(dt_setup(metadata, n_rows = 20))
  
  output$meta_summary <- renderTable({
    req(input$selected_condition)
    req(meta_sum)
    meta_sum[[input$selected_condition]]
  })
  
  output$data_table <- DT::renderDataTable(
    dt_setup(filtered_dataset(), n_rows = 20, dom_opt = "ftlip", show_rownames = TRUE)
  )

  mod_histogramServer("hist")
  
  mod_scatterplot_server("scatter", dataset)
  
  observeEvent(input$browser, browser())
  
  observeEvent(input$filter_button, {
    select_factors <- paste0(input$nav_name, "-", "include_factors")
    filtered_meta <- dplyr::filter(
      metadata, 
      .data[[input$nav_name]] %in% input[[select_factors]]
    )
    selected_samples <- dplyr::pull(filtered_meta, sample_names)
    # we're working with a matrix so can't do dplyr
    matrix_columns <- colnames(filtered_dataset()) %in% selected_samples
    filt <- filtered_dataset()[, matrix_columns]
    filtered_dataset(filt) # set reactiveVal
    
  })
  
  output$filter_summary <- renderText({
    "The filtering works on a very basic level. It only works for the include, not 
    the filter by value, and there are no validity checks. 
    It'll need update functions for the set of available and relevant conditions."
  })
  
  observeEvent(input$clear_filters, {
    filtered_dataset(dataset)
  })
  
  
}
