#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  dataset <- golem::get_golem_options("dataset")
  
  metadata <- golem::get_golem_options("metadata")
  meta_sum <- get_condition_summary(metadata)
  sample_names <- golem::get_golem_options("sample_names")
  of_interest <- golem::get_golem_options("of_interest")
  measure_names <- rownames(golem::get_golem_options("dataset"))
  #thematic::thematic_on(bg = "#1d305f", fg = "white")
  
  measures_of_interest <- reactiveVal(of_interest)
  
  # Data tab - the main dataset
  output$data_table <- DT::renderDataTable(
    dt_setup(filtered_dataset(), n_rows = 20, dom_opt = "ftlip", show_rownames = TRUE)
  )
  
  
  # Filter tab to filter by condition. Commenting this out for now, but may want 
  # to reinstate at some point
  # lapply(meta_sum, function(x) {
  #   appendTab(
  #     inputId = "nav_name", 
  #     mod_filter_panel_ui(id = names(x)[1], meta_field = x[[1]]))
  # })
  
  output$meta_info1 <- renderText({
    paste0("The dataset contains ", nrow(meta_sum[[sample_names]]), " samples.")
  })
  
  output$meta_info2 <- renderText({
    paste0("Variables are: ", 
           paste0(names(meta_sum)[!names(meta_sum) %in% sample_names], collapse = ", "),
           "."
    )
  })
  
  output$meta_info3 <- renderTable({
    tibble::enframe(sapply(meta_sum, nrow))
  }, colnames = FALSE)
  
  output$set_info1 <- renderText({
    n_sets <- length(measures_of_interest())
    if(n_sets == 1) text <- " set available"
    else text <- " sets available"
    paste0(n_sets, text, ". To add more, use the filter tab.")
  })  
  
  output$set_info2 <- renderTable({
    tibble::enframe(sapply(measures_of_interest(), nrow))
  }, colnames = FALSE)
  
  output$set_summary <- renderTable({
    req(input$selected_set)
    req(measures_of_interest())
    measures_of_interest()[[input$selected_set]]
  })

  filtered_dataset <- reactiveVal(dataset)
  
  output$meta_table <- DT::renderDataTable(dt_setup(metadata, n_rows = 20))
  
  #output$meta_table <- DT::renderDataTable(metadata, style = "bootstrap4")
  
  output$meta_summary <- renderTable({
    req(input$selected_condition)
    req(meta_sum)
    meta_sum[[input$selected_condition]]
  })
  


  mod_histogramServer("hist", filtered_dataset(), metadata, sample_name_col = sample_names)
  
  mod_heatmap_server("heatmap", filtered_dataset(), meta_sum, metadata, 
                     sample_name_col = sample_names, of_interest = of_interest)
  
  mod_scatterplot_server(
    "scatter", 
    filtered_dataset(), 
    meta_sum, 
    metadata, 
    sample_name_col = sample_names, 
    sets_of_interest = measures_of_interest
  )
  
  mod_violinplot_server("violinplot", filtered_dataset(), meta_sum, metadata, 
                        sample_name_col = sample_names)
  
  mod_boxplot_server("boxplot", filtered_dataset(), meta_sum, metadata, 
                     sample_name_col = sample_names)
  
  filter_results <- mod_name_filter_server("name_filter", measure_names, of_interest)
  
  observeEvent(filter_results(), {
    
    measures_of_interest(filter_results())
    print("filter results updated")
    updateSelectInput(inputId = "selected_set", choices = names(measures_of_interest()))
  })
  
  observeEvent(input$browser, browser())
  
  observeEvent(input$filter_button, {
    
    print(filter_results())
    
    # select_factors <- paste0(input$nav_name, "-", "include_factors")
    # filtered_meta <- dplyr::filter(
    #   metadata, 
    #   .data[[input$nav_name]] %in% input[[select_factors]]
    # )
    # selected_samples <- dplyr::pull(filtered_meta, sample_names)
    # # we're working with a matrix so can't do dplyr
    # matrix_columns <- colnames(filtered_dataset()) %in% selected_samples
    # filt <- filtered_dataset()[, matrix_columns]
    # filtered_dataset(filt) # set reactiveVal
    
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
