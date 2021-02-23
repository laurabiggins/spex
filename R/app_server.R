# Downloading and saving a file in a chosen location.
# There's a chrome setting in Settings -> Advanced -> Downloads -> 
# Ask where to save each file before downloading
# so it doesn't seem to be a Shiny thing

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  data_values <- golem::get_golem_options("dataset")
  
  metadata <- golem::get_golem_options("metadata")
  meta_sum <- get_condition_summary(metadata)
  sample_names <- golem::get_golem_options("sample_names")
  of_interest <- golem::get_golem_options("of_interest")
  measure_names <- rownames(golem::get_golem_options("dataset"))
  #thematic::thematic_on(bg = "#1d305f", fg = "white")
  
  measures_of_interest <- reactiveVal(of_interest)
  
  # Data tab - the main dataset
  output$data_table <- DT::renderDataTable(
    dt_setup(data_values, n_rows = 20, dom_opt = "ftlip", show_rownames = TRUE)
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

  output$meta_table <- DT::renderDataTable(dt_setup(metadata, n_rows = 20))
  
  output$meta_summary <- renderTable({
    req(input$selected_condition)
    req(meta_sum)
    meta_sum[[input$selected_condition]]
  })
  

  mod_histogramServer("hist", data_values, metadata, sample_name_col = sample_names)
  
  mod_heatmap_server("heatmap", data_values, meta_sum, metadata, 
                     sample_name_col = sample_names, of_interest = of_interest)
  
  mod_scatterplot_server(
    "scatter", 
    data_values, 
    meta_sum, 
    metadata, 
    sample_name_col = sample_names, 
    sets_of_interest = measures_of_interest
  )
  
  mod_violinplot_server("violinplot", data_values, meta_sum, metadata, 
                        sample_name_col = sample_names)
  
  mod_boxplot_server("boxplot", data_values, meta_sum, metadata, 
                     sample_name_col = sample_names)
  
  filter_results <- mod_name_filter_server("name_filter", measure_names, of_interest)
  
  observeEvent(filter_results(), {
    
    measures_of_interest(filter_results())
    print("filter results updated")
    updateSelectInput(inputId = "selected_set", choices = names(measures_of_interest()))
  })
  
  observeEvent(input$browser, browser())

}
