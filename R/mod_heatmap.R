#' histogram UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
# mod_histogram_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#  
#   )
# }
mod_heatmap_ui <- function(id, individual_samples, meta_sum){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(id = ns("panel"),
              plotOutput(ns("plot"), height = "500px"),
              
    ),
    br(),
    downloadButton(ns("download_png"), "png"),
    downloadButton(ns("download_pdf"), "pdf"),
    br(),
    br(),
    actionButton(inputId = ns("browser"), "browser"), 
#     tags$script("$(document).on('shiny:connected', function(event) {
# var myWidth = $(window).width();
# Shiny.onInputChange('heatmap-shiny_width',myWidth)
# 
# });"),
    tags$script("
                var myWidth = 0;
                $(document).on('shiny:connected', function(event) {
                  myWidth = $(window).width();
                  Shiny.onInputChange('heatmap-shiny_width', myWidth);
                });
                $(window).resize(function(event) {
                   myWidth = $(window).width();
                   Shiny.onInputChange('heatmap-shiny_width', myWidth);
                });
              "),
    tags$script("
                var myHeight = 0;
                $(document).on('shiny:connected', function(event) {
                  myHeight = $(window).height();
                  Shiny.onInputChange('heatmap-shiny_height', myHeight);
                });
                $(window).resize(function(event) {
                   myHeight = $(window).height();
                   Shiny.onInputChange('heatmap-shiny_height', myHeight);
                });
                ")
  )
}

    # "$(document).on('shiny:connected', function(event) {
    #   var myHeight = $(window).height();
    #   Shiny.onInputChange('heatmap-shiny_height', myHeight)
    #   
    # });



    
#' histogram Server Function
#'
#' @noRd 
mod_heatmap_server <- function(id, dataset, meta_sum, metadata, of_interest,
                               sample_name_col, prefix = "", session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      heatmap_options <- reactiveValues(
        annot_col = tibble::column_to_rownames(metadata , "sample_name"),
        annot_row = tibble::column_to_rownames(of_interest , "gene")
      )
      
      # this has to be in a reactive expression because the dataset 
      # passed to mod_heatmap_server is a reactive expression
      selected_data <- reactive({
        
        genes_of_interest <- dplyr::pull(of_interest, gene)
        filtered_meta <- dplyr::filter(metadata, type %in% c("AA", "DA", "OEA"))
        selected_samples <- dplyr::pull(filtered_meta, sample_name_col)
        # we're working with a matrix so can't do dplyr
        matrix_columns <- colnames(dataset) %in% selected_samples
        dataset[rownames(dataset) %in% genes_of_interest, matrix_columns]
      })
      
      heatmap_obj <- reactive({
        pheatmap::pheatmap(
          selected_data(),
          scale = "row",
          annotation_col = heatmap_options$annot_col,
          annotation_row = heatmap_options$annot_row,
          silent = TRUE
        )
      })
      
      output$plot <- renderPlot(plot(heatmap_obj()$gtable))
        
      output$download_png <- downloadHandler(
        filename = function() {
          paste0("heatmap.png")
        },
        content = function(file) {
          ggplot2::ggsave(
            file, 
            heatmap_obj(), 
            device = "png",
            width = input$shiny_width/4, ## 1pixel ~ 0.26mm at 96 dpi. it's ~0.35 at 72dpi
            height = input$shiny_height/4, 
            units = "mm"
          )
        }
      )
        
      observeEvent(input$browser, browser())
    }
  )
}

# option for scaling 
# if nrow > 100? just plot first 100/500 rows, and write an error message

# annot_col1 <- metadata %>%
#   select(sample_name, type) %>%
#   tibble::column_to_rownames("sample_name")

# cluster by correlation
# clustering_distance_rows = "correlation"
