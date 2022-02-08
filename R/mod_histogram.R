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
mod_histogramUI <- function(id, plot_height = 400){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(
      id = ns("panel"), 
      shinycssloaders::withSpinner(
        plotOutput(ns("plot"), width = "100%", height = plot_height), 
        image = "images/bioinf1.gif", 
        image.width = 100
      ),
      selectInput(
        inputId = ns("select_variable"),
        label = "select variable",
        choices = ""
      ),
      checkboxInput(inputId = ns("show_legend"), label = "display legend", value = TRUE)
    ),
    #actionButton(ns("browser"), "browser"),
    # so that the saved plot is the same size as the plot on the screen
    tags$script(
      "var myWidth = 0;
      $(document).on('shiny:connected', function(event) {
        myWidth = $(window).width();
        Shiny.onInputChange('hist-shiny_width', myWidth);
      });
      $(window).resize(function(event) {
         myWidth = $(window).width();
         Shiny.onInputChange('hist-shiny_width', myWidth);
      });"
    )
  )
}

    
#' histogram Server Function
#'
#' @noRd 
#mod_histogramServer <- function(id, data_to_plot, chosen_dataset, meta, prefix = "") {
mod_histogramServer <- function(id, data_to_plot, variables, prefix = "") {
  moduleServer(id, function(input, output, session) {
      
    observeEvent(input$browser, browser())

    observeEvent(variables(), {
      updateSelectInput(inputId = "select_variable", choices = sort(variables()))
    })
    
    density_plot_obj <- reactive({
      req(input$select_variable)
      n_to_plot <- length(unique(data_to_plot()[[input$select_variable]]))
      density_plot(data_to_plot(), input$select_variable, n_to_plot, input$show_legend)
    })
    
    output$plot <- renderPlot({
      density_plot_obj()
    }) %>% bindCache(input$select_variable, variables(), input$show_legend)
        
    output$download_png <- downloadHandler(
      filename = function() {
        paste0("density.png")
      },
      content = function(file) {
        ggplot2::ggsave(
          file, 
          density_plot_obj(), 
          device = "png", 
          width = input$shiny_width*0.75/4,
          units = "mm"
        )
      }
    )
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("density.pdf")
      },
      content = function(file) {
        ggplot2::ggsave(
          file, 
          density_plot_obj(), 
          device = "pdf", 
          width = input$shiny_width*0.75/4,
          units = "mm"
        )
      }
    )
  })
}

density_plot <- function(plotting_data, condition, n_samples, show_legend){
 
  req(condition %in% colnames(plotting_data))
  
  my_colours <- grDevices::colorRampPalette(c("#530c82", "#b9c9c9", "#024f4b"))(n_samples)
  
  p <- ggplot2::ggplot(plotting_data, ggplot2::aes(x = value, fill = .data[[condition]])) +
    ggplot2::geom_density(alpha = 0.5, size = 1) +
    #ggplot2::ggtitle("\nDistribution of data values\n") +
    ggplot2::scale_fill_manual(values = my_colours) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.text  = ggplot2::element_text(size = 8),
      axis.title   = ggplot2::element_text(size = 20),
      axis.text    = ggplot2::element_text(size = 14),
     # title        = ggplot2::element_text(size = 22),
      legend.spacing.x = ggplot2::unit(0.2, 'cm')
    )
  if(show_legend) p 
  else p + ggplot2::theme(legend.position="none")
  
}
