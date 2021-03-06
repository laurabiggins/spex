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
mod_histogramUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(
      id = ns("panel"),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          style = "padding: 10px",
          width = 4,
          selectInput(
            inputId = ns("select_variable"),
            label = "select variable",
            choices = ""
          ),
          br(),
          textInput(ns("text"), label = NULL, value = "my piece of text"),
          downloadButton(ns("download_png"), "png"),
          downloadButton(ns("download_pdf"), "pdf"),
          actionButton(ns("browser"), "browser")
        ),
        mainPanel(
          width = 8,
          shinycssloaders::withSpinner(
            plotOutput(ns("plot"), width = "100%", height = 500), 
            image = "images/bioinf1.gif", 
            image.width = 100
          )
        ) 
      )  
    ),
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
mod_histogramServer <- function(id, data_to_plot, chosen_dataset, meta, prefix = "") {
  moduleServer(id, function(input, output, session) {
      
    observeEvent(input$browser, browser())

    observeEvent(chosen_dataset(), {

      new_choices <- sort(names(meta()$meta_summary))
      
      updateSelectInput(
        inputId = "select_variable",
        choices = new_choices
      )
      
      updateTextInput(
        inputId = "text", 
        value = paste0("dataset = ", chosen_dataset())
      )
    })
    
    density_plot_obj <- reactive({
      
      req(input$select_variable)
      
      n_to_plot <- length(unique(data_to_plot()[[input$select_variable]]))
      
      # assertthat::assert_that(
      #   ncol(dataset) == length(unique(data_to_plot()$sample_name)),
      #   msg = "unexpected number of samples for density plot"
      # )
      density_plot(data_to_plot(), input$select_variable, n_to_plot)
    })
    
    output$plot <- renderPlot({
      density_plot_obj()
    }) %>% bindCache(input$select_variable, chosen_dataset())

        
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

density_plot <- function(plotting_data, condition, n_samples){
 
  req(condition %in% colnames(plotting_data))
  
  my_colours <- grDevices::colorRampPalette(c("#530c82", "#b9c9c9", "#024f4b"))(n_samples)
  
  ggplot2::ggplot(plotting_data, ggplot2::aes(x = value, fill = .data[[condition]])) +
    ggplot2::geom_density(alpha = 0.5, size = 1) +
    ggplot2::ggtitle("\nDistribution of data values\n") +
    ggplot2::scale_fill_manual(values = my_colours) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.text  = ggplot2::element_text(size = 8),
      axis.title   = ggplot2::element_text(size = 20),
      axis.text    = ggplot2::element_text(size = 14),
      title        = ggplot2::element_text(size = 22),
      legend.spacing.x = ggplot2::unit(0.2, 'cm')
    )
}
