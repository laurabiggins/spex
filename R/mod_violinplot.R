#' violinplot  UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_violinplot_ui <- function(id, meta_sum){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(
      id = ns("panel"),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 4,
          selectInput(
            inputId = ns("select_condition"),
            label = "select variable",
            choices = sort(names(meta_sum)),
          ),
          br(),
          checkboxInput(ns("add_boxplot"), "add boxplot"),
          br(),
          downloadButton(ns("download_png"), "png"),
          downloadButton(ns("download_pdf"), "pdf")#,
          #actionButton(ns("browser"), "browser")
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
    tags$script(
      "var myWidth = 0;
      $(document).on('shiny:connected', function(event) {
        myWidth = $(window).width();
        Shiny.onInputChange('violinplot-shiny_width', myWidth);
      });
      $(window).resize(function(event) {
         myWidth = $(window).width();
         Shiny.onInputChange('violinplot-shiny_width', myWidth);
      });"
    )
  )
}
    
#' scatterplot Server Function
#'
#' @noRd 
mod_violinplot_server <- function(id, long_data_tib, sample_name_col, chosen_dataset, prefix = "", session) {
  
  moduleServer(id, function(input, output, session) {

    violin_obj <- reactive({
      violinplot(long_data_tib(), input$select_condition, boxplot = input$add_boxplot)
    })
    
    output$plot <- renderPlot({
      violin_obj()
    }) %>% bindCache(input$select_condition, input$add_boxplot, chosen_dataset)
    
    output$download_png <- downloadHandler(
      filename = function() {
        paste0("violin.png")
      },
      content = function(file) {
        ggplot2::ggsave(
          file, 
          violin_obj(), 
          device = "png", 
          width = input$shiny_width*0.75/4,
          units = "mm"
        )
      }
    )
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("violin.pdf")
      },
      content = function(file) {
        ggplot2::ggsave(
          file, 
          violin_obj(), 
          device = "pdf", 
          width = input$shiny_width*0.75/4,
          units = "mm"
        )
      }
    )
    observeEvent(input$browser, browser())
  })
}


#' violinplot
#'
#' @param dataset entire dataset 
#' @param condition selected condition to plot
#' @param boxplot whether to add a boxplot on top of the violin plot (TRUE or FALSE)
#'
#' @return
violinplot <- function(dataset, condition, boxplot = FALSE) {

  p <- dataset %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[condition]], y = value)) +
    ggplot2::geom_violin(fill = "#70b5aa")
  
  if(boxplot){
    p <- p + ggplot2::geom_boxplot(width=0.1, fill = "#92d1c7")
  }
  p
}
