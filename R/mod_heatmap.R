## If this doesn't show up straightaway in the app, try dev.off() in the console.

#' histogram UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_heatmap_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(
      id = ns("panel"),
      fluidRow(
        column(2, downloadButton(ns("download_png"), "png")),
        column(2, downloadButton(ns("download_pdf"), "pdf")),
        column(3, 
          selectInput(
            inputId = ns("selected_set"),
            label = "select set",
            choices = ""
          )
        ),
        column(3, offset = 2, numericInput(ns("plot_height"), "plot height", 500))
      ),
      shinycssloaders::withSpinner(
        plotOutput(ns("plot"), width = "100%", height = "500"), 
        image = "bioinf1.gif", 
        image.width = 100
      )
    ),
    br(),

    br(),
    br(),
    actionButton(inputId = ns("browser"), "browser"), 
    
    #if we want the downloaded plot to be the window size
    # for height we make the user adjust it, as it won't resize with the window
    tags$script(
      "var myWidth = 0;
      $(document).on('shiny:connected', function(event) {
        myWidth = $(window).width();
        Shiny.onInputChange('heatmap-shiny_width', myWidth);
      });
      $(window).resize(function(event) {
         myWidth = $(window).width();
         Shiny.onInputChange('heatmap-shiny_width', myWidth);
      });"
    )
  )
}


#'
#' @noRd 
mod_heatmap_server <- function(id, dataset, metadata, of_interest,
                               sample_name_col, chosen_dataset, prefix = "", session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ## update select input ----
      observeEvent(of_interest(), {
        opts <- c("all", names(of_interest()))
        updateSelectInput(
          inputId = "selected_set", 
          choices = opts,
          selected = dplyr::last(opts)
        )
      })
      
      heatmap_height <- reactiveVal(500)
      
      observeEvent(input$plot_height, {
        validate(need(input$plot_height > 200, "plot height must be > 200"))
        heatmap_height(input$plot_height)
      })
      
      ## data reactive expressions ----
      # we could have tickboxes to select which samples are shown
      selected_data <- reactive({
        
        validate(need(dataset(), "Please load a dataset"))
        
        if(input$selected_set == "all") {
          return (dataset())
        } else {
          req(of_interest(), dataset())
          genes_of_interest <- of_interest()[[input$selected_set]][[1]]
          dataset()[rownames(dataset()) %in% genes_of_interest, ]
        }
      })
      
      heatmap_obj <- reactive({
        validate(
          need(
            !any(is.na(selected_data())),
            "Dataset contains NA or missing values so heatmap cannot be plotted."
          )
        )
        pheatmap::pheatmap(
          selected_data(),
          scale = "row",
          silent = TRUE
        )
      })

      ## renderPlot ----
      output$plot <- renderPlot({

        plot(heatmap_obj()$gtable)
      }, height = function(x) heatmap_height())

      ## download functions ----
      output$download_png <- downloadHandler(
        filename = function() {
          paste0("heatmap.png")
        },
        content = function(file) {
          req(heatmap_height())
          ggplot2::ggsave(
            file,
            heatmap_obj()$gtable,
            device = "png",
            width = input$shiny_width/4, ## 1pixel ~ 0.26mm at 96 dpi. it's ~0.35 at 72dpi
            #height = input$shiny_height/4,
            #width = input$plot_width*0.35,
            height = heatmap_height()*0.35,
            units = "mm"
          )
        }
      )

      output$download_pdf <- downloadHandler(
        filename = function() {
          paste0("heatmap.pdf")
        },
        content = function(file) {
          ggplot2::ggsave(
            file,
            heatmap_obj(),
            device = "pdf",
            width = input$shiny_width/4,
            #width = input$plot_width*0.35,
            height = heatmap_height()*0.35,
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
