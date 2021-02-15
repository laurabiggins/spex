#' scatterplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scatterplot_ui <- function(id, individual_samples, meta_sum){
  
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
            inputId = ns("select_condition"),
            label = "select variable",
            choices = names(meta_sum)
          ),
          selectInput(
            ns("x_axis_multi"), 
            label = "x axis", 
            choices = ""
          ),
          selectInput(
            ns("y_axis_multi"), 
            label = "y axis", 
            choices = "" 
          ),
          actionButton(ns("browser"), "browser"),
          actionButton(ns("plot_button"), "Update plot"),
          br(),
          br(),
          downloadButton(ns("download_png"), "png"),
          downloadButton(ns("download_pdf"), "pdf")
        ),
        mainPanel(
          width = 8,
          shinycssloaders::withSpinner(plotOutput(ns("plot"), width = "100%"), image = "bioinf1.gif", image.width = 100)
          #plotOutput(ns("plot"), width = "100%")#, height = "100%")
        )
      ),
      checkboxInput(ns("highlight_genes"), label = "highlight measure of interest"),
      checkboxInput(inputId = "highlight_panel", label = "show highlight options"),
      conditionalPanel(
        condition = "input.highlight_panel == 1",
        wellPanel(
          shinyWidgets::pickerInput(
            inputId = ns("measure_selector"),
            label = "Select measure",
            choices = "",
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE, 
              selectedTextFormat = "count > 10"
            )
          ),
          actionButton(inputId = ns("highlight_button"), "highlight on plot"),
          checkboxInput(inputId = ns("label_highlights"), label = "show labels")
        )
      )  
    )
  )
}

#' scatterplot server function
#' 
#' This is fairly neat but I don't know if it's the most efficient way of selecting 
#' and plotting. There are various combinations of reactives/observeEvent/eventReactive 
#' that could be used.
#'
#' @noRd 
mod_scatterplot_server <- function(id, dataset, meta_sum, metadata, sample_name_col, of_interest, prefix = "", session) {
  
  moduleServer(id, function(input, output, session) {
    
    # This breaks if it's not a reactive - I guess it's because it takes arguments
    # from the server function??
    tibble_dataset <- reactive(get_tibble_dataset(dataset, sample_name_col))
    
    options <- reactive(get_choices(input$select_condition, meta_sum))

    rv <- reactiveValues(label_highlighted = FALSE)
    
    # this needs to be made more generic
    genes_of_interest <- dplyr::pull(of_interest[[1]], gene)
    
    observeEvent(input$label_highlights, {
      rv$label_highlighted <- input$label_highlights
    })
    
    observeEvent(input$select_condition, {
      req(options())
      updateSelectInput(
        inputId = "x_axis_multi",
        choices = options(),
        session = session
      )
      updateSelectInput(
        inputId = "y_axis_multi",
        choices = options(),
        session = session,
        selected = options()[2]
      )
    })

    observeEvent(input$plot_button, {
      
      rv$selected_data <- selected_data()

      shinyWidgets::updatePickerInput(
        session,
        inputId = "measure_selector",
        choices = rv$selected_data$row_attribute)
      
    })
    
    observeEvent(input$highlight_genes, {
      
      req(rv$selected_data)
      
      if(input$highlight_genes){
        rv$selected_data <- dplyr::mutate(
          rv$selected_data,
          custom_colour = dplyr::if_else(
            row_attribute %in% genes_of_interest,
            "red",
            "grey"
          )
        )
      } else {
        rv$selected_data <- dplyr::mutate(rv$selected_data, custom_colour = "black")
      }  
    })
    
    observeEvent(input$highlight_button, {
      
      req(rv$selected_data)
      
      rv$selected_data <- dplyr::mutate(
        rv$selected_data,
        custom_colour = dplyr::if_else(
          row_attribute %in% input$measure_selector,
          "red",
          "grey"
        )
      )
    })
    
    # the set of selected samples
    selected_data <- reactive({
      
      dataset <- select_by_group(
                   metadata,
                   tibble_dataset(),
                   condition = input$select_condition,
                   sample_name_col = sample_name_col,
                   x_var = input$x_axis_multi,
                   y_var = input$y_axis_multi
                 )
      
      dplyr::mutate(dataset, custom_colour = "black")
    })

    scatter_plot_object <- reactive({
      
      req(rv$selected_data)
      
      scatter(
        rv$selected_data, 
        input$x_axis_multi, 
        input$y_axis_multi, 
        rv$label_highlighted
      )
      
    })
    
    output$plot <- renderPlot({
      req(rv$selected_data)
      scatter_plot_object()      
    })

    output$download_png <- downloadHandler(
      filename = function() {
        paste0("scatter.png")
      },
      content = function(file) {
        ggplot2::ggsave(file, scatter_plot_object(), device = "png")
      }
    )
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("scatter.pdf")
      },
      content = function(file) {
        ggplot2::ggsave(file, scatter_plot_object(), device = "pdf")
      }
    )
    
    observeEvent(input$browser, browser())
  })
}

#' scatter plot function
#'
#' @param dataset dataset in tibble format that should contain columns with the 
#' same names as x_var and y_var
#' @param x_var variable to plot on the x axis
#' @param y_var variable to plot on the x axis
#'
#' @return ggplot object
#' @noRd
scatter <- function(dataset, x_var, y_var, label_subset) {

  req(x_var %in% colnames(dataset))
  req(y_var %in% colnames(dataset))
  
  dataset <- dplyr::arrange(dataset, custom_colour)
  
  p <- dataset %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
    ) +
    ggplot2::geom_point(colour = dataset[["custom_colour"]]) +
    ggplot2::geom_abline(slope = 1, colour = "#3cc1f2") +
    ggplot2::theme(legend.position = "none") 
  
  if(label_subset){
    p <- p + ggplot2::geom_text(
      data = subset(dataset, custom_colour == "red"),
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], label = row_attribute),
      nudge_x = 1
      )
  }
  p
}

#' get_choices
#' 
#' Check which categories are available for a chosen condition.
#'
#' @param selected_condition 
#' @param meta_sum 
#'
#' @return
#' @export
#'
#' @noRd
get_choices <- function(selected_condition, meta_sum){
  
  assertthat::see_if(
    assertthat::has_name(
      meta_sum[[selected_condition]],
      selected_condition
    ),  
    msg = paste0("couldn't find the column ", selected_condition, 
                 " in meta_sum[[input$`scatter-select_condition`]]")
  )
  opts <- meta_sum[[selected_condition]][[selected_condition]]
  assertthat::see_if(
    length(opts) >= 2, 
    msg = paste0("number of factors in selected option for scatterplot is only ", 
                 length(opts), 
                 " so will not work well in a scatterplot."
    )
  )  
  opts
}

#' get_tibble_dataset
#' 
#' Convert the main matrix dataset into a tibble
#'
#' @param matrix_data main dataset
#' @param sample_name_col column name that contains all the sample names (should
#' be set in the initial golem options, then passed through as a function argument
#' to the module)
#'
#' @return tibble
get_tibble_dataset <- function(matrix_data, sample_name_col) {
  tib_data <- tibble::as_tibble(matrix_data, rownames = "row_attribute")
  tidyr::pivot_longer(tib_data, !row_attribute, names_to = sample_name_col)
}

#' select_by_group
#' 
#' select by group or sample
#' 
#' If samples are selected by group, so that there are multiple samples per group, 
#' the mean value for each measure within the group is calculated
#'
#' @param metadata the metadata tibble
#' @param tibble_dataset the main dataset in tibble format
#' @param condition the condition that we want to pull the groups from
#' @param sample_name_col the name of the column that contains all the sample names
#' @param x_var group 1 
#' @param y_var group 2
#'
#' @return
#' @export
#'
#' @examples
select_by_group <- function(metadata, tibble_dataset, condition, sample_name_col, x_var, y_var){
  
  selected_samples <- metadata %>%
    dplyr::filter(.data[[condition]] %in% c(x_var, y_var)) %>%
    dplyr::select(c(condition, sample_name_col))  
  
  n_samples <- dplyr::count(selected_samples)
  
  selected_data <- dplyr::inner_join(selected_samples, tibble_dataset)
  
  if(n_samples < 2 | length(unique(selected_data[[sample_name_col]])) < 2) {
    stop("only found 1 selected variable to plot on scatter")
  }
  # whether to group and summarise
  if(n_samples > 2) {
    selected_data <- selected_data %>%
      dplyr::group_by(.data[[condition]], row_attribute) %>%
      dplyr::summarise(mean_val = mean(value)) %>%
      dplyr::ungroup()
    
    return(tidyr::pivot_wider(selected_data, names_from = condition, values_from = mean_val))
  }
  # returned if n_samples == 2
  tidyr::pivot_wider(selected_data, names_from = condition, values_from = value)
}
