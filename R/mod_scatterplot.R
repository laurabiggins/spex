#' scatterplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
# UI ----
mod_scatterplot_ui <- function(id){
  
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
            choices = ""
          ),
          selectInput(
            ns("x_axis"), 
            label = "x axis", 
            choices = ""
          ),
          selectInput(
            ns("y_axis"), 
            label = "y axis", 
            choices = "" 
          ),
          actionButton(ns("browser"), "browser"),
          #br(),
          br(),
          downloadButton(ns("download_png"), "png"),
          downloadButton(ns("download_pdf"), "pdf")
        ),
        mainPanel(
          width = 8,
          shinycssloaders::withSpinner(
            plotOutput(ns("plot"), width = "100%", height = 500), 
            image = "images/bioinf1.gif", 
            image.width = 100, image.height = 40
          )
        )
      ),
      br(),
      checkboxInput(inputId = "highlight_panel", label = "show highlight options"),
      conditionalPanel(
        condition = "input.highlight_panel == 1",
        wellPanel(
          fluidRow(
            column(
              width = 6, 
              selectInput(
                ns("set_to_highlight"),
                "choose set",
                choices = ""
              )
            ),
            column(
              width = 6, 
              checkboxInput(ns("highlight_genes"), label = "highlight set"),
              checkboxInput(inputId = ns("label_highlights"), label = "show labels")
            )
          )
        )
      )  
    )
  )
}

#' scatterplot server function
#' 
#'
#' @noRd 
#' 
# server ----
mod_scatterplot_server <- function(id, data_to_plot, metadata, sample_name_col, sets_of_interest, chosen_dataset, prefix = "", session) {
  
  moduleServer(id, function(input, output, session) {

    ## condition and axis dropdowns ----
    
    observeEvent(chosen_dataset(), {
      
      updateSelectInput(
        inputId = "select_condition",
        choices = sort(names(metadata()$meta_summary))
      )
    })
    
    x_y_choices <- reactive({
      get_choices(input$select_condition, metadata()$meta_summary)
    }) %>% bindCache(chosen_dataset(), input$select_condition)

    observeEvent(x_y_choices(), {
      req(length(x_y_choices()) >= 2)
      updateSelectInput(
        inputId = "x_axis",
        choices = x_y_choices(),
        session = session
      )
      updateSelectInput(
        inputId = "y_axis",
        choices = x_y_choices(),
        session = session,
        selected = x_y_choices()[2]
      )
    })

  ## plotting data reactives ----
    
    selected_data <- reactive({
      req(data_to_plot())
      select_by_group(
        tibble_dataset = data_to_plot(),
        condition = input$select_condition,
        sample_name_col = sample_name_col,
        x_var = input$x_axis,
        y_var = input$y_axis
      )
    })
    
    scatter_plot_object <- reactive({
      req(selected_data())
      scatter(selected_data(), points_to_highlight(), input$x_axis, input$y_axis, label_highlighted())
    })

    # Attempt to see whether the scatter plot could not be re-rendered each time the highlight option
    # was selected but I don't think there's a way to do it.
    # plotly proxy function is now available but I don't know whether that allows enought control to highlight specific points
    # scatter_base_obj <- reactive({
    #   req(selected_data())
    #   scatter_black(selected_data(), input$x_axis, input$y_axis)
    # })
    # 
    # scatter_highlight_obj <- reactive({
    #   req(points_to_highlight())
    #   scatter_highlight(selected_data(), points_to_highlight(), input$x_axis, input$y_axis) 
    # })
    # 
    # scatter_labelled <- reactive({
    #   scatter_highlight_obj() + ggplot2::geom_text(
    #     data = points_to_highlight(),
    #     ggplot2::aes(x = .data[[input$x_axis]], y = .data[[input$y_axis]], label = row_attribute),
    #     nudge_x = 1
    #   )
    # })
    # 
    # output$plot <- renderPlot({
    #   req(scatter_base_obj())
    #   if(label_highlighted()) scatter_labelled()
    #   else if(input$highlight_genes & isTruthy(input$set_to_highlight)) scatter_highlight_obj()
    #   else scatter_base_obj()
    # })  
         
    
    # output$plot ----
    output$plot <- renderPlot({
      req(scatter_plot_object())
      scatter_plot_object()
    }) %>% bindCache(
      input$select_condition,
      input$x_axis,
      input$y_axis,
      input$set_to_highlight,
      input$highlight_genes,
      chosen_dataset(),
      label_highlighted()
    )

    ## highlight sets ----    
    
    label_highlighted <- reactiveVal(FALSE)
    
    shinyjs::disable("label_highlights")
    
    points_to_highlight <- reactive({
      req(selected_data())
      if(input$highlight_genes & isTruthy(input$set_to_highlight)) {
        genes <- get_set_to_highlight(sets_of_interest(), input$set_to_highlight)
        return(dplyr::filter(selected_data(), row_attribute %in% genes))
      } else NULL
    })
    
    observeEvent(sets_of_interest(), {
      updateSelectInput(
        session, 
        "set_to_highlight", 
        choices = names(sets_of_interest())
      )
    })
    
    observeEvent(input$highlight_genes, {
      if(input$highlight_genes) {
        shinyjs::enable("label_highlights")
      } else {
        shinyjs::disable("label_highlights")
        updateCheckboxInput(session, "label_highlights", "show labels", value = FALSE)
      }  
    })
    
    observeEvent(input$label_highlights, label_highlighted(input$label_highlights))
    
## download functions ----
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

# other functions ----

#' scatter plot function
#'
#' @param dataset dataset in tibble format that should contain columns with the 
#' same names as x_var and y_var
#' @param x_var variable to plot on the x axis
#' @param y_var variable to plot on the x axis
#'
#' @return ggplot object
#' @noRd
scatter <- function(dataset, points_to_highlight, x_var, y_var, label_subset) {

  req(x_var %in% colnames(dataset))
  req(y_var %in% colnames(dataset))
  
  main_colour <- dplyr::if_else(is.null(points_to_highlight), "black", "grey")
  
  p <- ggplot2::ggplot(dataset, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
    ggplot2::geom_point(colour = main_colour) + 
    ggplot2::geom_abline(slope = 1, colour = "#3cc1f2") +
    ggplot2::theme(legend.position = "none") 
  
  if(!is.null(points_to_highlight)) {
    p <- p + ggplot2::geom_point(
      data = points_to_highlight, 
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]), 
      colour = "red"
      )
  }
  
  if(label_subset){
    p <- p + ggplot2::geom_text(
      data = points_to_highlight,
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], label = row_attribute),
      nudge_x = 1
    )
  }
  p
}

# scatter_black <- function(dataset, x_var, y_var) {
#   
#   req(x_var %in% colnames(dataset))
#   req(y_var %in% colnames(dataset))
#   
#   ggplot2::ggplot(dataset, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
#     ggplot2::geom_abline(slope = 1, colour = "#3cc1f2") +
#     ggplot2::geom_point(colour = "black") +
#     ggplot2::theme(legend.position = "none") 
# }
# 
# scatter_highlight <- function(dataset, points_to_highlight, x_var, y_var) {
#   
#   req(x_var %in% colnames(dataset))
#   req(y_var %in% colnames(dataset))
#   
#   p <- ggplot2::ggplot(dataset, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
#     ggplot2::geom_abline(slope = 1, colour = "#3cc1f2") +
#     ggplot2::geom_point(colour = "grey") +
#     ggplot2::theme(legend.position = "none")
#   
#   p + ggplot2::geom_point(
#     data = points_to_highlight, 
#     ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]), 
#     colour = "red"
#   )
# }

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

#' Get the set of names to highlight on the plot 
#'
#' Extracts a vector of names from a list, the list item can have multiple columns
#' e.g. separate classes within a set of genes, but only the first column will be
#' returned and any information in other columns will be ignored.
#'
#' @param sets list object containing the sets of interest
#' @param selected_set the selected set
#'
#' @return vector of names (genes/proteins etc)
#' @export
#'
#' @examples
get_set_to_highlight <- function(sets, selected_set){
  # check it's not null
  sets[[selected_set]][[1]]
}


#' select_by_group
#' 
#' select by group or sample
#' 
#' If samples are selected by group, so that there are multiple samples per group, 
#' the mean value for each measure within the group is calculated
#'
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
#' 
select_by_group <- function(tibble_dataset, condition, sample_name_col, x_var, y_var){

  selected_data <- dplyr::filter(tibble_dataset, .data[[condition]] %in% c(x_var, y_var))
  n_samples <- dplyr::n_distinct(selected_data[[sample_name_col]])
  
  if(n_samples < 2) { # | length(unique(selected_data[[sample_name_col]])) < 2) {
    print("only found 1 selected variable to plot on scatter")
    print(
      paste0(
        "n_samples = ", 
        n_samples, 
        "selected_data[[sample_name_col]] = ", 
        selected_data[[sample_name_col]]
        )
      )
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

