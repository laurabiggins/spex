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
          width = 4,
          tabsetPanel(
            id = ns("plot_samples"),
            tabPanel(
              title = "single",
              br(),
              selectInput(
                ns("x_axis"), 
                label = "x axis", 
                choices = individual_samples
              ),
              selectInput(
                ns("y_axis"), 
                label = "y axis", 
                choices = individual_samples, 
                selected = individual_samples[2]
              )
            ),
            tabPanel(
              title = "condition",
              br(),
              selectInput(
                inputId = ns("select_condition"),
                label = "",
                choices = names(meta_sum)
              ),
              br(),
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
              actionButton(ns("browser"), "browser")
            )
          )  
         # actionButton(ns("browser"), "browser")
        ),
        mainPanel(
          #width = 8,
          plotOutput(ns("plot"), width = "100%")#, height = "100%")
        )
      )  
    )
  )
}
    
#' scatterplot Server Function
#'
#' @noRd 
mod_scatterplot_server <- function(id, dataset, meta_sum, metadata, sample_name_col, prefix = "", session) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$select_condition, {
      
      # does this need checking each time or can we extract it somewhere?
      assertthat::see_if(
        assertthat::has_name(
          meta_sum[[input$select_condition]],
          input$select_condition
        ),  
        msg = paste0("couldn't find the column ", input$select_condition, 
                    " in meta_sum[[input$`scatter-select_condition`]]")
      )
      opts <- meta_sum[[input$select_condition]][[input$select_condition]]
      assertthat::see_if(
        length(opts) >= 2, 
        msg = paste0("number of factors in selected option for scatterplot is only ", 
                     length(opts), 
                     " so will not work well in a scatterplot."
              )
      )  
      updateSelectInput(
        inputId = "x_axis_multi", 
        choices = opts, 
        session = session
      )
      updateSelectInput(
        inputId = "y_axis_multi", 
        choices = opts, 
        session = session,
        selected = opts[2]
      )
      
    })

    # this doesn't need to be reactive
    tibble_dataset <- reactive({
      tib_data <- tibble::as_tibble(dataset, rownames = "row_attribute")
      tidyr::pivot_longer(tib_data, !row_attribute, names_to = sample_name_col)
    })
    
    # the set of selected samples
    selected_data <- reactive({
      
      if(input$plot_samples == "single") {
        return(list(
          dataset = get_single_data_samples(dataset, input$x_axis, input$y_axis),
          x_axis = input$x_axis, 
          y_axis = input$y_axis
        ))
      } else if (input$plot_samples == "condition"){

          filt_meta <- dplyr::filter(
            metadata, 
            .data[[input$select_condition]] %in% c(input$x_axis_multi, input$y_axis_multi)
          )
          filt_meta <- dplyr::select(filt_meta, c(input$select_condition, sample_name_col)) 
          joined_meta <- dplyr::inner_join(filt_meta, tibble_dataset())
          grouped <- dplyr::group_by(joined_meta, .data[[input$select_condition]], row_attribute) 
          summarised <- dplyr::ungroup(dplyr::summarise(grouped, mean_val = mean(value)))
          
          meta_wider <- tidyr::pivot_wider(
            summarised, 
            names_from = input$select_condition, 
            values_from = mean_val
          )

          list(
            dataset = meta_wider,
            x_axis = input$x_axis_multi,
            y_axis = input$y_axis_multi
          ) 
      } else {
          print("oops, should have been single or condition in scatter selection")
      }
    })
    
    output$plot <- renderPlot({
      scatter(selected_data())
    })

    observeEvent(input$browser, browser())
  })
}


scatter <- function(selected_data) {

  # assertthat::assert_that(
  #   is.character(x1), 
  #   msg = "character value for for x axis selection required"
  # )
  # assertthat::assert_that(
  #   is.character(y1), 
  #   msg = "character value for for y axis selection required"
  # )
  
  dataset <- selected_data$dataset
  x_var <- selected_data$x_axis
  y_var <- selected_data$y_axis
  
  req(x_var %in% colnames(dataset))
  req(y_var %in% colnames(dataset))
  
  dataset %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
    ) +
    ggplot2::geom_point() #+#size = input$point_size) +
    #ggplot2::geom_boxplot() #+
    #ggplot2::geom_abline(slope = 1, colour = "#3cc1f2")
}


#' get_single_data_samples
#' 
#' @param dataset data matrix 
#' @param x1 single sample to show on x axis
#' @param y1 single sample to show on y axis
#'
#' @noRd 
get_single_data_samples <- function(dataset, x1, y1){

  assertthat::assert_that(
    is.matrix(dataset),
    msg = "dataset passed to mod_scatterplot must be a matrix"
  )
  assertthat::assert_that(
    assertthat::has_attr(dataset, "dimnames"),
    msg = "dataset must have rownames"
  )

  tibble::as_tibble(dataset[,c(x1,y1)])
}
