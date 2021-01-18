#' scatterplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scatterplot_ui <- function(id, samples_x){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(id = ns("panel"),
              sidebarLayout(position = "right",
                sidebarPanel(
                  selectInput(ns("x_axis"), 
                              label = "x axis", 
                              choices = samples_x),
                  selectInput(ns("y_axis"), 
                              label = "y axis", 
                              choices = samples_x, 
                              selected = samples_x[2]),
                 # actionButton(ns("browser"), "browser")
                ),
                mainPanel(
                  plotOutput(ns("plot"), width = "100%", height = "400px")
                )
              )  
    )
  )
}
    
#' scatterplot Server Function
#'
#' @noRd 
mod_scatterplot_server <- function(id, dataset, prefix = "") {
  
  moduleServer(id, function(input, output, session) {
      
      selected_data <- reactive({
        get_selected_data(dataset, input$x_axis, input$y_axis)
      })

      output$plot <- renderPlot(scatter(selected_data(), input$x_axis, input$y_axis))

      observeEvent(input$browser, browser())
  })
}


get_selected_data <- function(dataset, x1, y1){

  assertthat::assert_that(is.matrix(dataset), msg = "dataset must be a matrix")
  assertthat::assert_that(assertthat::has_attr(dataset, "dimnames"), 
                          msg = "dataset must have rownames")

  tibble::as_tibble(dataset[,c(x1,y1)])

}

scatter <- function(sel_data, x1, y1) {

  assertthat::assert_that(is.character(x1), 
                          msg = "character value for for x axis selection required")
  assertthat::assert_that(is.character(y1), 
                          msg = "character value for for y axis selection required")
  
  sel_data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[x1]],
                                 y = .data[[y1]])
    ) +
    ggplot2::geom_point()+#size = input$point_size) +
    ggplot2::geom_abline(slope = 1, colour = "#3cc1f2")
}

