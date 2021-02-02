#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 

# TODO: work out how to import the data properly - it's currently in inst and loaded
# automatically, then called in the ui and server separately.

app_ui <- function(request) {
  
  metadata <- golem::get_golem_options("metadata")
  meta_sum <- get_condition_summary(metadata)
  samples <- get_all_sample_names(metadata)
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      titlePanel("Dataset title"),
      tabsetPanel(
        tabPanel(
          "metadata",
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              checkboxInput("show_meta", "show all metadata", value = TRUE),
                # checkboxInput("show_meta_summary", "show metadata summary"),
                # conditionalPanel(condition = "input.show_meta_summary == 1",
                #                  selectInput("selected_condition", 
                #                              "select condition",
                #                              #choices = meta_sum)
                #                              choices = names(meta_sum)),
                # ),
              actionButton("browser", "browser")
            ),
            mainPanel(
              width = 9,
              conditionalPanel(
                condition = "input.show_meta_summary == 1",
                tableOutput("meta_summary")
              ),
              conditionalPanel(
                condition = "input.show_meta == 1", 
                DT::dataTableOutput("meta_table")
              )
            )
          )
        ),
        tabPanel(
          "data",
          br(),
          DT::dataTableOutput("data_table")
        ),
        tabPanel(
          "plot",
          navlistPanel(
            "plot type",
            tabPanel("scatterplot", mod_scatterplot_ui("scatter", samples, meta_sum)),
            tabPanel("boxplot", mod_boxplot_ui("boxplot", samples, meta_sum)),
            tabPanel("histogram", mod_histogramUI("hist")),
            widths = c(3,9)
          )
        ),
        tabPanel(
          "filter",
          wellPanel(
            style = "margin: 10px 0px",
            h3("Filter summary: "),
            textOutput("filter_summary"),
            actionButton("browser", "browser")
          ),
          navlistPanel(
            id = "nav_name",
            well = FALSE,
            widths = c(3,9)
          ),
          wellPanel(
            fluidRow(
              column(
                width = 4, 
                offset = 2,
                align = "center",
                actionButton("filter_button", "Filter data")
              ),
              column(
                width = 4,
                align = "center",
                actionButton("clear_filters", "Clear all filters")
              )  
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')#, package = "spex")
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'spex'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

