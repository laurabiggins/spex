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
  
  #metadata <- golem::get_golem_options("metadata")
  #of_interest <- golem::get_golem_options("of_interest")
  meta_sum <- get_condition_summary(metadata)
  samples <- get_all_sample_names(metadata)
  #meta_sum <- 1:4
  #samples <- 1:3
  #measure_names <- rownames(golem::get_golem_options("dataset"))
  bab_light_blue <- "#00aeef"
  bab_dark_blue <- "#1d305f"
  #thematic::thematic_on(bg = "#1d305f", fg = "white")
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    fluidPage(
      shinyFeedback::useShinyFeedback(),
      shinyjs::useShinyjs(),
      theme = bslib::bs_theme(
        bg = bab_dark_blue, 
        fg = "white", 
        primary = bab_light_blue,
        secondary = bab_light_blue
      ),
      titlePanel(
        tags$img(
          src = "bioinformatics_logo_square_small.png", 
          style="margin-top: -10px; padding-right:10px; padding-bottom:10px", 
          width = "70", 
          height = "70",
          align = "right"
          ),
        windowTitle="spex"
      ),
      br(),
      #titlePanel("Dataset title"),
      tabsetPanel(
        id = "main_panels",
        tabPanel(
          "info",
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 3,
              selectInput(
                inputId = "choose_dataset",
                label = "Choose dataset",
                choices = c("to be populated", 2, 3),
              ),
              p("Explore your chosen dataset by using the tabs above."),
              p("Sample names and experimental conditions are shown in the metadata section."),
              p("The data tab shows the whole dataset, which can be downloaded if required."),
              p("A range of plots can be viewed and downloaded to explore different aspects of the dataset.")
            ),
            mainPanel(
              br(),
              titlePanel(h1("dataset name", align = "center")),
              br(),
              h5("Information about the dataset and publication link"),
              br(),br(),br(),
              br(),br(),br(),br(),br(),br(),br(),
              h6("For more information about work carried out at the Babraham Institute
                 visit the", a(href= "https://www.babraham.ac.uk/", "website")),
              br(),br(),br(),br()
            )
          )  
        ),  
        tabPanel(
          "metadata",
          br(),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Dataset summary", align = "center", style="margin: 10px;"),
                textOutput("meta_info1"),
                textOutput("meta_info2"),
                h6("Number of categories in each variable:"),
                tableOutput("meta_info3"),
                checkboxInput("show_meta_summary", "show more information on variables"),
                conditionalPanel(
                  condition = "input.show_meta_summary == 1",
                  fluidRow(
                    column(
                      width = 6,
                      selectInput(
                        "selected_condition",
                        "select condition",
                        choices = names(meta_sum)
                      ),
                    ),
                    column(width = 6, tableOutput("meta_summary"))
                  )
                ),
                checkboxInput("show_meta", "show all metadata"),
                conditionalPanel(
                  condition = "input.show_meta == 1",
                  DT::dataTableOutput("meta_table")
                ),
                actionButton("browser", "browser")
              )
            ),
            column(
              width = 6,
              wellPanel(
                h3("Sets of interest", align = "center", style="margin: 10px;"),
                textOutput("set_info1"),
                h6("Number in each set:"),
                tableOutput("set_info2"),
                checkboxInput("show_sets", "show items in set"),
                conditionalPanel(
                  condition = "input.show_sets == 1",
                  fluidRow(
                    column(
                      width = 6,
                      selectInput(
                        "selected_set",
                        "select set",
                        choices = names(of_interest)
                      )
                    ),
                    column(width = 6, tableOutput("set_summary"))
                  )
                )
              )
            )  
          ),
          br(),
          br()
        ),
        tabPanel(
          "data",
          br(),
          wellPanel(DT::dataTableOutput("data_table"))
        ),
        tabPanel(
          "plot",
          br(),
          navlistPanel(
            "plot type",
            tabPanel("histogram", mod_histogramUI("hist", meta_sum)),
            tabPanel("scatterplot", mod_scatterplot_ui("scatter", samples, meta_sum, of_interest)),
            tabPanel("heatmap", mod_heatmap_ui("heatmap", samples, meta_sum)),
            tabPanel("violinplot", mod_violinplot_ui("violinplot", samples, meta_sum)),
            #tabPanel("boxplot", mod_boxplot_ui("boxplot", samples, meta_sum)),
            widths = c(3,9)
          )
        ),
        tabPanel(
          "filter",
          br(),
          mod_name_filter_ui("name_filter", measure_names)
        )
      ),
      br(),
      fluidRow(
        column(
          width = 3,
          tags$img(src = "bioinformatics_logo_small_grey.png", width = "200", height = "71")
        ),
        column(
          width = 6,
          offset = 3,
          br(),
          br(),
          p("Any problems please email laura.biggins@babraham.ac.uk", style = "font-size:12px", align = "right")
        )  
      ),
      br()
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

