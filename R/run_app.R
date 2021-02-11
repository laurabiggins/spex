#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  ...
) {
  #thematic::thematic_on(bg = "#1d305f", fg = "white")
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(metadata = readRDS("inst/extdata/metaFem_edited.rds"),
                      dataset  = readRDS("inst/extdata/femExpression2.rds"),
                      of_interest = readRDS("inst/extdata/of_interest2.rds"),
                      sample_names = "sample_name")
  )
}


#library(golem); library(devtools)
#load_all()
