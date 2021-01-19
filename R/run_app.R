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

  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(metadata = readRDS("inst/extdata/metaFem.rds"),
                      dataset  = readRDS("inst/extdata/femExpression.rds"),
                      sample_names = "condName")
  )
}


#library(golem); library(devtools)
#load_all()
