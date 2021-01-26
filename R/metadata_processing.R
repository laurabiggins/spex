#' Summary of metadata conditions
#' 
#'
#' @param metadata dataframe or tibble of metadata
#'
#' @return list of tibbles each containing a condition type defined in the metadata,
#' and the number of instances of each condition.
#' @export
#'
#' @examples
#' get_condition_summary(metadata)
get_condition_summary <- function(metadata){
  
  conditions <- base::colnames(metadata)
  
  # base::sapply(conditions, simplify = FALSE, USE.NAMES = TRUE, function(x) {
  #    dplyr::count(metadata, .data[[x]])
  # })
  
   purrr::map(conditions, function(x){
    # browser()
     dplyr::count(metadata, .data[[x]])
  })
}

