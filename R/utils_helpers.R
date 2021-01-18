# dom l-length changing input control, f-filtering input, t-table, i-table info, p-pagination
# selection = 'single' or 'multiple'
#' Title
#'
#' @param data  tibble or dataframe 
#' @param n_rows number of rows to display (integer)
#' @param lineHeight height of each row in table (percentage)
#' @param dom_opt dom options for table components, single character string. l-length changing input control, f-filtering input, t-table, i-table info, p-pagination, they will be displayed in the order supplied. 
#' Default: "tip" (table, info, pagination)
#' @param cols_to_round columns to round (vector of column numbers). 
#' See sig_digits.  Default: NULL 
#' @param dt_options list of options to supply to datatable() Default: NULL, options
#' are taken from the relevant supplied parameters i.e. dom, pageLength = n_rows, 
#' lengthMenu = table_lengths. This argument is for if further customisation is required.
#' @param sig_digits  Number of significant digits to round the columns supplied in 
#' cols_to_round to default: 3
#' @param regex default: false. If TRUE, allows regex searchng of the table e.g. gene1 | gene2  
#' @param selection row selection, one of "multiple", "single", default: single
#' @param table_lengths integer vector of available table lengths. Default: c(10,20,50,100).
#' This will only show if dom_opt includes "l"
#' @param filter_pos One of c("none", "bottom", "top"), where to place column filters. Default: "none"
#'
#' @return DT::datatable() object
#' @export
#'
#' @examples
#' dt_setup(iris)
dt_setup <-  function(data, n_rows = 10, lineHeight="80%", dom_opt = "tip", 
                      cols_to_round = NULL, dt_options = NULL, sig_digits = 3, 
                      regex = FALSE, selection = "single", 
                      table_lengths = c(10,20,50,100), filter_pos = "none") {
  
  assertthat::assert_that(tibble::is_tibble(data) | base::is.data.frame(data),
                          msg = "data supplied to dt_setup must be a tibble or data frame")
  
  if(is.null(dt_options)){
    dt_options = list(dom = dom_opt, 
                      pageLength = n_rows,
                      lengthMenu = table_lengths)
  }
  
  if(regex){
    dt_options[["search"]] <- list(regex = TRUE, caseInsensitive = TRUE)
  }
  
  dt_table <- DT::datatable(data,
                rownames = FALSE,
                selection = selection,
                escape = FALSE,
                filter = filter_pos,
                options = dt_options
                
  ) %>%
    DT::formatStyle(0, target = 'row', lineHeight = lineHeight)
  
  if(!is.null(cols_to_round)){
    dt_table <- DT::formatRound(dt_table, cols_to_round, sig_digits)
  }  
  dt_table   
  
}
