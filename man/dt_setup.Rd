% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_helpers.R
\name{dt_setup}
\alias{dt_setup}
\title{Set DT::datatable options}
\usage{
dt_setup(
  data,
  n_rows = 10,
  lineHeight = "80\%",
  dom_opt = "tip",
  cols_to_round = NULL,
  dt_options = NULL,
  sig_digits = 3,
  regex = FALSE,
  selection = "single",
  table_lengths = c(10, 20, 50, 100),
  filter_pos = "none",
  show_rownames = FALSE
)
}
\arguments{
\item{data}{A tibble or dataframe.}

\item{n_rows}{The number of rows to display (integer).}

\item{lineHeight}{The height of each row in table (percentage). Default "80\%".}

\item{dom_opt}{dom options for table components:
\itemize{
\item l - length changing input control
\item f - filtering input
\item t - table
\item i - table info
\item p - pagination
Any combination of these options should be supplied as a single character
string and will be displayed in the order supplied.
Default: "tip" (table, info, pagination).
}}

\item{cols_to_round}{columns to round (vector of column numbers).
See \code{sig_digits}. Default: NULL}

\item{dt_options}{list of options to supply to \code{datatable()}.
The default is NULL, meaning options are taken from the relevant supplied parameters,
i.e. \code{dom}, \code{pageLength = n_rows}, \code{lengthMenu = table_lengths}.
This argument is for if further customisation is required.}

\item{sig_digits}{The number of significant digits to round to if \code{cols_to_round}
is not NULL. Default: 3}

\item{regex}{If \code{TRUE}, this allows regex searchng of the table e.g. gene1 | gene2.}

\item{selection}{Whether to allow multiple rows to be selected at once, or just a
single row. Either \code{"multiple"} or \code{"single"}. Default: \code{"single"}}

\item{table_lengths}{Integer vector of available table lengths.
Default: c(10,20,50,100). This will only show if dom_opt includes "l".}

\item{filter_pos}{Location of column filters. One of c("none", "bottom", "top").
Default: "none"}

\item{show_rownames}{Whether to show rownames or not. Default: FALSE}
}
\value{
DT::datatable() object
}
\description{
Set DT::datatable options
}
\examples{
dt_setup(iris)
}
