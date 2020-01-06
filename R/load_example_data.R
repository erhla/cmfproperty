#' Loads example data
#'
#' @return a dataframe

#' @export
load_example_data <- function() {
  df <- utils::read.csv("data-raw/example_data.csv")
  df <-
    cmfproperty::reformat_data(df, "SALE_PRICE", "ASSESSED_VALUE",
                             "SALE_YEAR", TRUE)
  return(df)
}
