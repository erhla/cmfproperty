#' Reformats data for analysis by changing column names and filtering out non-arm's length sales.
#'
#' @param data a dataframe with assessment, sales, and time data
#' @param sale_col the name of the column with sales data
#' @param assessment_col the name of the column with assessment data
#' @param sale_year_col the name of the column with the year of sale
#' @param filter_data optional, default True. if True, keeps only arm's length sales. if False, keeps all data.
#'
#' @return a dataframe with appropriate column names and arm's length markings

#' @importFrom magrittr %>%

#' @export
reformat_data <-
  function(data,
           sale_col,
           assessment_col,
           sale_year_col,
           filter_data = TRUE) {
    # rename columns
    data <- col_rename_helper(data, sale_col, "SALE_PRICE")
    data <- col_rename_helper(data, assessment_col, "ASSESSED_VALUE")
    data <- col_rename_helper(data, sale_year_col, "SALE_YEAR")

    # create Tax Year
    data[["TAX_YEAR"]] <- data[["SALE_YEAR"]]

    # add ratio
    data <-
      data %>% dplyr::mutate(RATIO = ifelse(
        !is.na(.data$SALE_PRICE),
        ifelse(.data$SALE_PRICE > 100, .data$ASSESSED_VALUE / .data$SALE_PRICE, NA),
        NA
      ))
    # add arm's length
    data <-
      data %>% dplyr::group_by(.data$SALE_YEAR) %>%
      dplyr::mutate(arms_length_transaction = ifelse(!is.na(.data$RATIO), ifelse(
        (.data$RATIO > stats::quantile(.data$RATIO, na.rm = TRUE)[[4]] + 1.5 * stats::IQR(.data$RATIO, na.rm = TRUE)) |
        (.data$RATIO < stats::quantile(.data$RATIO, na.rm = TRUE)[[2]] - 1.5 * stats::IQR(.data$RATIO, na.rm = TRUE)),
      0, 1),
      NA))

    if (filter_data) {
      data <-
        data %>% dplyr::filter(.data$arms_length_transaction == 1,!is.na(.data$RATIO))
      print("Filtered out non-arm's length transactions")
    }
    #adjust for inflation
    data <- adj_for_inflation(data)
    return(data %>% as.data.frame())
  }


col_rename_helper <- function(data, current, rename_to){
  if(is.element(rename_to, names(data)) & (current != rename_to)){
    names(data)[names(data) == rename_to] <- paste0(rename_to, "_2")
    print(paste0("Renaming already present column '", rename_to, "' to '", rename_to, "_2'."))
  }
  names(data)[names(data) == current] <- rename_to
  return(data)
}

