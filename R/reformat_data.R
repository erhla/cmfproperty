#' Reformats data for analysis by changing column names and filtering out non-arm's length sales.
#'
#' @param df a dataframe with assessment, sales, and time data
#' @param sale_col the name of the column with sales data
#' @param assessment_col the name of the column with assessment data
#' @param sale_year_col the name of the column with the year of sale
#' @param filter_data optional, default True. if True, keeps only arm's length sales. if False, keeps all data.
#'
#' @return a dataframe with appropriate column names and arm's length markings

#' @importFrom magrittr %>%

#' @export
reformat_data <-
  function(df,
           sale_col,
           assessment_col,
           sale_year_col,
           filter_data = TRUE) {
    # rename columns
    df <- col_rename_helper(sale_col, "SALE_PRICE")
    df <- col_rename_helper(assessment_col, "ASSESSED_VALUE")
    df <- col_rename_helper(sale_year_col, "SALE_YEAR")

    # create Tax Year
    df[["TAX_YEAR"]] <- df[["SALE_YEAR"]]

    # add ratio
    df <-
      df %>% dplyr::mutate(RATIO = ifelse(
        !is.na(.data$SALE_PRICE),
        ifelse(.data$SALE_PRICE > 100, .data$ASSESSED_VALUE / .data$SALE_PRICE, NA),
        NA
      ))
    # add arm's length
    df <-
      df %>% dplyr::group_by(.data$SALE_YEAR) %>%
      dplyr::mutate(arms_length_transaction = ifelse(!is.na(.data$RATIO), ifelse(
        (.data$RATIO > stats::quantile(.data$RATIO, na.rm = TRUE)[[4]] + 1.5 * stats::IQR(.data$RATIO, na.rm = TRUE)) |
        (.data$RATIO < stats::quantile(.data$RATIO, na.rm = TRUE)[[2]] - 1.5 * stats::IQR(.data$RATIO, na.rm = TRUE)),
      0, 1),
      NA))

    if (filter_data) {
      df <-
        df %>% dplyr::filter(.data$arms_length_transaction == 1,!is.na(.data$RATIO))
      print("Filtered out non-arm's length transactions")
    }
    #adjust for inflation
    df <- adj_for_inflation(df)
    return(df %>% as.data.frame())
  }


col_rename_helper <- function(current, rename_to){
  if((rename_to %in% names(df)) & (current != rename_to)){
    names(df)[names(df) == rename_to] <- paste0(rename_to, "_2")
    print(paste0("Renaming already present column '", rename_to, "' to '", rename_to, "_2'."))
  }
  names(df)[names(df) == current] <- rename_to
  return(df)
}

