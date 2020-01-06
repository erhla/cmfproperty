# df <- data.table::fread("E:/report data/Buffalo/joined.csv")
# ratios <- reformat_data(df, "Sale Price", "total_assessment", "Sale_Year", TRUE, "full_market_value")
# calc_iaao_stats(ratios)

calc_ratio_and_mark_arms_length <- function(df) {
  df <-
    df %>% dplyr::mutate(RATIO = ifelse(
      !is.na(SALE_PRICE),
      ifelse(SALE_PRICE > 100, ASSESSED_VALUE / SALE_PRICE, NA),
      NA
    ))
  df <- df %>% dplyr::group_by(TAX_YEAR) %>%
    dplyr::mutate(arms_length_transaction = ifelse(!is.na(RATIO),
                                                   ifelse((
                                                     RATIO > quantile(RATIO, na.rm = TRUE)[[4]] + 1.5 * IQR(RATIO, na.rm = TRUE)
                                                   ) |
                                                     (
                                                       RATIO < quantile(RATIO, na.rm = TRUE)[[2]] - 1.5 * IQR(RATIO, na.rm = TRUE)
                                                     ),
                                                   0,
                                                   1
                                                   ), NA))
  return(df)
}

#' Reformats data for analysis by changing column names and filtering out non-arm's length sales.
#'
#' @param df a dataframe with assessment, sales, and time data
#' @param sale_col the name of the column with sales data
#' @param assessment_col the name of the column with assessment data
#' @param sale_year_col the name of the column with the year of sale
#' @param filter_data if True, keeps only arm's length sales. if False, keeps all data.
#' @param market_value_col optional, the name of the column with market value data
#' @param tax_year_col optional, the name of the column with tax year data. The default is to set to the sale year.
#'
#'@return a dataframe with appropriate column names and arm's length markings
reformat_data <-
  function(df, sale_col, assessment_col, sale_year_col, filter_data,
           market_value_col = NULL, tax_year_col = NULL) {

    #rename columns
    df[["SALE_PRICE"]] <- df[[sale_col]]
    df[["ASSESSED_VALUE"]] <- df[[assessment_col]]
    df[["SALE_YEAR"]] <- df[[sale_year_col]]
    if (!is.null(tax_year_col)) {
      df[["TAX_YEAR"]] <- df[[tax_year_col]]
    } else {
      df[["TAX_YEAR"]] <- df[[sale_year_col]]
    }
    if (!is.null(market_value_col)) {
      df[["MARKET_VALUE"]] <- df[[market_value_col]]
    }

    #arm's length
    df <- calc_ratio_and_mark_arms_length(df)
    if(keep_ratios_only){
      df <- df %>% dplyr::filter(arms_length_transaction == 1, !is.na(RATIO))
    }
    return(df)
  }
