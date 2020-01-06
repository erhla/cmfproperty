# df <- data.table::fread("E:/report data/Buffalo/joined.csv")
# ratios <- reformat_data(df, "Sale Price", "total_assessment", "Sale_Year", TRUE, "full_market_value")
# calc_iaao_stats(ratios)
#' @import magrittr
calc_ratio_and_mark_arms_length <- function(df) {
  df <-
    df %>% dplyr::mutate(RATIO = ifelse(
      !is.na(SALE_PRICE),
      ifelse(SALE_PRICE > 100, ASSESSED_VALUE / SALE_PRICE, NA),
      NA
    ))
  df <- df %>% dplyr::group_by(TAX_YEAR) %>%
    dplyr::mutate(arms_length_transaction =
                    ifelse(!is.na(RATIO), ifelse((RATIO > stats::quantile(RATIO, na.rm = TRUE)[[4]] + 1.5 * stats::IQR(RATIO, na.rm = TRUE)) |
                                                 (RATIO < stats::quantile(RATIO, na.rm = TRUE)[[2]] - 1.5 * stats::IQR(RATIO, na.rm = TRUE)),
                                                 0,
                                                 1),
                           NA))
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
    names(df)[names(df) == sale_col] <- 'SALE_PRICE'
    names(df)[names(df) == assessment_col] <- 'ASSESSED_VALUE'
    names(df)[names(df) == sale_year_col] <- 'SALE_YEAR'

    #create Tax Year if missing
    if (!is.null(tax_year_col)) {
      names(df)[names(df) == tax_year_col] <- 'TAX_YEAR'
    } else {
      df[["TAX_YEAR"]] <- df[[sale_year_col]]
    }
    #rename Market Value if present
    if (!is.null(market_value_col)) {
      names(df)[names(df) == market_value_col] <- 'MARKET_VALUE'
    }

    #arm's length
    df <- calc_ratio_and_mark_arms_length(df)
    if(filter_data){
      df <- df %>% dplyr::filter(arms_length_transaction == 1, !is.na(RATIO))
    }
    return(df)
  }
