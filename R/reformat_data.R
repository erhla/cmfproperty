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

#' @importFrom magrittr %>%

#'@export
reformat_data <-
  function(df,
           sale_col,
           assessment_col,
           sale_year_col,
           filter_data,
           market_value_col = NULL,
           tax_year_col = NULL) {
    # rename columns
    names(df)[names(df) == sale_col] <- "SALE_PRICE"
    names(df)[names(df) == assessment_col] <- "ASSESSED_VALUE"
    names(df)[names(df) == sale_year_col] <- "SALE_YEAR"

    # create Tax Year if missing
    if (!is.null(tax_year_col)) {
      names(df)[names(df) == tax_year_col] <- "TAX_YEAR"
    } else {
      df[["TAX_YEAR"]] <- df[[sale_year_col]]
    }
    # rename Market Value if present
    if (!is.null(market_value_col)) {
      names(df)[names(df) == market_value_col] <- "MARKET_VALUE"
    }
    # add ratio
    df <-
      df %>% dplyr::mutate(RATIO = ifelse(
        !is.na(SALE_PRICE),
        ifelse(SALE_PRICE > 100, ASSESSED_VALUE / SALE_PRICE, NA),
        NA
      ))
    # add arm's length
    df <-
      df %>% dplyr::group_by(SALE_YEAR) %>% dplyr::mutate(arms_length_transaction = ifelse(!is.na(RATIO), ifelse((
        RATIO >
          stats::quantile(RATIO, na.rm = TRUE)[[4]] + 1.5 * stats::IQR(RATIO, na.rm = TRUE)
      ) | (
        RATIO < stats::quantile(RATIO,
                                na.rm = TRUE)[[2]] - 1.5 * stats::IQR(RATIO, na.rm = TRUE)
      ),
      0,
      1
      ), NA))

    if (filter_data) {
      df <-
        df %>% dplyr::filter(arms_length_transaction == 1,!is.na(RATIO))
      print("Filtered out non-arm's length transactions")
    }
    #adjust for inflation
    df <- adj_for_inflation(df)
    return(df)
  }

adj_for_inflation <- function(df) {
  fred <- utils::read.csv("data-raw/CPIHOSNS.csv")
  fred <-
    fred %>% tidyr::separate(DATE, c("Year", "Month", "Day")) %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarize(CPIHOSNS = mean(CPIHOSNS))

  max_yr <- max(df$SALE_YEAR)
  if (max_yr > max(fred$Year)) {
    max_yr <- max(fred$Year)
  }
  print(paste0("Inflation adjusted to ", max_yr))

  final_indx_value <-
    fred %>% dplyr::filter(Year == max_yr) %>% dplyr::select(CPIHOSNS)
  fred <-
    fred %>% dplyr::mutate(percent_adj = as.numeric(final_indx_value) / CPIHOSNS) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarize(percent_adj = mean(percent_adj))

  df <- df %>% dplyr::left_join(fred, by = c("TAX_YEAR" = "Year"))

  df["SALE_PRICE_ADJ"] <- df["SALE_PRICE"] * df["percent_adj"]
  df["ASSESSED_VALUE_ADJ"] <- df["ASSESSED_VALUE"] * df["percent_adj"]
  df <- df %>% dplyr::select(-percent_adj)
  return(df)
}
