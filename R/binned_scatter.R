
#' Produces a key visualization of assessment regressivity, the binned scatter plot
#'
#' @param ratios the output of reformat_data
#' @param min_reporting_yr the minimum year to plot
#' @param max_reporting_yr the maximum year to ploy
#' @param jurisdiction_name the name of the jurisdiction analyzed

#' @return a list (caption, plot)


#' @export
binned_scatter <- function(ratios, min_reporting_yr, max_reporting_yr, jurisdiction_name) {
  ii <- min_reporting_yr:max_reporting_yr

  #binned scatter, using inflation adjusted dollars
  ratios <-
    ratios %>% dplyr::filter(dplyr::between(.data$SALE_YEAR, min_reporting_yr, max_reporting_yr))

  ratios <-
    ratios %>% dplyr::group_by(.data$SALE_YEAR) %>% dplyr::mutate(sale_decile_bin = dplyr::ntile(.data$SALE_PRICE, 10))

  sale_ratio_decile_tbl <-
    ratios %>%
      dplyr::group_by(.data$sale_decile_bin) %>%
      dplyr::summarize(ratio_mean = mean(.data$RATIO),
                       sale_mean = mean(.data$SALE_PRICE_ADJ))

  all_yr_tbl <-
    ratios %>%
      dplyr::group_by(.data$TAX_YEAR, .data$sale_decile_bin) %>%
      dplyr::summarize(ratio_mean = mean(.data$RATIO),
                       sale_mean = mean(.data$SALE_PRICE_ADJ))
  max_yr_tbl <-
    all_yr_tbl %>% dplyr::filter(.data$TAX_YEAR == max_reporting_yr)

  # captions
  binned_scatter_caption <-
    paste0("In ", max_reporting_yr, ", the most expensive homes (the top decile) were assessed at ",
           scales::percent(max_yr_tbl[10,3][[1]], 0.1), " of their value and the least expensive homes (the bottom decile) were assessed at ",
           scales::percent(max_yr_tbl[1,3][[1]], 0.1), ". In other words, the least expensive homes were assessed at <b>", round(max_yr_tbl[1,3][[1]] / max_yr_tbl[10,3][[1]], 2),
           " times</b> the rate applied to the most expensive homes. Across our sample from ", min_reporting_yr, " to ",
           max_reporting_yr, ", the most expensive homes were assessed at ",
           scales::percent(sale_ratio_decile_tbl[10,2][[1]], 0.1), " of their value and the least expensive homes were assessed at ",
           scales::percent(sale_ratio_decile_tbl[1,2][[1]], 0.1), ", which is <b>",
           round(sale_ratio_decile_tbl[1,2][[1]] / sale_ratio_decile_tbl[10,2][[1]], 2),
           " times</b> the rate applied to the most expensive homes.")

  # plots
  binned_scatter <-
    ggplot(data = sale_ratio_decile_tbl) +
    aes(x = .data$sale_mean, y = .data$ratio_mean) +
    geom_point(size = 3, color = "grey") +
    geom_line(size = 1.5, color = "grey", linetype = "dashed") +
    geom_point(data = max_yr_tbl, size = 3) +
    geom_line(data = max_yr_tbl,                                                                                                                     size = 1.5) + scale_x_continuous(labels = scales::dollar_format()) + labs(x = paste("Sale Price (", max_reporting_yr,
                                                                                                                                                                                                         "dollars)"), y = "Sales Ratio", caption = paste0(max_reporting_yr, " (solid). Full sample average (dashed).")) + my_theme
  return(list(binned_scatter_caption, binned_scatter))
}


