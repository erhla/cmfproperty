
#' Produces key visualizations of assessment regressivity
#'
#' @param ratios the output of reformat_data
#' @param min_reporting_yr the minimum year to plot
#' @param max_reporting_yr the maximum year to ploy
#' @param jurisdiction_name the name of the jurisdiction analyzed

#' @return a list of plots (cod, prd, prb, binned_scatter)


#' @export
plots <- function(ratios, min_reporting_yr, max_reporting_yr, jurisdiction_name) {
    ii <- min_reporting_yr:max_reporting_yr

    #binned scatter, using inflation adjusted dollars
    ratios <- ratios %>% dplyr::filter(dplyr::between(SALE_YEAR, min_reporting_yr, max_reporting_yr))
    ratios <- ratios %>% dplyr::group_by(SALE_YEAR) %>% dplyr::mutate(sale_decile_bin = dplyr::ntile(SALE_PRICE, 10))
    sale_ratio_decile_tbl <- ratios %>% dplyr::group_by(sale_decile_bin) %>% dplyr::summarize(ratio_mean = mean(RATIO), sale_mean = mean(SALE_PRICE_ADJ))
    all_yr_tbl <- ratios %>% dplyr::group_by(TAX_YEAR, sale_decile_bin) %>% dplyr::summarize(ratio_mean = mean(RATIO), sale_mean = mean(SALE_PRICE_ADJ))
    max_yr_tbl <- all_yr_tbl %>% dplyr::filter(TAX_YEAR == max_reporting_yr)

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
    binned_scatter <- ggplot(data = sale_ratio_decile_tbl) + aes(x = sale_mean, y = ratio_mean) + geom_point(size = 3, color = "grey") +
        geom_line(size = 1.5, color = "grey", linetype = "dashed") + geom_point(data = max_yr_tbl, size = 3) + geom_line(data = max_yr_tbl,
        size = 1.5) + scale_x_continuous(labels = scales::dollar_format()) + labs(x = paste("Sale Price (", max_reporting_yr,
        "dollars)"), y = "Sales Ratio", caption = paste0(max_reporting_yr, " (solid). Full sample average (dashed).")) + my_theme


    #percent over/under assessed
    avg_ratio_by_year <-
        ratios %>% dplyr::group_by(TAX_YEAR) %>% dplyr::summarize(avg_ratio = median(RATIO))
    ratios <- ratios %>% dplyr::left_join(avg_ratio_by_year)
    ratios <-
        ratios %>% dplyr::mutate(
            over_avg = ifelse(RATIO > avg_ratio, 1, 0),
            under_avg = ifelse(RATIO < avg_ratio, 1, 0)
        )

    over_under_data <-
        ratios %>% dplyr::group_by(sale_decile_bin) %>% dplyr::summarize(Overassessed = sum(over_avg) / dplyr::n(),
                                                                         Underassessed = sum(under_avg) / dplyr::n()) %>%
        tidyr::pivot_longer(-sale_decile_bin, names_to = "variable", values_to = "count")

    pct_over_underassessed <-
        ggplot(over_under_data,
               aes(fill = variable, x = sale_decile_bin, y = count)) +
        geom_bar(position = "dodge",
                 stat = "identity",
                 width = 0.75) + scale_y_continuous(labels = scales::percent_format()) + scale_x_continuous(breaks = 1:10) +
        labs(title = "Percent of property over/under assessed", x = "Sale Decile", y = NULL) + my_theme + theme(legend.title = element_blank(), legend.position =
                                                                                                                    "bottom")

    #over/under caption
    over_under_caption <- paste0("In ", jurisdiction_name, ", <b>", scales::percent(over_under_data[1,3][[1]]),
                      "</b> of the lowest value homes are overassessed and <b>",
                      scales::percent(over_under_data[19,3][[1]]), "</b> of the highest value homes are overassessed.")

    return(list(binned_scatter_caption, binned_scatter, over_under_caption, pct_over_underassessed))
}


