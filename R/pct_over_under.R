
#' Produces a key visualization of assessment regressivity, the percent over-under plot
#'
#' @param ratios the output of reformat_data
#' @param min_reporting_yr the minimum year to plot
#' @param max_reporting_yr the maximum year to ploy
#' @param jurisdiction_name the name of the jurisdiction analyzed

#' @return a list (caption, plot)

#' Import UChicago palettes and ggplot theme
source("R/themes.R")

#' @export
pct_over_under <- function(ratios, min_reporting_yr, max_reporting_yr, jurisdiction_name) {
    ii <- min_reporting_yr:max_reporting_yr

    avg_ratio_by_year <-
        ratios %>% dplyr::group_by(.data$TAX_YEAR) %>% dplyr::summarize(avg_ratio = median(.data$RATIO))

    ratios <-
        ratios %>% dplyr::group_by(.data$SALE_YEAR) %>% dplyr::mutate(sale_decile_bin = dplyr::ntile(.data$SALE_PRICE, 10))

    ratios <- ratios %>% dplyr::left_join(avg_ratio_by_year)
    ratios <-
        ratios %>% dplyr::mutate(
            over_avg = ifelse(.data$RATIO > .data$avg_ratio, 1, 0),
            under_avg = ifelse(.data$RATIO < .data$avg_ratio, 1, 0)
        )

    over_under_data <-
        ratios %>% dplyr::group_by(.data$sale_decile_bin) %>% dplyr::summarize(
            Overassessed = sum(.data$over_avg) / dplyr::n(),
            Underassessed = sum(.data$under_avg) / dplyr::n()
        ) %>%
        tidyr::pivot_longer(-.data$sale_decile_bin, names_to = "variable", values_to = "count")

    pct_over_underassessed <-
        ggplot(over_under_data,
               aes(fill = .data$variable, x = .data$sale_decile_bin, y = .data$count)) +
        geom_bar(position = "dodge",
                 stat = "identity",
                 width = 0.75) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_x_continuous(breaks = 1:10) +
      scale_fill_manual(values = v_uchicago_primary_palette) +
        labs(title = "Percent of property over/under assessed",
             x = "Sale Decile", y = NULL) +
      my_theme +
      theme(legend.title = element_blank(),
            legend.position = "bottom")

    #over/under caption
    over_under_caption <- paste0("In ", jurisdiction_name, ", <b>", scales::percent(over_under_data[1,3][[1]]),
                                 "</b> of the lowest value homes are overassessed and <b>",
                                 scales::percent(over_under_data[19,3][[1]]), "</b> of the highest value homes are overassessed.")
    return(list(over_under_caption, pct_over_underassessed))
}

