#' Produces plots and tables helpful for data diagnostics
#'
#' @param stats the output of calc_iaao_stats
#' @param ratios the output of reformat_data
#' @param min_reporting_yr the minimum year to plot
#' @param max_reporting_yr the maximum year to ploy

#' @return a list of plots (num_prop, sale_plot, sale_ratio_plot, sp_vs_av, ratio_plot, past_ratios_hist, past_ratios_cdf, current_ratios_hist, current_ratios_cdf)

#' @importFrom scales comma dollar_format percent_format
#' @importFrom stats quantile

#' @export

diagnostic_plots <- function(stats, ratios, min_reporting_yr, max_reporting_yr){
  ii <- (min_reporting_yr):(max_reporting_yr)
  q5 <- quantile(ratios$RATIO, 0.05, na.rm = TRUE)
  q95 <- quantile(ratios$RATIO, 0.95, na.rm = TRUE)
  s1 <- quantile(ratios$SALE_PRICE_ADJ, 0.01, na.rm = TRUE)
  s99 <- quantile(ratios$SALE_PRICE_ADJ, 0.99, na.rm = TRUE)
  filtered_ratios <- ratios %>% dplyr::filter(dplyr::between(.data$RATIO, q5, q95),
                                              dplyr::between(.data$SALE_PRICE_ADJ, s1, s99))

  #sale price and assessed value diagnostics

  num_prop <- ggplot(stats, aes(x = .data$Year, ymin = 0.6 * min(.data$N))) +
    geom_line(aes(y = .data$N), size = 1.5, color = "#2C4894") +
    geom_point(aes(y = .data$N), size = 3, color = "#2C4894") +
    my_theme_rotated +
    labs(title = "Number of Arm's Length Sales",
         x = "Tax Year", y = NULL) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = ii)

  sale_plot <- ggplot(data = stats) +
    geom_line(aes(x = .data$Year, y = .data$median_sale), size = 1.5, color = "#2C4894") +
    geom_point(aes(x = .data$Year, y = .data$median_sale), size = 3, color = "#2C4894") +
    geom_line(aes(x = .data$Year, y = .data$median_assessed_value), linetype = "dashed", size = 1.25, color = "#2C4894") +
    geom_line(aes(x = .data$Year, y = .data$q1_sale), size = 1.5, color = "#954B36") +
    geom_point(aes(x = .data$Year, y = .data$q1_sale), size = 3, color = "#954B36") +
    geom_line(aes(x = .data$Year, y = .data$q1_assessed_value), linetype = "dashed", size = 1.25, color = "#954B36") +
    geom_line(aes(x = .data$Year, y = .data$q3_sale), size = 1.5, color = "#D8A436") +
    geom_point(aes(x = .data$Year, y = .data$q3_sale), size = 3, color = "#D8A436") +
    geom_line(aes(x = .data$Year, y = .data$q3_assessed_value), linetype = "dashed", size = 1.25, color = "#D8A436") +
    labs(title = "Sale Price (Solid) & Assessed Value (Dashed)",
         subtitle = "25th, 50th, and 75th percentiles", x = "Tax Year", y = NULL) + my_theme_rotated +
    scale_y_continuous(labels = dollar_format()) +
    scale_x_continuous(breaks = ii)

  sale_ratio_plot <- ggplot(data = stats) +
    geom_line(aes(x = .data$Year, y = .data$median_assessed_value / .data$median_sale), size = 1.5, color = "#2C4894") +
    geom_point(aes(x = .data$Year, y = .data$median_assessed_value / .data$median_sale), size = 3, color = "#2C4894") +
    geom_line(aes(x = .data$Year, y = .data$q1_assessed_value / .data$q1_sale), size = 1.5, color = "#954B36") +
    geom_point(aes(x = .data$Year, y = .data$q1_assessed_value / .data$q1_sale), size = 3, color = "#954B36") +
    geom_line(aes(x = .data$Year, y = .data$q3_assessed_value / .data$q3_sale), size = 1.5, color = "#D8A436") +
    geom_point(aes(x = .data$Year, y = .data$q3_assessed_value / .data$q3_sale), size = 3, color = "#D8A436") +
    labs(title = "Ratio between Assessed Value and Sale Price",
         subtitle = "25th (Red), 50th (Blue), and 75th (Gold) percentiles", x = "Tax Year", y = "Ratio (percentage)") +
    my_theme_rotated +
    scale_y_continuous(labels = percent_format()) +
    scale_x_continuous(breaks = ii)

  plot_max <- max(filtered_ratios$SALE_PRICE_ADJ, filtered_ratios$ASSESSED_VALUE_ADJ)

  sp_vs_av <-
    ggplot(data = filtered_ratios, aes(x = .data$SALE_PRICE_ADJ, y = .data$ASSESSED_VALUE_ADJ)) +
    geom_smooth(size=2, level=0.9) +
    scale_x_continuous(labels = dollar_format(), limits = c(0, plot_max*.75)) +
    scale_y_continuous(labels = dollar_format(), limits = c(0, plot_max*.75)) +
    geom_segment(aes(x=0, y=0, xend=0.75*plot_max, yend=0.75*plot_max), color="grey", linetype="dashed", size=1.5) +
    labs(x = "Sale Price", y = "Total Assessed Value", title = "Inflation Adjusted Sale Price and Assessed Value") +
    my_theme

  #ratio diagnostics
  ratio_plot <- ggplot(data = stats) +
    geom_line(aes(x = .data$Year, y = .data$median_ratio), color = "#D8A436", size = 1.5) +
    geom_point(aes(x = .data$Year, y = .data$median_ratio), color = "#D8A436", size = 3) +
    my_theme_rotated +
    labs(title = "Median Sales Ratio", y = NULL) +
    scale_y_continuous(labels = percent_format()) +
    scale_x_continuous(breaks = ii)

  past_ratios <- filtered_ratios %>% dplyr::filter(.data$TAX_YEAR != max_reporting_yr)
  current_ratios <- filtered_ratios %>% dplyr::filter(.data$TAX_YEAR == max_reporting_yr)

  past_ratios_hist <- ggplot(data = past_ratios) +
    geom_histogram(aes(x = .data$RATIO), breaks = c(seq(q5, q95, length.out = 30)), fill = "#2C4894", alpha = 0.5) +
    coord_cartesian(xlim = c(q5, q95)) +
    labs(y = NULL, x = NULL, title = "Past Ratios") +
    scale_y_continuous(expand = c(0, 0)) + my_theme

  current_ratios_hist <- ggplot(data = current_ratios) +
    geom_histogram(aes(x = .data$RATIO), breaks = c(seq(q5, q95, length.out = 30)), fill = "#2C4894", alpha = 0.7) +
    coord_cartesian(xlim = c(q5, q95)) +
    labs(y = NULL, x = NULL, title = paste0(max_reporting_yr, " Ratios")) +
    scale_y_continuous(expand = c(0, 0)) + my_theme

  past_ratios_cdf <- ggplot(data = past_ratios) +
    stat_ecdf(aes(x = .data$RATIO), size = 1.25, color = "#2C4894") + xlim(q5, q95) +
    labs(y = "Cumulative Distribution", x = NULL) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_hline(yintercept = 0.5, linetype = "dashed") + my_theme

  current_ratios_cdf <- ggplot(data = current_ratios) +
    stat_ecdf(aes(x = .data$RATIO), size = 1.25, color = "#2C4894") + xlim(q5, q95) +
    labs(y = "Cumulative Distribution", x = NULL) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_hline(yintercept = 0.5, linetype = "dashed") + my_theme

  return(list(num_prop, sale_plot, sale_ratio_plot, sp_vs_av, ratio_plot, past_ratios_hist, past_ratios_cdf, current_ratios_hist, current_ratios_cdf))
}


