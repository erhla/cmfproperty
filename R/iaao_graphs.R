#' Produces graphs of IAAO standards over time
#'
#' @param stats the output of iaao_stats
#' @param ratios the output of reformat_data
#' @param min_reporting_yr the minimum year to plot
#' @param max_reporting_yr the maximum year to ploy
#' @param jurisdiction_name the name of the jurisdiction analyzed
#' @return a list of plots (cod, prd, prb, binned_scatter)

#' @importFrom magrittr %>%

#' @export
iaao_graphs <-  function(stats, ratios, min_reporting_yr, max_reporting_yr, jurisdiction_name){
  ii <- min_reporting_yr:max_reporting_yr
  max_stats <- stats[stats["Year"] == max_reporting_yr, ]
  rect_alpha <- 0.25

  #COD
  cod_standard <- ifelse(max_stats$COD < 15, "did", "did not")

  cod_text <- paste0("For ", max_reporting_yr, ", the COD in ", jurisdiction_name, " was ", round(max_stats$COD, 2), " which <b>",
                     cod_standard, " meet</b> the IAAO standard for uniformity. ")

  cod_plot <- ggplot(data = stats, aes(x = .data$Year)) +
    geom_line(aes(y = .data$COD), color = "#2C4894", size = 1.5) +
    geom_point(aes(y = .data$COD), color = "#2C4894", size = 3) +
    geom_line(aes(y = .data$COD + 1.96 * .data$COD_SE), linetype = "dashed") +
    geom_line(aes(y = .data$COD - 1.96 * .data$COD_SE), linetype = "dashed") +
    my_theme_rotated +
    labs(caption = "IAAO Benchmark: 15 or below (shaded). Dotted lines represent the 95% Confidence Interval.", y = "COD", x = "Year") +
    annotate("rect", xmin = as.numeric(min_reporting_yr), xmax = as.numeric(max_reporting_yr),
             ymin = 10, ymax = 15, alpha = rect_alpha) +
    scale_x_continuous(breaks = ii)

  #PRD
  prd_text <- paste0(" In ", max_reporting_yr, ", the PRD in  ", jurisdiction_name, ", was ",
                     round(max_stats$PRD, 3), " which <b>", ifelse(dplyr::between(max_stats$PRD, 0.98, 1.03), "meets ", "does not meet "),
                     "</b> the IAAO standard for vertical equity.")

  prd_plot <- ggplot(data = stats, aes(x = .data$Year)) +
    geom_line(aes(y = .data$PRD), color = "#2C4894", size = 1.5) +
    geom_point(aes(y = .data$PRD), color = "#2C4894", size = 3) +
    geom_line(aes(y = .data$PRD - 1.96 * .data$PRD_SE), linetype = "dashed") +
    geom_line(aes(y = .data$PRD + 1.96 * .data$PRD_SE), linetype = "dashed") +
    my_theme_rotated +
    annotate("rect", xmin = as.numeric(min_reporting_yr), xmax = as.numeric(max_reporting_yr),
             ymin = 0.98, ymax = 1.03, alpha = rect_alpha) +
    labs(caption = "IAAO Benchmark: 0.98 to 1.03 (shaded). Dotted lines represent the 95% Confidence Interval.", y = "Year", x = "PRD") +
    scale_x_continuous(breaks = ii)

  #PRB
  prb_text <- paste0("In ", max_reporting_yr, ", the PRB in ", jurisdiction_name, " was ", round(max_stats$PRB, 3), " which indicates that sales ratios ",
                     ifelse(max_stats$PRB > 0, "increase", "decrease"), " by ", scales::percent(abs(max_stats$PRB), 0.1), " when home values double.",
                     " This <b>", ifelse(dplyr::between(max_stats$PRB, -0.05, 0.05), "meets ", "does not meet "), "</b>the IAAO standard.")

  prb_plot <- ggplot(data = stats, aes(x = .data$Year)) +
    geom_line(aes(y = .data$PRB), color = "#2C4894", size = 1.5) +
    geom_point(aes(y = .data$PRB), color = "#2C4894", size = 3) +
    geom_line(aes(y = .data$PRB - 1.96 * .data$PRB_SE), linetype = "dashed") +
    geom_line(aes(y = .data$PRB + 1.96 * .data$PRB_SE), linetype = "dashed") +
    my_theme_rotated +
    labs(caption = "IAAO Benchmark: +/- 0.05 (shaded). Dotted lines represent the 95% Confidence Interval.", y = "PRB", x = "Year") +
    annotate("rect", xmin = as.numeric(min_reporting_yr), xmax = as.numeric(max_reporting_yr),
             ymin = -.05, ymax = .05, alpha = rect_alpha) +
    scale_x_continuous(breaks = ii)

  return(list(cod_text, cod_plot, prd_text, prd_plot, prb_text, prb_plot))
}


