
#' Produces various plots
#'
#' @param stats the output of iaao_stats
#' @param ratios the output of reformat_data
#' @param min_reporting_yr the minimum year to plot
#' @param max_reporting_yr the maximum year to ploy
#' @return a list of plots (cod, prd, prb, binned_scatter)

#' @import ggplot2

#' @export
plots <- function(stats, ratios, min_reporting_yr, max_reporting_yr) {
    # constants and themes
    ii <- min_reporting_yr:max_reporting_yr
    my_theme <- theme_classic() + theme(panel.grid.major = element_line(color = "gray"), axis.line.x = element_line(color = "gray"), 
        axis.line.y = element_blank())
    
    # adjust for inflation
    fred <- utils::read.csv("data-raw/CPIHOSNS.csv")
    fred <- fred %>% tidyr::separate(DATE, c("Year", "Month", "Day")) %>% dplyr::mutate_all(as.numeric) %>% dplyr::group_by(Year) %>% 
        dplyr::summarize(CPIHOSNS = mean(CPIHOSNS))
    final_indx_value <- fred %>% dplyr::filter(Year == max_reporting_yr) %>% dplyr::select(CPIHOSNS)
    fred <- fred %>% dplyr::mutate(percent_adj = as.numeric(final_indx_value)/CPIHOSNS) %>% dplyr::group_by(Year) %>% dplyr::summarize(percent_adj = mean(percent_adj))
    ratios <- ratios %>% dplyr::left_join(fred, by = c(TAX_YEAR = "Year"))
    ratios["SALE_PRICE"] <- ratios["SALE_PRICE"] * ratios["percent_adj"]
    
    # manipulate data for binned scatter
    ratios <- ratios %>% dplyr::filter(dplyr::between(SALE_YEAR, min_reporting_yr, max_reporting_yr))
    
    ratios <- ratios %>% dplyr::group_by(SALE_YEAR) %>% dplyr::mutate(sale_decile_bin = dplyr::ntile(SALE_PRICE, 10))
    
    sale_ratio_decile_tbl <- ratios %>% dplyr::group_by(sale_decile_bin) %>% dplyr::summarize(ratio_mean = mean(RATIO), sale_mean = mean(SALE_PRICE))
    
    all_yr_tbl <- ratios %>% dplyr::group_by(TAX_YEAR, sale_decile_bin) %>% dplyr::summarize(ratio_mean = mean(RATIO), sale_mean = mean(SALE_PRICE))
    
    max_yr_tbl <- all_yr_tbl %>% dplyr::filter(TAX_YEAR == max_reporting_yr)
    
    # captions
    cod_maxyr <- round(stats[stats["Year"] == max_reporting_yr, "COD"], 2)
    cod_caption <- paste0("Dotted lines represent 95% Confidence Interval.\nIn ", max_reporting_yr, ", the Coefficient of Dispersion was ", 
        cod_maxyr, " which ", ifelse(cod_maxyr <= 15, "does ", "does not "), "meet the IAAO standard for uniformity.\n", 
        " With this value, a property worth $100,000 has a 50% chance to be assessed between $", round(1e+05 * (1 - cod_maxyr/100), 
            2), " and $", round(1e+05 * (1 + cod_maxyr/100), 2), ".")
    
    prd_last <- round(stats[stats["Year"] == max_reporting_yr, "PRD"], 3)
    prd_caption <- paste0("Dotted lines represent 95% Confidence Interval.\nIn ", max_reporting_yr, ", the Price-Related Differential was ", 
        prd_last, " which ", ifelse(dplyr::between(prd_last, 0.98, 1.03), "does ", "does not "), "meet the IAAO standard for uniformity.")
    prb_last <- round(stats[stats["Year"] == max_reporting_yr, "PRB"], 3)
    prb_caption <- paste0("Dotted lines represent 95% Confidence Interval.\nIn ", max_reporting_yr, ", the Price-Related Bias was ", 
        prb_last, " which ", ifelse(dplyr::between(prb_last, -0.05, 0.05), "does ", "does not "), "meet the IAAO standard for uniformity.\n", 
        "This value indicates that assessment ratios ", ifelse(prb_last > 0, "increase", "decrease"), " by ", abs(prb_last * 
            100), "% when assessed value doubles.")
    
    binnedscatter_caption <- paste0("For ", max_reporting_yr, ", the highest ten percent of sales were assessed at ", round(100 * 
        max_yr_tbl[10, 3][[1]]/max_yr_tbl[1, 3][[1]], 1), "% of the rate of assessment applied \nto the lowest ten percent of sales. Top decile rate: ", 
        round(100 * max_yr_tbl[10, 3][[1]], 1), "%. Bottom decile rate: ", round(100 * max_yr_tbl[1, 3][[1]], 1), "%.")
    # plots
    cod_plot <- ggplot(data = stats, aes(x = Year)) + geom_line(aes(y = COD), color = "#2C4894", size = 1.5) + geom_point(aes(y = COD), 
        color = "#2C4894", size = 3) + geom_line(aes(y = COD + 1.96 * COD_SE), linetype = "dashed") + geom_line(aes(y = COD - 
        1.96 * COD_SE), linetype = "dashed") + my_theme + labs(title = "Coefficient of Dispersion", caption = cod_caption, 
        subtitle = "IAAO Benchmark: 15 or below", y = NULL, x = NULL) + annotate("rect", xmin = as.numeric(min_reporting_yr), 
        xmax = as.numeric(max_reporting_yr), ymin = 10, ymax = 15, alpha = 0.1) + scale_x_continuous(breaks = ii, limits = c(min_reporting_yr, 
        max_reporting_yr))
    
    binned_scatter <- ggplot(data = sale_ratio_decile_tbl) + aes(x = sale_mean, y = ratio_mean) + geom_point(size = 3, color = "grey") + 
        geom_line(size = 1.5, color = "grey", linetype = "dashed") + geom_point(data = max_yr_tbl, size = 3) + geom_line(data = max_yr_tbl, 
        size = 1.5) + scale_x_continuous(labels = scales::dollar_format()) + labs(x = paste("Sale Price (SP),", "in", max_reporting_yr, 
        "dollars"), y = "Assessment Ratio (ASR)", title = paste("Binned Scatter Plot of ASR vs. SP for", max_reporting_yr, 
        "(solid)"), caption = binnedscatter_caption, subtitle = "Full sample average (dashed)") + my_theme
    
    prd_plot <- ggplot(data = stats, aes(x = Year)) + geom_line(aes(y = PRD), color = "#2C4894", size = 1.5) + geom_point(aes(y = PRD), 
        color = "#2C4894", size = 3) + geom_line(aes(y = PRD - 1.96 * PRD_SE), linetype = "dashed") + geom_line(aes(y = PRD + 
        1.96 * PRD_SE), linetype = "dashed") + my_theme + annotate("rect", xmin = as.numeric(min_reporting_yr), xmax = as.numeric(max_reporting_yr), 
        ymin = 0.98, ymax = 1.03, alpha = 0.1) + labs(title = "Price-Related Differential", subtitle = "IAAO Benchmark: 0.98 to 1.03", 
        y = NULL, x = NULL, caption = prd_caption) + scale_x_continuous(breaks = ii, limits = c(min_reporting_yr, max_reporting_yr))
    
    prb_plot <- ggplot(data = stats, aes(x = Year)) + geom_line(aes(y = PRB), color = "#2C4894", size = 1.5) + geom_point(aes(y = PRB), 
        color = "#2C4894", size = 3) + geom_line(aes(y = PRB - 1.96 * PRB_SE), linetype = "dashed") + geom_line(aes(y = PRB + 
        1.96 * PRB_SE), linetype = "dashed") + my_theme + labs(title = "Price-Related Bias", caption = prb_caption, subtitle = "IAAO Benchmark: +/- 0.05", 
        y = NULL, x = NULL) + annotate("rect", xmin = as.numeric(min_reporting_yr), xmax = as.numeric(max_reporting_yr), 
        ymin = -0.05, ymax = 0.05, alpha = 0.1) + scale_x_continuous(breaks = ii, limits = c(min_reporting_yr, max_reporting_yr))
    
    return(list(cod_plot, prd_plot, prb_plot, binned_scatter))
}
