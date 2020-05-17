#' @import ggplot2
#' @importFrom rlang .data


adj_for_inflation <- function(df) {
  fred <- #within sysdata.rda
    fred %>% tidyr::separate(.data$DATE, c("Year", "Month", "Day")) %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::group_by(.data$Year) %>%
    dplyr::summarize(CPIHOSNS = mean(.data$CPIHOSNS))

  max_yr <- max(df$SALE_YEAR)
  if (max_yr > max(fred$Year)) {
    max_yr <- max(fred$Year)
  }
  print(paste0("Inflation adjusted to ", max_yr))

  final_indx_value <-
    fred %>% dplyr::filter(.data$Year == max_yr) %>% dplyr::select(.data$CPIHOSNS)
  fred <-
    fred %>% dplyr::mutate(percent_adj = as.numeric(final_indx_value) / .data$CPIHOSNS) %>%
    dplyr::group_by(.data$Year) %>%
    dplyr::summarize(percent_adj = mean(.data$percent_adj))

  df <- df %>% dplyr::left_join(fred, by = c("SALE_YEAR" = "Year"))

  #for when data is more recent than fred
  df <- df %>% tidyr::replace_na(list(percent_adj = 1))

  df["SALE_PRICE_ADJ"] <- df["SALE_PRICE"] * df["percent_adj"]
  df["ASSESSED_VALUE_ADJ"] <- df["ASSESSED_VALUE"] * df["percent_adj"]
  df <- df %>% dplyr::select(-.data$percent_adj)
  return(df)
}


my_theme <- theme_classic() + theme(panel.grid.major = element_line(color = "gray"), axis.line.x = element_line(color = "gray"),
                                    axis.line.y = element_blank())

my_theme_rotated <- theme_classic() + theme(panel.grid.major = element_line(color = "gray"),
                                            axis.line.x = element_line(color = "gray"), axis.line.y = element_blank(),
                                            axis.text.x = element_text(angle = 45, hjust = 1))
