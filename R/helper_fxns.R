adj_for_inflation <- function(df) {
  fred <- #within sysdata.rda
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


my_theme <- theme_classic() + theme(panel.grid.major = element_line(color = "gray"), axis.line.x = element_line(color = "gray"),
                                    axis.line.y = element_blank())

my_theme_rotated <- theme_classic() + theme(panel.grid.major = element_line(color = "gray"),
                                            axis.line.x = element_line(color = "gray"), axis.line.y = element_blank(),
                                            axis.text.x = element_text(angle = 45, hjust = 1))
