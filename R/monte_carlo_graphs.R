#' Conducts a monte carlo simulation, which approximates the impacts of random factors in regressivity statistics
#'
#' @param ratios a dataframe with assessment, sales, and time data which has been processed by reformat_data
#' @return a list of six graphs for COD, PRB, PRD, paglin72, cheng74, and IAAO78 (see README for example)

#' @export
monte_carlo_graphs<- function(ratios){
  avg_stats <- monte_carlo_sim(ratios, 2)
  comp_stats <- get_stats(ratios, 100)
  reg_stats <- paglin_cheng_IAAO_coefs(ratios)

  COD_shock <- approxfun(avg_stats$COD, avg_stats$shock_pct, method = "linear")(comp_stats$COD)
  PRD_shock <- approxfun(avg_stats$PRD, avg_stats$shock_pct, method = "linear")(comp_stats$PRD)
  PRB_shock <- approxfun(avg_stats$PRB, avg_stats$shock_pct, method = "linear")(comp_stats$PRB)
  paglin72_shock <- approxfun(avg_stats$paglin72, avg_stats$shock_pct, method = "linear")(reg_stats[[1]])
  cheng74_shock <- approxfun(avg_stats$cheng74, avg_stats$shock_pct, method = "linear")(reg_stats[[2]])
  IAAO78_shock <- approxfun(avg_stats$IAAO78, avg_stats$shock_pct, method = "linear")(reg_stats[[3]])

  shock_df <- as.data.frame(cbind(list("COD", "PRD", "PRB", "paglin72", "cheng74", "IAAO78"),
                                  list(COD_shock, PRD_shock, PRB_shock, paglin72_shock, cheng74_shock, IAAO78_shock)))
  names(shock_df) <- c("metric", "shock percent")
  shock_df["shock percent"] <- format(shock_df["shock percent"], scientific=FALSE, digits = 3)

  if(nrow(shock_df[shock_df["shock percent"] == "NA", ]) > 0){
    shock_df[shock_df["shock percent"] == "NA", ]["shock percent"] <- '>0.25'
  }

  COD_graph <- ggplot(data = avg_stats, aes(x = .data$shock_pct, y = .data$COD)) + geom_line(size = 1.5) +
    geom_point(size = 3) + geom_hline(yintercept = comp_stats$COD) +
    geom_vline(xintercept = COD_shock) +
    labs(title = "COD", x = "Shock Percentage", y = "COD") + scale_x_continuous(labels = scales::percent_format()) +
    my_theme

  PRD_graph <- ggplot(data = avg_stats, aes(x = .data$shock_pct, y = .data$PRD)) + geom_line(size = 1.5) +
    geom_point(size = 3) + geom_hline(yintercept = comp_stats$PRD) +
    geom_vline(xintercept = PRD_shock) +
    labs(title = "PRD", x = "Shock Percentage", y = "PRD") + scale_x_continuous(labels = scales::percent_format()) +
    my_theme

  PRB_graph <- ggplot(data = avg_stats, aes(x = .data$shock_pct, y = .data$PRB)) + geom_line(size = 1.5) +
    geom_point(size = 3) + geom_hline(yintercept = comp_stats$PRB) +
    geom_vline(xintercept = PRB_shock) +
    labs(title = "PRB", x = "Shock Percentage", y = "PRB") + scale_x_continuous(labels = scales::percent_format()) +
    my_theme

  paglin72_graph <- ggplot(data = avg_stats, aes(x = .data$shock_pct, y = .data$paglin72)) + geom_line(size = 1.5) +
    geom_point(size = 3) + geom_hline(yintercept = reg_stats[[1]]) +
    geom_vline(xintercept = paglin72_shock) +
    labs(title = "AV ~ SP", x = "Shock Percentage", y = "Coef") + scale_x_continuous(labels = scales::percent_format()) +
    my_theme

  cheng74_graph <- ggplot(data = avg_stats, aes(x = .data$shock_pct, y = .data$cheng74)) + geom_line(size = 1.5) +
    geom_point(size = 3) + geom_hline(yintercept = reg_stats[[2]]) +
    geom_vline(xintercept = cheng74_shock) +
    labs(title = "log(AV) ~ log(SP)", x = "Shock Percentage", y = "Coef") + scale_x_continuous(labels = scales::percent_format()) +
    my_theme

  IAAO78_graph <- ggplot(data = avg_stats, aes(x = .data$shock_pct, y = .data$IAAO78)) + geom_line(size = 1.5) +
    geom_point(size = 3) + geom_hline(yintercept = reg_stats[[3]]) +
    geom_vline(xintercept = IAAO78_shock) +
    labs(title = "RATIO ~ SP", x = "Shock Percentage", y = "Coef") + scale_x_continuous(labels = scales::percent_format()) +
    my_theme

  return(list(COD_graph, PRB_graph, PRD_graph, paglin72_graph, cheng74_graph, IAAO78_graph))
}

paglin_cheng_IAAO_coefs <- function(ratios){
  ratios <- ratios %>% dplyr::mutate(logsp = log(.data$SALE_PRICE + 1),
                              logav = log(.data$ASSESSED_VALUE + 1))

  paglin72 <- stats::lm(ASSESSED_VALUE ~ SALE_PRICE, data = ratios)$coefficients[[2]]
  cheng74 <- stats::lm(logav ~ logsp, data = ratios)$coefficients[[2]]
  IAAO78 <- stats::lm(RATIO ~ SALE_PRICE, data = ratios)$coefficients[[2]]
  return(list(paglin72, cheng74, IAAO78))
}

monte_carlo_sim <- function(ratios, iters){
  all_rslts <- data.frame()
  ratios <- ratios %>% dplyr::filter(.data$SALE_PRICE > 100 & .data$ASSESSED_VALUE > 100)
  for (shock_pct in seq(0, 0.25, by = 0.01)){
    iter <- 0
    while (iter < iters){
      cur <- ratios %>% dplyr::mutate(shock = rnorm(dplyr::n(), 0, shock_pct),
                                      SALE_PRICE = .data$ASSESSED_VALUE * (1 + .data$shock), #This is simulated SALE_PRICE
                                      RATIO = .data$ASSESSED_VALUE /.data$SALE_PRICE)

      cur <- cur %>% dplyr::filter(abs(.data$shock) < 1)
      tmp <- get_stats(as.data.frame(cur), 5)
      tmp2 <- paglin_cheng_IAAO_coefs(cur)

      tmp <- tmp %>% dplyr::mutate(shock_pct = shock_pct,
                            iter = iter,
                            paglin72 = tmp2[[1]],
                            cheng74 = tmp2[[2]],
                            IAAO78 = tmp2[[3]])

      iter <- iter + 1
      all_rslts <- rbind(all_rslts, tmp)
    }
  }
  avg_stats <- all_rslts %>% dplyr::group_by(.data$shock_pct) %>%
    dplyr::summarize(COD = mean(.data$COD),
                     PRD = mean(.data$PRD),
                     PRB = mean(.data$PRB),
                     med_ratio = mean(.data$median_ratio),
                     paglin72 = mean(.data$paglin72),
                     cheng74 = mean(.data$cheng74),
                     IAAO78 = mean(.data$IAAO78))
  return(avg_stats)
}


