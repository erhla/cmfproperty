cod_func <- function(df, ratio_col, bootstrap_iters) {
    n <- length(df)
    generated_cods <- NULL
    for (i in 1:bootstrap_iters) {
        s <- dplyr::sample_n(data.frame(df[[ratio_col]]), n, replace = T)
        generated_cods[i] <- 100 * sum(abs(s[, 1] - stats::median(s[, 1])))/(n * stats::median(s[, 1]))
    }
    cod_output <- c(round(mean(generated_cods, na.rm = TRUE), 4), round(stats::sd(generated_cods, na.rm = TRUE), 4))
    return(cod_output)
}

prd_func <- function(df, ratio_col, sale_price_col, bootstrap_iters) {
    generated_prds <- NULL
    for (i in 1:bootstrap_iters) {
        df <- dplyr::sample_n(data.frame(df), nrow(df), replace = TRUE)
        generated_prds[i] <- mean(df[[ratio_col]], na.rm = TRUE)/stats::weighted.mean(df[[ratio_col]], df[[sale_price_col]], 
            na.rm = TRUE)
    }
    prd_output <- c(round(mean(generated_prds, na.rm = TRUE), 4), round(stats::sd(generated_prds, na.rm = TRUE), 4))
    return(prd_output)
}

prb_func <- function(df, ratio_col, assessed_value_col, sale_price_col, bootstrap_iters) {
    # create dataframe for regression
    reg_data <- data.frame(ratio = df[[ratio_col]], sale_price = df[[sale_price_col]], fitted_value = df[[assessed_value_col]], 
        log2 = log(2))
    
    prb_formula <- stats::lm(((ratio - median(ratio))/median(ratio)) ~ I(log(0.5 * (sale_price + fitted_value/median(ratio)))/log2), 
        data = reg_data, na.action = stats::na.exclude)
    prb_output <- c(round(summary(prb_formula)$coefficients[2], 4), round(summary(prb_formula)$coefficients[2, "Std. Error"], 
        4))
    return(prb_output)
}

get_stats <- function(df, bootstrap_iters) {
    cod_calcs <- cod_func(df, "RATIO", bootstrap_iters)
    prd_calcs <- prd_func(df, "RATIO", "SALE_PRICE", bootstrap_iters)
    prb_calcs <- prb_func(df, "RATIO", "ASSESSED_VALUE", "SALE_PRICE", bootstrap_iters)
    
    stats <- data.frame(N = nrow(df), COD = cod_calcs[1], COD_SE = cod_calcs[2], PRD = prd_calcs[1], PRD_SE = prd_calcs[2], 
        PRB = prb_calcs[1], PRB_SE = prb_calcs[2], q1_ratio = stats::quantile(df$RATIO)[[2]], median_ratio = stats::median(df$RATIO), 
        q3_ratio = stats::quantile(df$RATIO)[[4]], q1_sale = stats::quantile(df$SALE_PRICE)[[2]], median_sale = stats::median(df$SALE_PRICE), 
        q3_sale = stats::quantile(df$SALE_PRICE)[[4]], q1_assessed_value = stats::quantile(df$ASSESSED_VALUE)[[2]], median_assessed_value = stats::quantile(df$ASSESSED_VALUE)[[3]], 
        q3_assessed_value = stats::quantile(df$ASSESSED_VALUE)[[4]])
    return(stats)
}


#' Evaluates the accuracy of assessments.
#'
#' @param ratios A dataframe which has been pre-processed by \code{\link{reformat_data}}
#' @return Various statistics and facts on assessments by year

#' @export
calc_iaao_stats <- function(ratios) {
    stats <- data.frame()
    for (y in sort(unique(ratios$SALE_YEAR))) {
        mini_df <- ratios[ratios["SALE_YEAR"] == y, ]
        new <- get_stats(mini_df, 100)
        new["Year"] <- y
        stats <- rbind(stats, new)
    }
    return(stats)
}

