#' Offers various tests for regressivity.
#'
#' @param property_data a dataframe with assessment, sales, and time data which has been processed by reformat_data
#' @param return_model_objs boolean, if true will return a list of model objects
#' #' @return a list of model objects or a summary data.frame of results

#'@export
vertical_equity_metrics <- function(property_data, return_model_objs = FALSE) {

    property_data <- property_data %>% dplyr::group_by(TAX_YEAR) %>% dplyr::mutate(SALE_PRICE_SQD = SALE_PRICE^2, price_tercile = dplyr::ntile(SALE_PRICE,
        3), av_tercile = dplyr::ntile(ASSESSED_VALUE, 3), low = ifelse(price_tercile == 1, 1, 0), high = ifelse(price_tercile ==
        3, 1, 0), Z = ifelse(price_tercile == 1 & av_tercile == 1, -1, ifelse(price_tercile == 3 & av_tercile == 3, 1, 0))) %>%
        dplyr::filter(ASSESSED_VALUE != 0 & SALE_PRICE != 0)

    model_ls <- c("paglin72", "cheng74", "IAAO78", "kochin82", "bell84", "sunderman90", "clapp90")
    results_df <- data.frame(Model = character(), coef_value = numeric(), test = character(), coef_t_stat = numeric(), conclusion = character())

    paglin72 <- lm(ASSESSED_VALUE ~ SALE_PRICE, data = property_data)
    cheng74 <- lm(log(ASSESSED_VALUE) ~ log(SALE_PRICE), data = property_data)
    IAAO78 <- lm(RATIO ~ SALE_PRICE, data = property_data)
    kochin82 <- lm(log(SALE_PRICE) ~ log(ASSESSED_VALUE), data = property_data)
    bell84 <- lm(ASSESSED_VALUE ~ SALE_PRICE + SALE_PRICE_SQD, data = property_data)
    sunderman90 <- lm(ASSESSED_VALUE ~ SALE_PRICE + low + high + low * SALE_PRICE + high * SALE_PRICE, data = property_data)
    clapp90 <- AER::ivreg(log(SALE_PRICE) ~ log(ASSESSED_VALUE) | Z, data = property_data)

    for (name in model_ls) {

        if (name == "paglin72") {
            mini <- summary(paglin72)$coefficients
            target_coef <- mini[1, 1]
            target_coef_t_stat <- mini[1, 3]
            regressivity_tst <- target_coef > 0
            test <- "> 0"
            description <- "AV ~ SP"
        } else if (name == "cheng74") {
            mini <- summary(cheng74)$coefficients
            target_coef <- mini[2, 1]
            target_coef_t_stat <- mini[2, 3]
            regressivity_tst <- target_coef < 1
            test <- "< 1"
            description <- "ln(AV) ~ ln(SP)"
        } else if (name == "IAAO78") {
            mini <- summary(IAAO78)$coefficients
            target_coef <- mini[2, 1]
            target_coef_t_stat <- mini[2, 3]
            regressivity_tst <- target_coef < 0
            test <- "< 0"
            description <- "RATIO ~ SP"
        } else if (name == "kochin82") {
            mini <- summary(kochin82)$coefficients
            target_coef <- mini[2, 1]
            target_coef_t_stat <- mini[2, 3]
            regressivity_tst <- target_coef < 1
            test <- "< 1"
            description <- "ln(SP) ~ ln(AV)"
        } else if (name == "bell84") {
            mini <- summary(bell84)$coefficients
            target_coef <- mini[1, 1]
            target_coef_t_stat <- c(mini[1, 3], mini[3, 3])
            target_coef_2 <- mini[3, 1]
            regressivity_tst <- target_coef > 0 & target_coef_2 < 0
            target_coef <- c(target_coef, target_coef_2)
            test <- c("> 0", "< 0")
            description <- "AV ~ SP + SP^2"
            name <- c("bell84", "")
        } else if (name == "sunderman90") {
            mini <- summary(sunderman90)$coefficients
            target_coef <- mini[3, 1]
            target_coef_t_stat <- mini[3, 3]
            regressivity_tst <- target_coef > 0
            test <- c("> 0")
            description <- "AV ~ SP + low + high + low * SP + high * SP"
            name <- c("sunderman90")
        } else if (name == "clapp90") {
            mini <- summary(clapp90)$coefficients
            target_coef <- mini[2, 1]
            target_coef_t_stat <- mini[2, 3]
            regressivity_tst <- target_coef > 1
            test <- c("> 1")
            description <- "ln(SP) ~ ln(AV) -> ln(AV) ~ Z"
        }
        if (regressivity_tst) {
            conclusion <- "Regressive"
        } else {
            conclusion <- "Progressive"
        }

        if (min(abs(target_coef_t_stat)) < 2.5) {
            conclusion <- "Not Significant"
        }

        results_df <- rbind(results_df, data.frame(name = name, coef_value = target_coef, test = test, coef_t_stat = target_coef_t_stat,
            conclusion = conclusion, description = description))
    }
    names(results_df) <- c("Model", "Value", "Test", "T Statistic", "Conclusion", "Model Description")

    if (return_model_objs) {
        return(list(paglin72, cheng74, IAAO78, kochin82, bell84, sunderman90, clapp90))
    } else {
        return(results_df)
    }
}
