# extract coefficients
model_estimates <- function(model) {
    options(digits = 4)
    if (class(model)[1] %in% c("glm", "lm", "vglm",
        "LORgee")) {
        coef <- coef(summary(model)) %>%
            as.data.frame() %>%
           dplyr::add_rownames()
        coef <- coef %>%
            `colnames<-`(c("variable", "estimate", "se",
                "z value", "p value"))
        coef <- coef %>%
            mutate(., `signif code` = ifelse(`p value` <
                0.001, "***", ifelse(`p value` < 0.01,
                "**", ifelse(`p value` < 0.05, "*",
                  ""))))
        # coef = coef %>% mutate(., estimate =
        # paste0(estimate, ' (', se,')'), `p
        # value` = paste0(`p value`, ' ', `signif
        # code`)) coef = coef %>%
        # dplyr::select(-se, - `signif code`)
        coef <- coef %>%
            mutate(across(where(is.numeric), round,
                4))
    }
    if (class(model)[1] %in% c("glimML")) {
        coef <- summary(model)@Coef %>%
            as.data.frame() %>%
           dplyr::add_rownames()
        coef <- coef %>%
            `colnames<-`(c("variable", "estimate", "se",
                "z value", "p value"))
        coef <- coef %>%
            mutate(., `signif code` = ifelse(`p value` <
                0.001, "***", ifelse(`p value` < 0.01,
                "**", ifelse(`p value` < 0.05, "*",
                  ""))))
        # coef = coef %>% mutate(., estimate =
        # paste0(estimate, ' (', se,')'), `p
        # value` = paste0(`p value`, ' ', `signif
        # code`)) coef = coef %>%
        # dplyr::select(-se, - `signif code`)
        coef <- coef %>%
            mutate(across(where(is.numeric), round,
                4))
    }


    if (class(model)[1] %in% c("gee")) {
        coef <- coef(summary(model)) %>%
            as.data.frame() %>%
           dplyr::add_rownames() %>%
            dplyr::select(!!c(1:2, 6)) %>%
            `colnames<-`(c("variable", "estimate", "se"))
        coef <- coef %>%
            mutate(across(where(is.numeric), round,
                4))
    }


    # return model specific coefficients
    return(assign(paste("coef", deparse(substitute(model)),
        sep = "_"), coef, envir = .GlobalEnv))
}
# model_estimates(mfit_m4)

# get working correlation for gee models
working_corr <- function(model) {
    if (class(model)[1] %in% "gee") {
        y <- summary(model)$working.correlation[1, 2]

    }

    if (class(model)[1] %in% "glimML") {
        y <- summary(model)@Phi[1, 1]

    }
    return(y)
}


# modify output

model_print = function(model) {
    if (!grepl("gee", deparse(substitute(model)))) {
        x <- model %>%
            mutate(., estimate = paste0(estimate, "(",
                se, ")", " ", `signif code`), )
    } else {
        x <- model %>%
            mutate(., estimate = paste0(estimate, "(",
                se, ")"))

    }

    x <- x %>%
        dplyr::select(variable, estimate)
    return(assign(paste("print", deparse(substitute(model)),
        sep = "_"), x, envir = .GlobalEnv))
}

# overdispersion

extract_overdispersion = function(model) {
    y <- tibble(variable = "overdispersion", estimate = "None")
    y <- y %>%
        mutate(., estimate = !!paste0("param", "=",
            round(working_corr(model), 4)))
    return(y)
}

# function for aic and bic extraction
extract_aic_bic = function(model,
                   model_name) {
  aic_bic = tibble(
    Model = c(model_name),
    `-2 log L` = -2 * logLik(model),
    AIC = round(AICvlm(model), 2),
    BIC = round(BICvlm(model), 2)
  )

  return(aic_bic)
}


# rank aic and bic
model_rank = function(x) {
  y = x %>% mutate(rank_aic = rank(AIC),
                   rank_bic = rank(BIC)) %>% as.data.frame()
  y = y %>% mutate(AIC = paste0(AIC, '(', rank_aic, ')'),
                   BIC = paste0(BIC, '(', rank_bic, ')')) %>%
    dplyr::select(-rank_aic,-rank_bic)
  names(y) = c('Model', "-2 log L", 'AIC(rank)', 'BIC(rank)')
  # return(y)
  return(assign(paste('model_rank', deparse(
    substitute(x)
  ), sep = "_"), y, envir = .GlobalEnv))
}
