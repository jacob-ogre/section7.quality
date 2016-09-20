# BSD_2_clause

#' Run binomial GLM for overall quality of consultations
#'
#' @param x The 'combo' data.frame of all consultations
#' @return A list of results including:
#'   \describe{
#'     \item{mods}{Nine GLM (binomial) model objects}
#'     \item{AICs}{AICc values for the nine models}
#'     \item{summaries}{Results from \code{summary(mod)} for the nine models}
#'     \item{aovs}{Analysis of Variance for the nine models}
#'   }
#' @export
#' @examples
#' \dontrun{
#' run_glm_combo(combo)
#' }
run_glm_combo <- function(x) {
  resp <- cbind(as.integer(x$Sum_Q), x$Points_Possible)
  mod_1 <- glm(resp ~ Service + Formal + Year + Action_type + Programmatic + total_duration,
               data = x,
               family = binomial)
  mod_2 <- glm(resp ~ Service + Formal + Year + Programmatic + total_duration,
               data = x,
               family = binomial)
  mod_3 <- glm(resp ~ Service + Formal + Year + Action_type + total_duration,
               data = x,
               family = binomial)
  mod_4 <- glm(resp ~ Service + Formal + Year + total_duration,
               data = x,
               family = binomial)
  mod_5 <- glm(resp ~ Service * Formal,
               data = x,
               family = binomial)
  mod_6 <- glm(resp ~ Service,
               data = x,
               family = binomial)
  mod_7 <- glm(resp ~ Formal,
               data = x,
               family = binomial)
  mod_8 <- glm(resp ~ total_duration,
               data = x,
               family = binomial)
  mod_9 <- glm(resp ~ Service + Formal + Programmatic + total_duration,
               data = x,
               family = binomial)

  mods <- list(m1 <- mod_1,
               m2 <- mod_2,
               m3 <- mod_3,
               m4 <- mod_4,
               m5 <- mod_5,
               m6 <- mod_6,
               m7 <- mod_7,
               m8 <- mod_8,
               m9 <- mod_9)
  AICs <- list(m1 <- AICc(mod_1),
               m2 <- AICc(mod_2),
               m3 <- AICc(mod_3),
               m4 <- AICc(mod_4),
               m5 <- AICc(mod_5),
               m6 <- AICc(mod_6),
               m7 <- AICc(mod_7),
               m8 <- AICc(mod_8),
               m9 <- AICc(mod_9))

  sums <- list(m1 <- summary(mod_1),
               m2 <- summary(mod_2),
               m3 <- summary(mod_3),
               m4 <- summary(mod_4),
               m5 <- summary(mod_5),
               m6 <- summary(mod_6),
               m7 <- summary(mod_7),
               m8 <- summary(mod_8),
               m9 <- summary(mod_9))
  aovs <- list(m1 <- aov(mod_1),
               m2 <- aov(mod_2),
               m3 <- aov(mod_3),
               m4 <- aov(mod_4),
               m5 <- aov(mod_5),
               m6 <- aov(mod_6),
               m7 <- aov(mod_7),
               m8 <- aov(mod_8),
               m9 <- aov(mod_9))
  return(list(mods = mods, AICs = AICs, summaries = sums, aovs = aovs))
}

#' Return conf. intervals for Odds Ratios of GLM
#'
#' @param x A model of class
#' @return A table of odds ratios and 95% CI
#' @seealso if any see alsos
#' @export
#' @examples
#' \dontrun{
#' get_ORs_CIs(mod1)
#' }
get_ORs_CIs <- function(x) {
    res <- exp(cbind(OR = coef(x), confint(x)))
    return(res)
}

#' Run ordinal linear regression on consultation quality components
#'
#' @param x The 'combo' data.frame of all consultations
#' @param y The response variable (quality component) for the models
#' @return A list of results including:
#'   \describe{
#'     \item{mods}{Four GLM (binomial) model objects}
#'     \item{AICs}{AICc values for the four models}
#'     \item{summaries}{Results from \code{summary(mod)} for the four models}
#'     \item{aovs}{Analysis of Variance for the four models}
#'   }
#' @export
#' @examples
#' \dontrun{
#' run_polrs(formal, formal$status_Q)
#' }
run_polrs <- function(x, y) {
  mod1 <- polr(as.factor(y) ~ Service + Programmatic + Year,
               data = x,
               Hess = TRUE)
  mod2 <- polr(as.factor(y) ~ Service + Programmatic,
               data = x,
               Hess = TRUE)
  mod3 <- polr(as.factor(y) ~ Service,
               data = x,
               Hess = TRUE)
  mod4 <- polr(as.factor(y) ~ Programmatic,
               data = x,
               Hess = TRUE)

  mods <- list(m1 = mod1,
               m2 = mod2,
               m3 = mod3,
               m4 = mod4)
  AICs <- list(m1 = AICc(mod1),
               m2 = AICc(mod2),
               m3 = AICc(mod3),
               m4 = AICc(mod4))
  sums <- list(m1 = summary(mod1),
               m2 = summary(mod2),
               m3 = summary(mod3),
               m4 = summary(mod4))
  coef <- list(m1 = coeftest(mod1),
               m2 = coeftest(mod2),
               m3 = coeftest(mod3),
               m4 = coeftest(mod4))
    return(list(mods = mods, AICs = AICs, summaries = sums, coef = coef))
}

#' Return conf. intervals for Odds Ratios of GLM (alt version)
#'
#' Unclear why this alternative version exists...
#'
#' @param x A model of class
#' @return A table of odds ratios and 95% CI
#' @export
#' @examples
#' \dontrun{
#' get_ORs_CIs_alt(mod1)
#' }
get_ORs_CIs_alt <- function(x) {
    res <- exp(cbind(OR = coef(x), confint.default(x)))
    return(res)
}

#' Run binomial GLM for informal consultation quality components
#'
#' @param x The "informal" data.frame
#' @param y The response variable
#' @return A list of results including:
#'   \describe{
#'     \item{mods}{Four GLM (binomial) model objects}
#'     \item{AICs}{AICc values for the four models}
#'     \item{summaries}{Results from \code{summary(mod)} for the four models}
#'     \item{aovs}{Analysis of Variance for the four models}
#'   }
#' @export
#' @examples
#' inf_binom(informal, informal$map)
inf_binom <- function(x, y) {
    mod1 <- glm(y ~ Service + total_duration,
                data = x,
                family = binomial())
    mod2 <- glm(y ~ Service * total_duration,
                data = x,
                family = binomial())
    mod3 <- glm(y ~ Service,
                data = x,
                family = binomial())
    mod4 <- glm(y ~ total_duration,
                data = x,
                family = binomial())

    mods <- list(m1 = mod1,
                 m2 = mod2,
                 m3 = mod3,
                 m4 = mod4)
    AICs <- list(m1 = AICc(mod1),
                 m2 = AICc(mod2),
                 m3 = AICc(mod3),
                 m4 = AICc(mod4))
    sums <- list(m1 = summary(mod1),
                 m2 = summary(mod2),
                 m3 = summary(mod3),
                 m4 = summary(mod4))
    coef <- list(m1 = coeftest(mod1),
                 m2 = coeftest(mod2),
                 m3 = coeftest(mod3),
                 m4 = coeftest(mod4))
    return(list(mods = mods, AICs = AICs, summaries = sums, coef = coef))
}

#' Run binomial GLM for overall consultation specificity
#'
#' @param x The data.frame on which to run the nine candidate models
#' @return A list of results including:
#'   \describe{
#'     \item{mods}{Nine GLM (binomial) model objects}
#'     \item{AICs}{AICc values for the nine models}
#'     \item{summaries}{Results from \code{summary(mod)} for the nine models}
#'     \item{aovs}{Analysis of Variance for the nine models}
#'   }
#' @export
#' @examples
#' run_glm_Ospec(formal)
run_glm_Ospec <- function(x) {
    resp <- cbind(as.integer(x$Specificity_Sum), x$Specificity_PP)
    mod_1 <- glm(resp ~ Service + Year + Action_type + Programmatic + total_duration,
                data = x,
                family = binomial)
    mod_2 <- glm(resp ~ Service + Year + Programmatic + total_duration,
                data = x,
                family = binomial)
    mod_3 <- glm(resp ~ Service + Year + total_duration,
                data = x,
                family = binomial)
    mod_4 <- glm(resp ~ Service + total_duration,
                data = x,
                family = binomial)
    mod_5 <- glm(resp ~ Service * Year,
                data = x,
                family = binomial)
    mod_6 <- glm(resp ~ Service,
                data = x,
                family = binomial)
    mod_7 <- glm(resp ~ Year,
                data = x,
                family = binomial)
    mod_8 <- glm(resp ~ total_duration,
                data = x,
                family = binomial)
    mod_9 <- glm(resp ~ Service + Programmatic + total_duration,
                data = x,
                family = binomial)

    mods <- list(m1 <- mod_1,
                 m2 <- mod_2,
                 m3 <- mod_3,
                 m4 <- mod_4,
                 m5 <- mod_5,
                 m6 <- mod_6,
                 m7 <- mod_7,
                 m8 <- mod_8,
                 m9 <- mod_9)
    AICs <- list(m1 <- AICc(mod_1),
                 m2 <- AICc(mod_2),
                 m3 <- AICc(mod_3),
                 m4 <- AICc(mod_4),
                 m5 <- AICc(mod_5),
                 m6 <- AICc(mod_6),
                 m7 <- AICc(mod_7),
                 m8 <- AICc(mod_8),
                 m9 <- AICc(mod_9))

    sums <- list(m1 <- summary(mod_1),
                 m2 <- summary(mod_2),
                 m3 <- summary(mod_3),
                 m4 <- summary(mod_4),
                 m5 <- summary(mod_5),
                 m6 <- summary(mod_6),
                 m7 <- summary(mod_7),
                 m8 <- summary(mod_8),
                 m9 <- summary(mod_9))
    aovs <- list(m1 <- aov(mod_1),
                 m2 <- aov(mod_2),
                 m3 <- aov(mod_3),
                 m4 <- aov(mod_4),
                 m5 <- aov(mod_5),
                 m6 <- aov(mod_6),
                 m7 <- aov(mod_7),
                 m8 <- aov(mod_8),
                 m9 <- aov(mod_9))
    return(list(mods = mods, AICs = AICs, summaries = sums, aovs = aovs))
}
