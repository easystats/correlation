#' Adjust data for the effect of other variable(s)
#'
#' This function can be used to adjust the data for the effect of other variables present in the dataset. It is based on an underlying fitting of regressions models, allowing for quite some flexibility, such as including factors as random effects in mixed models (multilevel partialization), continuous variables as smooth terms in general additive models (non-linear partialization) and/or fitting these models under a Bayesian framework. The values returned by this function are the residuals of the regression models. Note that a regular correlation between two "adjusted" variables is equivalent to the partial correlation between them.
#'
#' @param data A dataframe.
#' @param effect Character vector of column names to be adjusted for (regressed out). If \code{NULL} (the default), all variables will be selected.
#' @inheritParams standardize
#' @param multilevel If \code{TRUE}, the factors are included as random factors. Else, if \code{FALSE} (default), they are included as fixed effects in the simple regression model.
#' @param additive If \code{TRUE}, continuous variables as included as smooth terms in additive models. The goal is to regress-out potential non-linear effects.
#' @param bayesian If \code{TRUE}, the models are fitted under the Bayesian framework using \code{rstanarm}.
#'
#' @examples
#' adjust(iris, effect = "Species", select = "Sepal.Length")
#' \donttest{
#' adjust(iris, effect = "Species", select = "Sepal.Length", multilevel = TRUE)
#' adjust(iris, effect = "Species", select = "Sepal.Length", bayesian = TRUE)
#' adjust(iris, effect = "Petal.Width", select = "Sepal.Length", additive = TRUE)
#' adjust(iris, effect = "Petal.Width", select = "Sepal.Length",
#'        additive = TRUE, bayesian = TRUE)
#' adjust(iris, effect = c("Petal.Width", "Species"), select = "Sepal.Length",
#'        multilevel = TRUE, additive = TRUE)
#' adjust(iris)
#' }
#'
#' @export
adjust <- function(data, effect = NULL, select = NULL, exclude = NULL, multilevel = FALSE, additive = FALSE, bayesian = FALSE){

  # Find predictors
  if(is.null(effect)){
    effect <- names(data)
  }

  # Factors
  formula_random <- NULL
  facs <- names(data[effect][!sapply(data[effect], is.numeric)])
  if (length(facs) >= 1){
    if(multilevel){
      if(additive){
        formula_random <- as.formula(paste("~", paste(paste0("(1|", facs, ")"), collapse = " + ")))
      } else{
        formula_random <- paste("+", paste(paste0("(1|", facs, ")"), collapse = " + "))
      }
      effect <- effect[!effect %in% facs]
    }
  }

  nums <- sapply(data, is.numeric)
  # Find outcomes
  if(is.null(select)){
    select <- names(data[nums])
  }
  if(!is.null(exclude)){
    select <- select[!select %in% c(exclude)]
  }

  # Fit models
  out <- data.frame(.ID = 1:nrow(data))
  for(var in select){
    predictors <- effect[effect != var]
    if(additive){
      predictors_num <- names(data[predictors][sapply(data[predictors], is.numeric)])
      predictors[predictors == predictors_num] <- paste0("s(", predictors_num, ")")
    }
    formula_predictors <- paste(c("1", predictors), collapse = " + ")
    formula <- paste(var, "~", formula_predictors)

    x <- .model_adjust_for(data=data[unique(c(var, effect, facs))], formula, multilevel = multilevel, additive = additive, bayesian = bayesian, formula_random = formula_random)
    out[var] <- x
  }
  out[names(data)[!names(data) %in% names(out)]] <- data[names(data)[!names(data) %in% names(out)]]
  out[names(data)]
}




#' @importFrom stats lm residuals as.formula complete.cases
#' @keywords internal
.model_adjust_for <- function(data, formula, multilevel = FALSE, additive = FALSE, bayesian = FALSE, formula_random = NULL) {

  # Additive -----------------------
  if(additive){
    # Bayesian
    if(bayesian){
      if (!requireNamespace("rstanarm")) {
        stop("This function needs `rstanarm` to be installed. Please install by running `install.packages('rstanarm')`.")
      }
      adjusted <- rstanarm::stan_gamm4(as.formula(formula), random = formula_random, data = data, refresh = 0)$residuals
      # Frequentist
    } else{
      if (!requireNamespace("gamm4")) {
        stop("This function needs `gamm4` to be installed. Please install by running `install.packages('gamm4')`.")
      }
      adjusted <- gamm4::gamm4(as.formula(formula), random = formula_random, data = data)$gam$residuals
    }

  # Linear -------------------------
  } else{
    # Bayesian
    if(bayesian){
      if (!requireNamespace("rstanarm")) {
        stop("This function needs `rstanarm` to be installed. Please install by running `install.packages('rstanarm')`.")
      }
      if (multilevel) {
        adjusted <- rstanarm::stan_lmer(paste(formula, formula_random), data = data, refresh = 0)$residuals
      } else{
        adjusted <- rstanarm::stan_glm(formula, data = data, refresh = 0)$residuals
      }
    # Frequentist
    } else{
      if (multilevel) {
        if (!requireNamespace("lme4")) {
          stop("This function needs `lme4` to be installed. Please install by running `install.packages('lme4')`.")
        }
        adjusted <- residuals(lme4::lmer(paste(formula, formula_random), data = data))
      } else{
        adjusted <- lm(formula, data = data)$residuals
      }
    }
  }

  # Deal with missing data
  out <- rep(NA, nrow(data))
  out[complete.cases(data)] <- as.vector(adjusted)

  out
}
