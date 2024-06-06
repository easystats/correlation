#' Correlation test
#'
#' This function performs a correlation test between two variables.
#' You can easily visualize the result using [`plot()`][visualisation_recipe.easycormatrix()] (see examples [**here**](https://easystats.github.io/correlation/reference/visualisation_recipe.easycormatrix.html#ref-examples)).
#'
#' @param x,y Vectors of the two variables the correlation test is done for.
#'   \cr Alternatively, can be names of variables in `data`.
#' @param data An optional data frame.
#' @param ci Confidence/Credible Interval level. If `"default"`, then it is
#'   set to `0.95` (`95%` CI).
#' @param method A character string indicating which correlation coefficient is
#'   to be used for the test. \cr Possible Values: `"pearson"` (default),
#'   `"kendall"`, `"spearman"`, `"biserial"`, `"point-biserial"`, `"rankbiserial"`,
#'   `"polychoric"`, `"tetrachoric"`, `"biweight"`, `"distance"`, `"percentage"`
#'   (for percentage bend correlation), `"blomqvist"` (for Blomqvist's
#'   coefficient), `"hoeffding"` (for Hoeffding's D), `"gamma"`, `"gaussian"`
#'   (for Gaussian Rank correlation), `"shepherd"` (for Shepherd's Pi correlation).
#'   \cr (polychoric when ordinal factors involved, tetrachoric when dichotomous
#'   factors involved, point-biserial if one dichotomous and one continuous and
#'   pearson otherwise). See below the **details** section for a description of
#'   these indices.
#' @param bayesian If `TRUE`, will run the correlations under a Bayesian
#'   framework.
#' @param verbose Toggle warnings.
#' @param ... Optional arguments:
#'  - `data` A data frame (when `x` and/or `y` are not vectors).
#'  - Arguments dependent on `method` being:
#'    - `"kendall"`:
#'      - `tau_type` = `"b"`
#'      - `direction` = `"row"` (used when `tau_type` = `"a"`)
#'    - `"distance"`:
#'      - `corrected` = `TRUE`
#'    - `"percentage"`:
#'      - `beta` = `0.2`
#'    - `"bayes"`:
#'      - `bayesian_prior` = "medium"
#'      - `bayesian_ci_method` = "hdi"
#'      - `bayesian_test` = `c("pd", "rope", "bf")`
#'
#'
#' @details
#'
#' ## Correlation Types
#' - **Pearson's correlation**: This is the most common correlation method. It
#' corresponds to the covariance of the two variables normalized (i.e., divided)
#' by the product of their standard deviations.
#'
#' - **Spearman's rank correlation**: A non-parametric measure of rank
#' correlation (statistical dependence between the rankings of two variables).
#' \cr The Spearman correlation between two variables is equal to the Pearson
#' correlation between the rank values of those two variables; while Pearson's
#' correlation assesses linear relationships, Spearman's correlation assesses
#' monotonic relationships (whether linear or not). \cr Confidence Intervals
#' (CI) for Spearman's correlations are computed using the Fieller et al. (1957)
#' correction (see Bishara and Hittner, 2017).
#'
#' - **Kendall's rank correlation**: Is used to quantify the association between
#' two variables based on the ranks of their data points. \cr It comes in three
#' variants which provide different approaches for handling tied ranks, allowing
#' for robust assessments of association across different datasets and
#' scenarios. \cr Confidence Intervals (CI) for Kendall's correlations are
#' computed using the Fieller et al. (1957) correction (see Bishara and Hittner,
#' 2017). \cr The three variants are: tau-a, tau-b (default), and tau-c.
#'   - **Tau-a** doesn't account for ties and calculates the difference between
#'   the proportions of concordant and discordant pairs.
#'   - **Tau-b** adjusts for ties by incorporating a correction factor, ensuring
#'   a more accurate measure of association.
#'   - **Tau-c**, similar to tau-b, considers ties, but it only adjusts for
#'   pairs where both variables have tied ranks.
#'
#' - **Biweight midcorrelation**: A measure of similarity that is
#' median-based, instead of the traditional mean-based, thus being less
#' sensitive to outliers. \cr It can be used as a robust alternative to other
#' similarity metrics, such as Pearson correlation (Langfelder & Horvath,
#' 2012).
#'
#' - **Distance correlation**: Distance correlation measures both
#' linear and non-linear association between two random variables or random
#' vectors. \cr This is in contrast to Pearson's correlation, which can only detect
#' linear association between two random variables.
#'
#' - **Percentage bend correlation**: Introduced by Wilcox (1994), it
#' is based on a down-weight of a specified percentage of marginal observations
#' deviating from the median (by default, `20%`).
#'
#' - **Shepherd's Pi correlation**: Equivalent to a Spearman's rank
#' correlation after outliers removal (by means of bootstrapped Mahalanobis
#' distance).
#'
#' - **Blomqvist’s coefficient**: The Blomqvist’s coefficient (also
#' referred to as Blomqvist's Beta or medial correlation; Blomqvist, 1950) is a
#' median-based non-parametric correlation that has some advantages over
#' measures such as Spearman's or Kendall's estimates (see Shmid & Schimdt,
#' 2006).
#'
#' - **Hoeffding’s D**: The Hoeffding’s D statistics is a non-parametric rank
#' based measure of association that detects more general departures from
#' independence (Hoeffding 1948), including non-linear associations. \cr
#' Hoeffding’s D varies between -0.5 and 1 (if there are no tied ranks,
#' otherwise it can have lower values), with larger values indicating a stronger
#' relationship between the variables.
#'
#' - **Somers’ D**: The Somers’ D statistics is a non-parametric rank
#' based measure of association between a binary variable and a continuous
#' variable, for instance, in the context of logistic regression the binary
#' outcome and the predicted probabilities for each outcome. \cr Usually,
#' Somers' D is a measure of ordinal association, however, this implementation
#' it is limited to the case of a binary outcome.
#'
#' - **Point-Biserial, Rank-Biserial and biserial correlation**: Correlation
#' coefficient used when one variable is continuous and the other is dichotomous
#' (binary). \cr Point-Biserial is equivalent to a Pearson's correlation, while
#' Biserial should be used when the binary variable is assumed to have an
#' underlying continuity. For example, anxiety level can be measured on a
#' continuous scale, but can be classified dichotomously as high/low. \cr
#' Rank-Biserial is also equivalent to a Pearson's correlation, but it is used
#' when the continuous variable is ordinal, and the dichotomous variable is
#' assumed to have any relation to the order of the ordinal variable rather than
#' it's value.
#'
#' - **Gamma correlation**: The Goodman-Kruskal gamma statistic is similar to
#' Kendall's Tau coefficient. It is relatively robust to outliers and deals well
#' with data that have many ties.
#'
#' - **Gaussian rank Correlation**: The Gaussian rank correlation estimator is a
#' simple and well-performing alternative for robust rank correlations (Boudt et
#' al., 2012). \cr It is based on the Gaussian quantiles of the ranks.
#'
#' - **Polychoric correlation**: Correlation between two theorized normally
#' distributed continuous latent variables, from two observed ordinal variables.
#'
#' - **Tetrachoric correlation**: Special case of the polychoric correlation
#' applicable when both observed variables are dichotomous.
#'
#' ## Confidence Intervals
#'
#' For correlation methods that do not have a direct parametric method of
#' obtaining _p_-values and CIs, we use [cor_to_p] and [cor_to_ci].
#'
#' @examples
#' library(correlation)
#' data("iris")
#'
#' cor_test(iris$Sepal.Length, iris$Sepal.Width) # method = "pearson"
#' # or
#' cor_test("Sepal.Length", "Sepal.Width", data = iris) # method = "pearson"
#' cor_test("Sepal.Length", "Sepal.Width", data = iris, method = "spearman")
#' \donttest{
#' cor_test("Sepal.Length", "Sepal.Width", data = iris, method = "kendall")
#' cor_test("Sepal.Length", "Sepal.Width", data = iris, method = "biweight")
#' cor_test("Sepal.Length", "Sepal.Width", data = iris, method = "distance")
#' cor_test("Sepal.Length", "Sepal.Width", data = iris, method = "percentage")
#' cor_test("Sepal.Length", "Sepal.Width", data = iris, method = "blomqvist")
#' cor_test("Sepal.Length", "Sepal.Width", data = iris, method = "gamma")
#' cor_test("Sepal.Length", "Sepal.Width", data = iris, method = "gaussian")
#' cor_test("Sepal.Length", "Sepal.Width", data = iris, method = "shepherd")
#'
#' if (require("Hmisc", quietly = TRUE)) {
#'   cor_test("Sepal.Length", "Sepal.Width", data = iris, method = "hoeffding")
#' }
#'
#' if (require("BayesFactor", quietly = TRUE)) {
#'   cor_test("Sepal.Length", "Sepal.Width", data = iris, bayesian = TRUE)
#' }
#'
#' # Tetrachoric, Polychoric, and Biserial
#' if (require("psych", quietly = TRUE) && require("rstanarm", quietly = TRUE)) {
#'   data("mtcars")
#'   mtcars$am <- factor(mtcars$am, levels = 0:1)
#'   mtcars$vs <- factor(mtcars$vs, levels = 0:1)
#'   mtcars$cyl <- ordered(mtcars$cyl, levels = c(4, 6, 8))
#'   mtcars$carb <- ordered(mtcars$carb, , levels = c(1:4, 6, 8))
#'
#'   # Tetrachoric
#'   cor_test(mtcars$am, mtcars$vs, method = "tetrachoric")
#'
#'   # Biserial
#'   cor_test(mtcars$mpg, mtcars$am, method = "biserial")
#'
#'   # Polychoric
#'   cor_test(mtcars$cyl, mtcars$carb, method = "polychoric")
#'
#'   # When one variable is continuous, will run 'polyserial' correlation
#'   cor_test(mtcars$cyl, mtcars$mpg, method = "polychoric")
#' }
#' }
#' @export
cor_test <- function(x, y,
                     data = NULL,
                     method = "pearson",
                     ci = 0.95,
                     alternative = "two.sided",
                     bayesian = FALSE,
                     verbose = TRUE,
                     ...) {
  # +======+
  #  checks
  # +======+

  # check value of method
  method <- match.arg(tolower(method), c("pearson", "spearman", "spear", "s",
                                         "kendall", "biserial", "pointbiserial",
                                         "point-biserial", "rankbiserial",
                                         "rank-biserial", "biweight", "distance",
                                         "percentage", "percentage_bend",
                                         "percentagebend", "pb", "blomqvist",
                                         "median", "medial", "hoeffding", "gamma",
                                         "gaussian", "shepherd", "sheperd",
                                         "shepherdspi", "pi",  "somers", "poly",
                                         "polychoric", "tetra", "tetrachoric"))
  methodUse <- ifelse(method %in% c("pearson", "spearman", "spear", "s"), "frequantive", method)
  methodUse <- ifelse(method %in% c("pointbiserial", "point-biserial"), "point-biserial", methodUse)
  methodUse <- ifelse(method %in% c("rankbiserial", "rank-biserial"), "rank-biserial", methodUse)
  methodUse <- ifelse(method %in% c("percentage", "percentage_bend", "percentagebend", "pb"), "percentage", methodUse)
  methodUse <- ifelse(method %in% c("blomqvist", "median", "medial"), "blomqvist", methodUse)
  methodUse <- ifelse(method %in% c("shepherd", "sheperd", "shepherdspi", "pi"), "shepherd", methodUse)
  methodUse <- ifelse(method %in% c("poly", "polychoric"), "polychoric", methodUse)
  methodUse <- ifelse(method %in% c("tetra", "tetrachoric"), "tetrachoric", methodUse)

  # vectors or names check
  xIsName <- is.character(x) && (length(x) == 1L)
  yIsName <- is.character(y) && (length(y) == 1L)

  x_name <- ifelse(xIsName, x, deparse(substitute(x)))
  y_name <- ifelse(yIsName, y, deparse(substitute(y)))

  # if x,y are names, get them from data.frame
  if (xIsName != yIsName) {
    insight::format_error("Both x,y must be vectors or valid names on columns in data.")
  } else if (xIsName) {
    if (is.data.frame(data) && all(c(x, y) %in% colnames(data))) {
      x <- data[[x]]
      y <- data[[y]]
    } else if (is.null(data)) {
      insight::format_error("No data frame has been provided.")
    } else if (!is.data.frame(data)) {
      insight::format_error("The data provided is not a data frame.")
    } else {
      insight::format_error(paste0(x, " or ", y, "not found in data."))
    }
  }

  # get complete cases
  oo <- stats::complete.cases(x, y)
  var_x <- x[oo]
  var_y <- y[oo]

  # check validity of the amount of observations
  if (length(var_x) < 3L) {
    insight::format_alert(paste(x, "and", y, "have less than 3 complete observations."))
  }

  # Make sure x,y are not factor(s)
  if (!methodUse %in% c("tetrachoric", "polychoric")) {
    var_x <- datawizard::to_numeric(var_x, dummy_factors = FALSE)
    var_y <- datawizard::to_numeric(var_y, dummy_factors = FALSE)
  }

  # check value of ci (confidence level)
  if (ci == "default") {
    ci <- 0.95
  } else if (!is.null(ci)) {
    if (length(ci) != 1L || ci <= 0 || ci >= 1)
      stop("The confidence level (ci) is not between 0 and 1")
  }

  # check value of alternative
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  # check value of tau_type and direction when relevant
  if(method == "kendall") {
    tau_type <- "b"
    direction <- "row"
    if ("tau_type" %in% names(list(...))) {
      tau_type <- match.arg(tolower(list(...)$tau_type), c("a", "b", "c"))
    }
    if ("direction" %in% names(list(...))) {
      direction <- match.arg(tolower(list(...)$direction), c("row", "column"))
    }
  }

  if (methodUse == "distance") {
    corrected <- TRUE
    if ("corrected" %in% names(list(...))) {
      corrected <- list(...)$corrected
    }
  }

  if(methodUse == "percentage") {
    beta <- 0.2
    if("beta" %in% names(list(...))) {
      beta <- list(...)$beta
      if (length(beta) != 1L || beta <= 0 || beta >= 0.5) {
        stop("The bend criterion (beta) is not between 0 and 0.5")
      }
    }
  }

  if(bayesian) {
    bayesian_prior <- "medium"
    bayesian_ci_method <- "hdi"
    bayesian_test <- c("pd", "rope", "bf")

    if("bayesian_prior" %in% names(list(...))) {
      bayesian_prior <- match.arg(tolower(list(...)$bayesian_prior),
                                  c("medium", "medium.narrow", "wide", "ultra-wide"))
    }

    if ("bayesian_ci_method" %in% names(list(...))) {
      bayesian_ci_method <- list(...)$bayesian_ci_method
    }

    if ("bayesian_test" %in% names(list(...))) {
      bayesian_test <- list(...)$bayesian_test
    }
  }

  # +=======================+
  #  calculate by the method
  # +=======================+

  # when bayesian
  if(bayesian) {
    if (methodUse %in% c("kendall", "biserial", "point-biserial", "rank-biserial", "biweight", "distance", "percentage", "gamma", "somers", "polychoric", "tetrachoric"))
      insight::format_error(paste0("The bayesian form of ", toupper(methodUse[1]), methodUse[-1], " correlation method is not supported yet. Get in touch if you want to contribute."))
    if (methodUse %in% c("blomqvist", "hoeffding"))
      insight::format_error(paste0("Bayesian ", toupper(methodUse[1]), methodUse[-1], ifelse(methodUse == "hoeffding", "'s", ""), "correlations are not supported yet. Check-out the BBcor package (https://github.com/donaldRwilliams/BBcor)."))
    out <- .cor_test_bayes(var_x, var_y, ci, method, ...)
    out$Parameter1 <- x_name
    out$Parameter2 <- y_name
  }
  else {
    out <- switch(methodUse,
                  "frequantive" = .cor_test_freq(var_x, var_y, ci, alternative, method, ...),
                  "kendall" = .cor_test_kendall(var_x, var_y, ci, alternative, tau_type, direction, ...),
                  "biserial" = .cor_test_biserial(var_x, var_y, ci, alternative, xType = "base", ...),
                  "point-biserial" = .cor_test_biserial(var_x, var_y, ci, alternative, xType = "point", ...),
                  "rank-biserial" = .cor_test_biserial(var_x, var_y, ci, alternative, xType = "rank", ...),
                  "biweight" = .cor_test_biweight(var_x, var_y, ci, alternative, ...),
                  "distance" = .cor_test_distance(var_x, var_y, ci, alternative, corrected, ...),
                  "percentage" = .cor_test_percentage(var_x, var_y, ci, alternative, beta, ...),
                  "blomqvist" = .cor_test_freq(sign(var_x - median(var_x)), sign(var_y - median(var_y)), ci, alternative, ...),
                  "hoeffding" = .cor_test_hoeffding(var_x, var_y, ci, alternative, ...),
                  "gamma" = .cor_test_gamma(var_x, var_y, ci, alternative, ...),
                  "gaussian" = .cor_test_freq(stats::qnorm(rank(var_x) / (length(var_x) + 1)), stats::qnorm(rank(var_y) / (length(var_y) + 1)), ci, alternative, ...),
                  "shepherd" = .cor_test_shepherd(var_x, var_y, ci, alternative, ...),
                  "somers" = .cor_test_somers(var_x, var_y, ci, alternative, ...),
                  "polychoric" = .cor_test_polychoric(var_x, var_y, ci, alternative, ...),
                  "tetrachoric" = .cor_test_tetrachoric(var_x, var_y, ci, alternative, ...))
    out$Parameter1 <- x_name
    out$Parameter2 <- y_name
  }

  if (!"Method" %in% names(out)) {
    out$Method <- methodUse
  }
  out$Method <- paste0(out$Method, ifelse(bayesian, " (Bayesian)", ""))

  # Reorder columns
  order <- c("Parameter1", "Parameter2", "r", "rho", "tau", "Dxy", "CI", "CI_low", "CI_high", "Method")
  out <- out[c(order[order %in% names(out)], setdiff(names(out), order[order %in% names(out)]))]

  attr(out, "method") <- out$Method
  attr(out, "coefficient_name") <- c("r", "rho", "tau", "Dxy")[c("r", "rho", "tau", "Dxy") %in% names(out)][1]
  attr(out, "ci") <- ci
  if ("data" %in% list(...)) attr(out, "data") <- data
  class(out) <- unique(c("easycor_test", "easycorrelation", "prameters_model", class(out)))
  out
}


#  Corr methods -------------------

# pearson and spearman calc function
#' @keywords internal
.cor_test_freq <- function(var_x, var_y,
                           ci = 0.95,
                           alternative = "two.sided",
                           method = "pearson",
                           ...) {
  # calculating the pearson or spearman correlation coefficient
  r <- cor(var_x, var_y, method = method)
  # calculating the degrees of freedom, t-value and p-value
  df <- length(var_x) - 2
  t_p <- .t_p_value(r, df, alternative)
  # creating output dataframe
  out <- data.frame("r" = r,
                    "df_error" = df,
                    "t" = t_p[1],
                    "p" = t_p[2],
                    "Method" = method)
  # calculating the confidence interval
  if (!is.null(ci)) {
    CI <- switch(alternative,
                 "two.sided" = .ci_value(r, c(-1, 1), (1 + ci) / 2, df),
                 "less" = c(-Inf, .ci_value(r, 1, ci, df)),
                 "greater" = c(.ci_value(r, -1, ci, df), Inf))
    out$CI <- ci
    out$CI_low <- CI[1]
    out$CI_high <- CI[2]
  }
  # returning output
  out
}

# kendall's tau calc function
#' @keywords internal
.cor_test_kendall <- function(var_x, var_y,
                              ci = 0.95,
                              alternative = "two.sided",
                              tau_type = "b",
                              direction = "row",
                              ...) {
  tab <- table(var_x, var_y)
  n <- length(var_x)
  # calculating the concordant and discordant pairs amounts within the data and across it
  ConDisParams <- DescTools::ConDisPairs(tab)[3:4]
  # calculating kendall's tau
  tau <- switch(tau_type,
                "a" = DescTools::KendallTauA(var_x, var_y, direction = direction, conf.level = ci),
                "b" = DescTools::KendallTauB(var_x, var_y, conf.level = ci),
                "c" = DescTools::StuartTauC(var_x, var_y, conf.level = ci))
  CI <- tau[2:3]
  tau <- tau[[1]]
  # calculating the z-value according to the tau_type required
  if(tau_type != "a") {
    xi <- rowSums(tab)
    yj <- colSums(tab)
    vx <- sum(xi * (xi - 1) * (2 * xi + 5))
    vy <- sum(yj * (yj - 1) * (2 * yj + 5))
    v1 <- sum(xi * (xi - 1)) * sum(yj * (yj - 1)) / (2 * n * (n - 1))
    v2 <- sum(xi * (xi - 1) * (xi - 2)) * sum(yj * (yj - 1) * (yj - 2)) / (9 * n * (n - 1) * (n - 2))
    v <- (n * (n - 1) * (2 * n + 5) - vx - vy)/18 + v1 + v2
    z <- (ConDisParams$C- ConDisParams$D) / sqrt(v)
  }
  else {
    z <- (ConDisParams$C - ConDisParams$D) / sqrt(n * (n - 1) * (2 * n + 5) / 18)
  }
  # calculating the p-value
  p <- switch(alternative,
              "two.sided" = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
              "less" = stats::pnorm(z),
              "greater" = stats::pnorm(z, lower.tail = FALSE))
  # creating output dataframe
  out <- data.frame("tau" = tau,
                    "z" = z,
                    "p" = p,
                    "df_error" = NA)
  sd <- (CI[2] - tau) / qnorm((1 + ci) / 2)
  # calculating the confidence interval
  if (!is.null(ci)) {
    CI <- switch(alternative,
                 "two.sided" = CI,
                 "less" = c(-Inf, tau + qnorm(ci) * sd),
                 "greater" = c(tau - qnorm(ci) * sd, Inf))
    out$CI <- ci
    out$CI_low <- CI[1]
    out$CI_high <- CI[2]
  }
  # returning output
  out
}

# biserial correlation calc function
#' @keywords internal
.cor_test_biserial <- function(var_x, var_y,
                               ci = 0.95,
                               alternative = "two.sided",
                               xType = "base",
                               ...) {
  xVartype <- .vartype(var_x)
  yVartype <- .vartype(var_y)

  if (!xVartype$is_binary == !yVartype$is_binary) insight::format_error("Biserial correlation can noly be applied for one dichotomous variable and one continuous variable.")
  else if (xVartype$is_binary)
  {
    temp <- var_x
    var_x <- var_y
    var_y <- temp
    temp <- xVartype
    xVartype <- yVartype
    yVartype <- temp
  }

  if (yVartype$is_factor || yVartype$is_character) var_y <- as.numeric(var_y)
  var_y <- as.vector((var_y - min(var_y, na.rm = TRUE)) / (diff(range(var_y, na.rm = TRUE))))

  if (xType == "point") {
    out <- .cor_test_freq(var_x, var_y, ci, alternative)
    out$Method <- "Point Biserial"
  }

  else {
    # calculating helping values
    n <- length(var_x)
    m0 <- mean(var_x[var_y == 0])
    m1 <- mean(var_x[var_y == 1])
    q <- mean(var_y)

    # calculating coefficient
    r <- switch(xType,
                "base" = ((m1 - m0) * (1 - q) * q / stats::dnorm(stats::qnorm(q))) / stats::sd(var_x),
                "rank" = 2 * (m1 - m0) / n)

    # calculating the degrees of freedom, t-value and p-value
    df <- n - 2
    t_p <- .t_p_value(r, df, alternative)

    # creating output dataframe
    out <- data.frame("r" = r,
                      "df_error" = df,
                      "t" = t_p[1],
                      "p" = t_p[2],
                      "Method" = switch(xType,
                                        "base" = "Biserial",
                                        "rank" = "Rank Biserial"))

    # calculating the confidence interval
    if (!is.null(ci)) {
      CI <- switch(alternative,
                   "two.sided" = .ci_value(r, c(-1, 1), (1 + ci) / 2, df),
                   "less" = c(-Inf, .ci_value(r, 1, ci, df)),
                   "greater" = c(.ci_value(r, -1, ci, df), Inf))
      out$CI <- ci
      out$CI_low <- CI[1]
      out$CI_high <- CI[2]
    }
  }

  # returning output
  out
}

# biweight midcorrelation calc function
#' @keywords internal
.cor_test_biweight <- function(var_x, var_y,
                               ci = 0.95,
                               alternative = "two.sided",
                               ...) {
  # finding helping values
  xb <- (var_x - median(var_x)) / (9 * mad(var_x, constant = 1))
  yb <- (var_y - median(var_y)) / (9 * mad(var_y, constant = 1))
  wx <- (1 - xb ^ 2) ^ 2 * (1 - abs(xb) > 0)
  wy <- (1 - yb ^ 2) ^ 2 * (1 - abs(yb) > 0)
  xDnm <- sqrt(sum(((var_x - median(var_x)) * wx) ^ 2))
  yDnm <- sqrt(sum(((var_y - median(var_y)) * wy) ^ 2))

  # finding x Tilda and y Tilda for use infinal calculation
  xTil <- ((var_x - median(var_x)) * wx) / xDnm
  yTil <- ((var_y - median(var_y)) * wy) / yDnm

  # calculating the coefficient
  r <- sum(xTil * yTil)
  # calculating the degrees of freedom, t-value and p-value
  df <- length(var_x) - 2
  t_p <- .t_p_value(r, df, alternative)
  # creating output dataframe
  out <- data.frame("r" = r,
                    "df_error" = df,
                    "t" = t_p[1],
                    "p" = t_p[2])
  # calculating the confidence interval
  if (!is.null(ci)) {
    CI <- switch(alternative,
                 "two.sided" = .ci_value(r, c(-1, 1), (1 + ci) / 2, df),
                 "less" = c(-Inf, .ci_value(r, 1, ci, df)),
                 "greater" = c(.ci_value(r, -1, ci, df), Inf))
    out$CI <- ci
    out$CI_low <- CI[1]
    out$CI_high <- CI[2]
  }
  # returning output
  out
}

# distance correlation calc function (same as original, with little bit of tweaks)
#' @keywords internal
.cor_test_distance <- function(var_x, var_y,
                               ci = 0.95,
                               alternative = "two.sided",
                               corrected = TRUE,
                               ...) {
  if (!corrected) {
    n <- length(var_x)
    if("index" %in% names(list(...))) {
      if (index < 0 || index > 2) {
        insight::format_error("`index` must be between 0 and 2.")
        index <- 1.0
      }
    }
    else index <- 1.0

    var_x <- as.matrix(stats::dist(var_x))
    var_y <- as.matrix(stats::dist(var_y))

    A <- .A_kl(var_x, index)
    B <- .A_kl(var_y, index)

    V <- sqrt(sqrt(mean(A * A)) * sqrt(mean(B * B)))
    if (V > 0) {
      r <- sqrt(mean(A * B)) / V
    } else {
      r <- 0
    }

    df = n - 2
    t_p <- .t_p_value(r, df, alternative)
    if (!is.null(ci)) {
      CI <- switch(alternative,
                   "two.sided" = .ci_value(r, c(-1, 1), (1 + ci) / 2, df),
                   "less" = c(-Inf, .ci_value(r, 1, ci, df)),
                   "greater" = c(.ci_value(r, -1, ci, df), Inf))
    }

    rez <- data.frame("r" = r,
                      "df_error" = df,
                      "t" = t_p[1],
                      "p" = t_p[2],
                      "CI" = ci,
                      "CI_low" = CI[1],
                      "CI_high" = CI[2])
  }
  else {
    var_x <- as.matrix(stats::dist(var_x))
    var_y <- as.matrix(stats::dist(var_y))
    n <- nrow(var_x)

    A <- .A_star(var_x)
    B <- .A_star(var_y)

    XY <- (sum(A * B) - (n / (n - 2)) * sum(diag(A * B))) / n^2
    XX <- (sum(A * A) - (n / (n - 2)) * sum(diag(A * A))) / n^2
    YY <- (sum(B * B) - (n / (n - 2)) * sum(diag(B * B))) / n^2

    r <- XY / sqrt(XX * YY)

    # due to the fact that the calculation of distance correlation is based on
    # every pair of samples, the degrees freedom increases combinatorially, and
    # it is calculated as:
    #
    # df <- n * (n - 3) / 2 - 1
    #
    # but, for the simplicity of the function and its results across all types
    # of correlations, we will keep the degrees of freedom as it is usually
    # calculated.

    df <- n - 2
    t_p <- .t_p_value(r, df, alternative)

    # calculating the confidence interval
    if (!is.null(ci)) {
      CI <- switch(alternative,
                   "two.sided" = .ci_value(r, c(-1, 1), (1 + ci) / 2, df),
                   "less" = c(-Inf, .ci_value(r, 1, ci, df)),
                   "greater" = c(.ci_value(r, -1, ci, df), Inf))
    }

    rez <- data.frame("r" = r,
                      "df_error" = df,
                      "t" = t_p[1],
                      "p" = t_p[2],
                      "CI" = ci,
                      "CI_low" = CI[1],
                      "CI_high" = CI[2],
                      "Method" = "Distance (Bias Corrected)")
  }

  rez
}

# percentage bend correlation calc function
#' @keywords internal
.cor_test_percentage <- function(var_x, var_y,
                                 ci = 0.95,
                                 alternative = "two.sided",
                                 beta = 0.2,
                                 ...) {
  # finding helping values
  ohmX <- .ohmhat(var_x, beta)
  ohmY <- .ohmhat(var_y, beta)
  pbosX <- .pbos(var_x, beta)
  pbosY <- .pbos(var_y, beta)
  # finding a and b values
  a <- (var_x - pbosX) / ohmX
  b <- (var_y - pbosY) / ohmY
  a <- ifelse(a < -1, -1, ifelse(a > 1, 1, a))
  b <- ifelse(b < -1, -1, ifelse(b > 1, 1, b))
  # calculating the coefficient
  r <- sum(a * b) / sqrt(sum(a ^ 2) * sum(b ^ 2))
  # calculating the degrees of freedom, t-value and p-value
  df <- length(var_x) - 2
  t_p <- .t_p_value(r, df, alternative)
  # creating output dataframe
  out <- data.frame("r" = r,
                    "df_error" = df,
                    "t" = t_p[1],
                    "p" = t_p[2])
  # calculating the confidence interval
  if (!is.null(ci)) {
    CI <- switch(alternative,
                 "two.sided" = .ci_value(r, c(-1, 1), (1 + ci) / 2, df),
                 "less" = c(-Inf, .ci_value(r, 1, ci, df)),
                 "greater" = c(.ci_value(r, -1, ci, df), Inf))
    out$CI <- ci
    out$CI_low <- CI[1]
    out$CI_high <- CI[2]
  }
  # returning output
  out
}

# hoeffding's D correlation calc function (same as original, with little bit of tweaks)
#' @keywords internal
.cor_test_hoeffding <- function(var_x, var_y,
                                ci = 0.95,
                                alternative = "two.sided",
                                ...) {
  insight::check_if_installed("Hmisc", "for 'hoeffding' correlations")

  rez <- Hmisc::hoeffd(var_x, var_y)

  df = length(var_x) - 2
  t_p <- .t_p_value(rez$D[2, 1], df, alternative)
  if (!is.null(ci)) {
    CI <- switch(alternative,
                 "two.sided" = .ci_value(rez$D[2, 1], c(-1, 1), (1 + ci) / 2, df),
                 "less" = c(-Inf, .ci_value(rez$D[2, 1], 1, ci, df)),
                 "greater" = c(.ci_value(rez$D[2, 1], -1, ci, df), Inf))
  }

  data.frame("r" = rez$D[2, 1],
             "df_error" = length(var_x) - 2,
             "t" = t_p[1],
             "p" = rez$P[2, 1],
             "CI" = ci,
             "CI_low" = CI[1],
             "CI_high" = CI[2])
}

# gamma correlation calc function (almost the same as original)
#' @keywords internal
.cor_test_gamma <- function(var_x, var_y,
                            ci = 0.95,
                            alternative = "two.sided",
                            ...) {
  ConDisField <- outer(var_x, var_x, function(x1, x2) sign(x1 - x2)) * outer(var_y, var_y, function(y1, y2) sign(y1 - y2))
  r <- sum(ConDisField) / sum(abs(ConDisField))
  # calculating the degrees of freedom, t-value and p-value
  df <- length(var_x) - 2
  t_p <- .t_p_value(r, df, alternative)
  # creating output dataframe
  out <- data.frame("r" = r,
                    "df_error" = df,
                    "t" = t_p[1],
                    "p" = t_p[2])
  # calculating the confidence interval
  if (!is.null(ci)) {
    CI <- switch(alternative,
                 "two.sided" = .ci_value(r, c(-1, 1), (1 + ci) / 2, df),
                 "less" = c(-Inf, .ci_value(r, 1, ci, df)),
                 "greater" = c(.ci_value(r, -1, ci, df), Inf))
    out$CI <- ci
    out$CI_low <- CI[1]
    out$CI_high <- CI[2]
  }
  # returning output
  out
}

# Shepherd's Pi calc function
#' @keywords internal
.cor_test_shepherd <- function(var_x, var_y,
                               ci = 0.95,
                               alternative = "two.sided",
                               bayesian = FALSE,
                               ...) {
  # finding outliers using bootstraped mahalanobis
  d <- .robust_bootstrap_mahalanobis(cbind(var_x, var_y))
  outliers <- d >= 6
  out <- .cor_test_freq(var_x[!outliers], var_y[!outliers], ci, alternative, "spearman")
  out$Method <- "Shepherd's Pi"

  # returning output
  out
}

# somers' D correlation calc function (same as original, with little bit of tweaks)
#' @keywords internal
.cor_test_somers <- function(var_x, var_y,
                             ci = 0.95,
                             alternative = "two.sided",
                             ...) {
  insight::check_if_installed("Hmisc", "for 'somers' correlations")

  xVartype <- .vartype(var_x)
  yVartype <- .vartype(var_y)

  if (!xVartype$is_binary == !yVartype$is_binary) insight::format_error("Somers' D can noly be applied for one dichotomous variable and one continuous variable.")
  else if (xVartype$is_binary)
  {
    temp <- var_x
    var_x <- var_y
    var_y <- temp
  }

  rez <- Hmisc::somers2(var_x, var_y)

  df = length(var_x) - 2
  t_p <- .t_p_value(rez["Dxy"], df, alternative)
  if (!is.null(ci)) {
    CI <- switch(alternative,
                 "two.sided" = .ci_value(rez["Dxy"], c(-1, 1), (1 + ci) / 2, df),
                 "less" = c(-Inf, .ci_value(rez["Dxy"], 1, ci, df)),
                 "greater" = c(.ci_value(rez["Dxy"], -1, ci, df), Inf))
  }

  data.frame("Dxy" = rez["Dxy"],
             "df_error" = length(var_x) - 2,
             "t" = t_p[1],
             "p" = t_p[2],
             "CI" = ci,
             "CI_low" = CI[1],
             "CI_high" = CI[2],
             "Method" = "Somers' D")
}

# polychoric correlation calc function (same as original, with little bit of tweaks)
#' @keywords internal
.cor_test_polychoric <- function(var_x, var_y,
                                 ci = 0.95,
                                 alternative = "two.sided",
                                 ...) {
  insight::check_if_installed("psych", "for 'polychoric' correlations")

  # valid matrix check
  if (!is.factor(var_x) && !is.factor(var_y)) insight::format_error("Polychoric correlations can only be ran on ordinal factors.")

  if (!is.factor(var_x) || !is.factor(var_y)) {
    insight::check_if_installed("polycor", "for 'polyserial' correlations")

    r <- polycor::polyserial(
      x = if (is.factor(var_x)) as.numeric(var_y) else as.numeric(var_x),
      y = if (is.factor(var_x)) as.numeric(var_x) else as.numeric(var_y)
    )
    method <- "Polyserial"
  } else {
    # Reconstruct dataframe
    dat <- data.frame(as.numeric(var_x), as.numeric(var_y))
    junk <- utils::capture.output({
      r <- suppressWarnings(psych::polychoric(dat)$rho[2, 1])
    })
    method <- "Polychoric"
  }

  # calculating the degrees of freedom, t-value and p-value
  df <- length(var_x) - 2
  t_p <- .t_p_value(r, df, alternative)
  # creating output dataframe
  out <- data.frame("r" = r,
                    "df" = df,
                    "t" = t_p[1],
                    "p" = t_p[2],
                    "Method" = method)
  # calculating the confidence interval
  if (!is.null(ci)) {
    CI <- switch(alternative,
                 "two.sided" = .ci_value(r, c(-1, 1), (1 + ci) / 2, df),
                 "less" = c(-Inf, .ci_value(r, 1, ci, df)),
                 "greater" = c(.ci_value(r, -1, ci, df), Inf))
    out$CI <- ci
    out$CI_low <- CI[1]
    out$CI_high <- CI[2]
  }
  # returning output
  out
}

# tetrachoric correlation calc function (same as original, with little bit of tweaks)
#' @keywords internal
.cor_test_tetrachoric <- function(var_x, var_y,
                                  ci = 0.95,
                                  alternative = "two.sided",
                                  ...) {
  insight::check_if_installed("psych", "for 'tetrachoric' correlations")

  # valid matrix check
  if (length(unique(var_x)) > 2 && length(unique(var_y)) > 2) insight::format_error("Tetrachoric correlations can only be ran on dichotomous data.")

  # Reconstruct dataframe
  dat <- data.frame(var_x, var_y)

  junk <- utils::capture.output(r <- psych::tetrachoric(dat)$rho[2, 1]) # nolint

  # calculating the degrees of freedom, t-value and p-value
  df <- length(var_x) - 2
  t_p <- .t_p_value(r, df, alternative)
  # creating output dataframe
  out <- data.frame("r" = r,
                    "df_error" = df,
                    "t" = t_p[1],
                    "p" = t_p[2])
  # calculating the confidence interval
  if (!is.null(ci)) {
    CI <- switch(alternative,
                 "two.sided" = .ci_value(r, c(-1, 1), (1 + ci) / 2, df),
                 "less" = c(-Inf, .ci_value(r, 1, ci, df)),
                 "greater" = c(.ci_value(r, -1, ci, df), Inf))
    out$CI <- ci
    out$CI_low <- CI[1]
    out$CI_high <- CI[2]
  }
  # returning output
  out
}

# bayesian frequentist calc function (same as original, with little bit of tweaks)
.cor_test_bayes <- function(var_x, var_y,
                            ci = 0.95,
                            method = "pearson",
                            bayesian_prior = "medium",
                            bayesian_ci_method = "hdi",
                            bayesian_test = c("pd", "rope", "bf"),
                            ...) {
  insight::check_if_installed("BayesFactor")

  if (all(var_x == var_y)) insight::format_error("The two variables must be different.")

  method_label <- "Bayesian Pearson"
  method <- tolower(method)
  if (method %in% c("spearman", "spear", "s")) {
    var_x <- datawizard::ranktransform(var_x, method = "average")
    var_y <- datawizard::ranktransform(var_y, method = "average")
    metho_label <- "Bayesian Spearman"
  } else if (method == "gaussian") {
    var_x <- stats::qnorm(rank(var_x) / (length(var_x) + 1))
    var_y <- stats::qnorm(rank(var_y) / (length(var_y) + 1))
    method_label <- "Bayesian Gaussian"
  } else if (method %in% c("shepherd", "sheperd", "shepherdspi", "pi")) {
    d <- .robust_bootstrap_mahalanobis(cbind(var_x, var_y))
    outliers <- d >= 6

    var_x <- datawizard::ranktransform(var_x[!outliers], method = "average")
    var_y <- datawizard::ranktransform(var_y[!outliers], method = "average")

    method_label <- "Bayesian Shepherd's Pi"
  }

  out <- .cor_test_bayes_base(
    var_x,
    var_y,
    ci = ci,
    bayesian_prior = bayesian_prior,
    bayesian_ci_method = bayesian_ci_method,
    bayesian_test = bayesian_test,
    ...
  )

  # Add method
  out$Method <- method_label
  out
}


#  internal helping functions --------------------

# confidence interval calculation
#' @keywords internal
.ci_value <- function(r, side, ci, df) {
  z_fisher(z = z_fisher(r = r) + side * stats::qnorm(ci) / sqrt(df - 1))
}

# t-value & p-value calculation
#' @keywords internal
.t_p_value <- function(r, df, alternative) {
  t <- r * sqrt(df / (1 - r ^ 2))
  p <- switch(alternative,
              "two.sided" = 2 * stats::pt(abs(t), df, lower.tail = FALSE),
              "less" = stats::pt(t, df),
              "greater" = stats::pt(t, df, lower.tail = FALSE))
  c(t, p)
}

# Specific helpers -----------------------

##  distance============

#' @keywords internal
.A_kl <- function(x, index) {
  d <- as.matrix(x)^index
  m <- rowMeans(d)
  M <- mean(d)
  a <- sweep(d, 1, m)
  b <- sweep(a, 2, m)
  (b + M)
}

#' @keywords internal
.A_star <- function(d) {
  ## d is a distance matrix or distance object
  ## modified or corrected doubly centered distance matrices
  ## denoted A* (or B*) in JMVA t-test paper (2013)
  d <- as.matrix(d)
  n <- nrow(d)
  if (n != ncol(d)) stop("Argument d should be distance", call. = FALSE)
  m <- rowMeans(d)
  M <- mean(d)
  a <- sweep(d, 1, m)
  b <- sweep(a, 2, m)
  A <- b + M # same as plain A
  # correction to get A^*
  A <- A - d / n
  diag(A) <- m - M
  (n / (n - 1)) * A
}

##  percentage bend============

# ohmhat calculation
#' @keywords internal
.ohmhat <- function(x, beta) sort(abs(x - median(x)))[floor((1 - beta) * length(x))]

# pbos calculation
#' @keywords internal
.pbos <- function(x, beta) {
  ohmhat <- .ohmhat(x, beta)
  psi <- (x - median(x)) / ohmhat
  i1 <- length(psi[psi < -1])
  i2 <- length(psi[psi > 1])
  sx <- ifelse(psi < -1, 0, ifelse(psi > 1, 0, x))
  (sum(sx) + ohmhat * (i2 - i1)) / (length(x) - i1 - i2)
}

## shepherd's D============

# robust bootstrap mahalanobis calculation
#' @keywords internal
.robust_bootstrap_mahalanobis <- function(data, iterations = 1000) {
  Ms <- replicate(n = iterations, {
    # Draw random numbers from 1:n with replacement
    idx <- sample(nrow(data), replace = TRUE)
    # Resample data
    dat <- data[idx, ]
    # Calculating the Mahalanobis distance for each actual observation using resampled data
    stats::mahalanobis(data, center = colMeans(dat), cov = stats::cov(dat))
  })

  apply(Ms, 1, stats::median)
}

## biserial============

#' @keywords internal
.vartype <- function(x) {
  out <- list(
    is_factor = FALSE,
    is_numeric = FALSE,
    is_character = FALSE,
    is_binary = FALSE,
    is_continuous = FALSE,
    is_count = FALSE
  )

  if (is.factor(x)) out$is_factor <- TRUE
  if (is.character(x)) out$is_character <- TRUE
  if (is.numeric(x)) out$is_numeric <- TRUE
  if (length(unique(x)) == 2) out$is_binary <- TRUE
  if (out$is_numeric && !out$is_binary) out$is_continuous <- TRUE
  if (all(x %% 1 == 0)) out$is_count <- TRUE

  out
}

## bayes============

#' @keywords internal
.cor_test_bayes_base <- function(var_x,
                                 var_y,
                                 ci = 0.95,
                                 bayesian_prior = "medium",
                                 bayesian_ci_method = "hdi",
                                 bayesian_test = c("pd", "rope", "bf"),
                                 ...) {
  insight::check_if_installed("BayesFactor")

  rez <- BayesFactor::correlationBF(var_x, var_y, rscale = bayesian_prior)
  params <- parameters::model_parameters(
    rez,
    dispersion = FALSE,
    ci_method = bayesian_ci_method,
    test = bayesian_test,
    rope_range = c(-0.1, 0.1),
    rope_ci = 1,
    ...
  )
  # validation check: do we have a BF column?
  if (is.null(params$BF)) {
    params$BF <- NA
  }

  # Rename coef
  if (sum(names(params) %in% c("Median", "Mean", "MAP")) == 1) {
    names(params)[names(params) %in% c("Median", "Mean", "MAP")] <- "rho"
  }

  # Remove useless columns
  params[names(params) %in% c("Effects", "Component")] <- NULL

  # returning output
  params[names(params) != "Parameter"]
}
