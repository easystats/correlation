# Correlation test

This function performs a correlation test between two variables. You can
easily visualize the result using
[`plot()`](https://easystats.github.io/correlation/reference/visualisation_recipe.easycormatrix.md)
(see examples
[**here**](https://easystats.github.io/correlation/reference/visualisation_recipe.easycormatrix.html#ref-examples)).

## Usage

``` r
cor_test(
  data,
  x,
  y,
  method = "pearson",
  ci = 0.95,
  bayesian = FALSE,
  bayesian_prior = "medium",
  bayesian_ci_method = "hdi",
  bayesian_test = c("pd", "rope", "bf"),
  include_factors = FALSE,
  partial = FALSE,
  partial_bayesian = FALSE,
  multilevel = FALSE,
  ranktransform = FALSE,
  winsorize = FALSE,
  bootstrap = FALSE,
  iterations = 1000,
  cluster = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame.

- x, y:

  Names of two variables present in the data.

- method:

  A character string indicating which correlation coefficient is to be
  used for the test. One of `"pearson"` (default), `"kendall"`,
  `"spearman"` (but see also the `ranktransform` argument),
  `"biserial"`, `"polychoric"`, `"tetrachoric"`, `"biweight"`,
  `"distance"`, `"percentage"` (for percentage bend correlation),
  `"blomqvist"` (for Blomqvist's coefficient), `"hoeffding"` (for
  Hoeffding's D), `"gamma"`, `"gaussian"` (for Gaussian Rank
  correlation) or `"shepherd"` (for Shepherd's Pi correlation). Setting
  `"auto"` will attempt at selecting the most relevant method
  (polychoric when ordinal factors involved, tetrachoric when
  dichotomous factors involved, point-biserial if one dichotomous and
  one continuous and pearson otherwise). See below the **details**
  section for a description of these indices.

- ci:

  Confidence/Credible Interval level. If `"default"`, then it is set to
  `0.95` (`95%` CI).

- bayesian:

  If `TRUE`, will run the correlations under a Bayesian framework.

- bayesian_prior:

  For the prior argument, several named values are recognized:
  `"medium.narrow"`, `"medium"`, `"wide"`, and `"ultrawide"`. These
  correspond to scale values of `1/sqrt(27)`, `1/3`, `1/sqrt(3)` and
  `1`, respectively. See the
  [`BayesFactor::correlationBF`](https://rdrr.io/pkg/BayesFactor/man/correlationBF.html)
  function.

- bayesian_ci_method, bayesian_test:

  See arguments in
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  for `BayesFactor` tests.

- include_factors:

  If `TRUE`, the factors are kept and eventually converted to numeric or
  used as random effects (depending of `multilevel`). Ordered factors
  are converted to the ranks of their levels. If `FALSE`, factors are
  removed upfront.

- partial:

  Can be `TRUE` or `"semi"` for partial and semi-partial correlations,
  respectively.

- partial_bayesian:

  If partial correlations under a Bayesian framework are needed, you
  will also need to set `partial_bayesian` to `TRUE` to obtain "full"
  Bayesian partial correlations. Otherwise, you will obtain
  pseudo-Bayesian partial correlations (i.e., Bayesian correlation based
  on frequentist partialization).

- multilevel:

  If `TRUE`, the factors are included as random factors. Else, if
  `FALSE` (default), they are included as fixed effects in the simple
  regression model.

- ranktransform:

  If `TRUE`, will rank-transform the variables prior to estimating the
  correlation, which is one way of making the analysis more resistant to
  extreme values (outliers). Note that, for instance, a Pearson's
  correlation on rank-transformed data is equivalent to a Spearman's
  rank correlation. Thus, using `ranktransform=TRUE` and
  `method="spearman"` is redundant. Nonetheless, it is an easy option to
  increase the robustness of the correlation as well as flexible way to
  obtain Bayesian or multilevel Spearman-like rank correlations.

- winsorize:

  Another way of making the correlation more "robust" (i.e., limiting
  the impact of extreme values). Can be either `FALSE` or a number
  between 0 and 1 (e.g., `0.2`) that corresponds to the desired
  threshold. See the
  [`datawizard::winsorize()`](https://easystats.github.io/datawizard/reference/winsorize.html)
  function for more details.

- bootstrap:

  If `TRUE`, the confidence interval, standard error, and p-value are
  obtained from a nonparametric percentile bootstrap instead of the
  analytic formulas: rows are resampled with replacement `iterations`
  times and the coefficient is recomputed on each resample. See the
  'Bootstrap' section. Not available for Bayesian, partial, or
  multilevel correlations; the interval and p-value are two-sided, so
  `alternative` (passed through `...`) must be `"two.sided"`, the
  default.

- iterations:

  Number of bootstrap resamples (default `1000`). Only used when
  `bootstrap = TRUE` or `cluster` is given.

- cluster:

  Name of a column in `data` identifying clusters of rows (for example,
  participants with repeated measures). If given, a cluster bootstrap is
  run (`bootstrap` is set to `TRUE`): whole clusters, rather than rows,
  are resampled with replacement; rows with a missing cluster value are
  dropped. See the 'Bootstrap' section.

- verbose:

  Toggle warnings.

- ...:

  Additional arguments (e.g., `alternative`) to be passed to other
  methods. See
  [`stats::cor.test`](https://rdrr.io/r/stats/cor.test.html) for further
  details.

## Value

A data frame of class `easycor_test` with the coefficient, its
confidence interval, the test statistic, and the p-value. Under
bootstrap (`bootstrap = TRUE` or `cluster`), the object also has an `SE`
column and three attributes: `ci_method` (`"bootstrap"` or
`"cluster-bootstrap"`), `iterations` (the number of replicates kept
after dropping the failed ones), and `bootstrap_replicates` (the kept
replicate coefficients, from which `CI_low`, `CI_high`, `SE`, and,
except for distance correlation and Hoeffding's D, `p` are computed).
With fewer than three complete observations the bootstrap is skipped and
neither the `SE` column nor these attributes are added.

## Details

### Correlation Types

- **Pearson's correlation**: This is the most common correlation method.
  It corresponds to the covariance of the two variables normalized
  (i.e., divided) by the product of their standard deviations.

- **Spearman's rank correlation**: A non-parametric measure of rank
  correlation (statistical dependence between the rankings of two
  variables). The Spearman correlation between two variables is equal to
  the Pearson correlation between the rank values of those two
  variables; while Pearson's correlation assesses linear relationships,
  Spearman's correlation assesses monotonic relationships (whether
  linear or not). Confidence Intervals (CI) for Spearman's correlations
  are computed using the Fieller et al. (1957) correction (see Bishara
  and Hittner, 2017).

- **Kendall's rank correlation**: In the normal case, the Kendall
  correlation is preferred than the Spearman correlation because of a
  smaller gross error sensitivity (GES) and a smaller asymptotic
  variance (AV), making it more robust and more efficient. However, the
  interpretation of Kendall's tau is less direct than that of Spearman's
  rho, in the sense that it quantifies the difference between the
  percentage of concordant and discordant pairs among all possible
  pairwise events. Confidence Intervals (CI) for Kendall's correlations
  are computed using the Fieller et al. (1957) correction (see Bishara
  and Hittner, 2017).

- **Biweight midcorrelation**: A measure of similarity that is
  median-based, instead of the traditional mean-based, thus being less
  sensitive to outliers. It can be used as a robust alternative to other
  similarity metrics, such as Pearson correlation (Langfelder & Horvath,
  2012).

- **Distance correlation**: Distance correlation measures both linear
  and non-linear association between two random variables or random
  vectors. This is in contrast to Pearson's correlation, which can only
  detect linear association between two random variables.

- **Percentage bend correlation**: Introduced by Wilcox (1994), it is
  based on a down-weight of a specified percentage of marginal
  observations deviating from the median (by default, `20%`).

- **Shepherd's Pi correlation**: Equivalent to a Spearman's rank
  correlation after outliers removal (by means of bootstrapped
  Mahalanobis distance).

- **Blomqvist’s coefficient**: The Blomqvist’s coefficient (also
  referred to as Blomqvist's Beta or medial correlation;
  Blomqvist, 1950) is a median-based non-parametric correlation that has
  some advantages over measures such as Spearman's or Kendall's
  estimates (see Shmid & Schimdt, 2006). It is computed by
  [`wdm::wdm()`](https://tnagler.github.io/wdm-r/reference/wdm.html),
  which splits each variable at its median by average rank (observations
  with rank at or below `(n + 1) / 2` go in the lower half), so when an
  observation sits exactly at the split (an odd number of rows, or ties
  spanning the median) the coefficient is not exactly sign-symmetric.

- **Hoeffding’s D**: The Hoeffding’s D statistics is a non-parametric
  rank based measure of association that detects more general departures
  from independence (Hoeffding 1948), including non-linear associations.
  Hoeffding’s D varies between -0.5 and 1 (if there are no tied ranks,
  otherwise it can have lower values), with larger values indicating a
  stronger relationship between the variables.

- **Somers’ D**: The Somers’ D statistics is a non-parametric rank based
  measure of association between a binary variable and a continuous
  variable, for instance, in the context of logistic regression the
  binary outcome and the predicted probabilities for each outcome.
  Usually, Somers' D is a measure of ordinal association, however, this
  implementation it is limited to the case of a binary outcome.

- **Point-Biserial and biserial correlation**: Correlation coefficient
  used when one variable is continuous and the other is dichotomous
  (binary). Point-Biserial is equivalent to a Pearson's correlation,
  while Biserial should be used when the binary variable is assumed to
  have an underlying continuity. For example, anxiety level can be
  measured on a continuous scale, but can be classified dichotomously as
  high/low.

- **Gamma correlation**: The Goodman-Kruskal gamma statistic is similar
  to Kendall's Tau coefficient. It is relatively robust to outliers and
  deals well with data that have many ties.

- **Winsorized correlation**: Correlation of variables that have been
  formerly Winsorized, i.e., transformed by limiting extreme values to
  reduce the effect of possibly spurious outliers.

- **Gaussian rank Correlation**: The Gaussian rank correlation estimator
  is a simple and well-performing alternative for robust rank
  correlations (Boudt et al., 2012). It is based on the Gaussian
  quantiles of the ranks.

- **Polychoric correlation**: Correlation between two theorized normally
  distributed continuous latent variables, from two observed ordinal
  variables.

- **Tetrachoric correlation**: Special case of the polychoric
  correlation applicable when both observed variables are dichotomous.

### Partial Correlation

**Partial correlations** are estimated as the correlation between two
variables after adjusting for the (linear) effect of one or more other
variable. The correlation test is then run after having partialized the
dataset, independently from it. In other words, it considers
partialization as an independent step generating a different dataset,
rather than belonging to the same model. This is why some discrepancies
are to be expected for the t- and p-values, CIs, BFs etc (but *not* the
correlation coefficient) compared to other implementations (e.g.,
`ppcor`). (The size of these discrepancies depends on the number of
covariates partialled-out and the strength of the linear association
between all variables.) Such partial correlations can be represented as
Gaussian Graphical Models (GGM), an increasingly popular tool in
psychology. A GGM traditionally include a set of variables depicted as
circles ("nodes"), and a set of lines that visualize relationships
between them, which thickness represents the strength of association
(see Bhushan et al., 2019).

**Multilevel correlations** are a special case of partial correlations
where the variable to be adjusted for is a factor and is included as a
random effect in a mixed model (note that the remaining continuous
variables of the dataset will still be included as fixed effects,
similarly to regular partial correlations). The model is a random
intercept model, i.e. the multilevel correlation is adjusted for
`(1 | groupfactor)`.That said, there is an important difference between
using `cor_test()` and
[`correlation()`](https://easystats.github.io/correlation/reference/correlation.md):
If you set `multilevel=TRUE` in
[`correlation()`](https://easystats.github.io/correlation/reference/correlation.md)
but `partial` is set to `FALSE` (as per default), then a
back-transformation from partial to non-partial correlation will be
attempted (through
[`pcor_to_cor()`](https://easystats.github.io/correlation/reference/cor_to_pcor.md)).
However, this is not possible when using `cor_test()` so that if you set
`multilevel=TRUE` in it, the resulting correlations are partial one.
Note that for Bayesian multilevel correlations, if `partial = FALSE`,
the back transformation will also recompute *p*-values based on the new
*r* scores, and will drop the Bayes factors (as they are not relevant
anymore). To keep Bayesian scores, set `partial = TRUE`.

### Notes

Kendall and Spearman correlations when `bayesian=TRUE`: These are
technically Pearson Bayesian correlations of rank transformed data,
rather than pure Bayesian rank correlations (which have different
priors).

### Bootstrap confidence intervals

With `bootstrap = TRUE`, or when `cluster` names a column, the
confidence interval, standard error, and p-value of every non-Bayesian
method are obtained by resampling instead of from the analytic formulas
above. The rows with complete data on both variables (and on `cluster`,
if given) or, with `cluster`, whole clusters of such rows, are resampled
with replacement `iterations` times and the coefficient is recomputed on
each resample. `CI_low` and `CI_high` are the `(1 - ci) / 2` and
`1 - (1 - ci) / 2` quantiles of the replicates (the percentile interval;
Efron & Tibshirani, 1993, section 13.3, pp. 170-171); `SE` is the
standard deviation of the replicates (Efron & Tibshirani, 1993, eq. 6.6,
p. 47); and `p` is the two-sided achieved significance level of the same
replicate distribution,
`2 * min(sum(t <= 0) + 1, sum(t >= 0) + 1) / (B + 1)` capped at 1, where
`t` are the `B` kept replicates (Efron & Tibshirani, 1993, section 15.4,
pp. 214-215; the `+ 1` form is the Monte Carlo p-value of Davison &
Hinkley, 1997, eq. 4.11, p. 141). This p-value tests whether the
population coefficient is 0. It is approximate, somewhat above nominal
in small samples, and can never be smaller than `2 / (iterations + 1)`;
the `+ 1` makes it slightly conservative relative to the percentile
interval, so near the boundary an interval that excludes 0 can go with a
`p` just above `1 - ci`. The analytic test statistic and its degrees of
freedom are set to `NA`.

Distance correlation and Hoeffding's D are degenerate under
independence, so the bootstrap cannot test whether they are 0: their `p`
is `NA` under bootstrap (use the analytic test instead), and their
interval and standard error are reliable only when an association is
present.

Replicates on which the coefficient cannot be computed (for instance, a
resample in which a variable is constant or loses a category) are
dropped with a warning naming the number kept; if fewer than half
survive, an error is raised. In
[`correlation()`](https://easystats.github.io/correlation/reference/correlation.md),
this warning is shown for the first pair only, as for every other
warning it raises. The `winsorize` and `ranktransform` transformations
are applied once, to the full data, before resampling.

The cluster bootstrap (`cluster`) resamples whole clusters, so the
dependence within a cluster (repeated measures of one participant, say)
is kept intact. Its accuracy is driven by the number of clusters, not
the number of rows: a warning is raised below 20 clusters, and very
unequal cluster sizes lower the coverage of the interval (Davison &
Hinkley, 1997, section 3.8, pp. 100-102; Field & Welsh, 2007, section
3.3, p. 383, whose cluster bootstrap this is). Giving each row its own
cluster, numbered in row order, reproduces the plain bootstrap's
replicates (the result is still labelled `"cluster-bootstrap"` and the
fewer-than-20-clusters warning applies).

In
[`correlation()`](https://easystats.github.io/correlation/reference/correlation.md)
with `p_adjust = "holm"` (the default) or `"bonferroni"`, adjusted
p-values cannot fall below `2 * m / (iterations + 1)` for `m` pairs, so
with many pairs raise `iterations` (to at least `2 * m / alpha` for a
threshold `alpha`) or use `p_adjust = "none"`; the other adjustment
methods have a lower floor, but never below `2 / (iterations + 1)`.
Hoeffding's D and Shepherd's Pi are slow under bootstrap: each replicate
recomputes a statistic quadratic in the number of rows, or an inner
1000-sample outlier bootstrap.

## References

- Davison, A. C., & Hinkley, D. V. (1997). Bootstrap methods and their
  application. Cambridge University Press.

- Efron, B., & Tibshirani, R. J. (1993). An introduction to the
  bootstrap. Chapman & Hall.

- Field, C. A., & Welsh, A. H. (2007). Bootstrapping clustered data.
  Journal of the Royal Statistical Society: Series B, 69(3), 369-390.

- Boudt, K., Cornelissen, J., & Croux, C. (2012). The Gaussian rank
  correlation estimator: robustness properties. Statistics and
  Computing, 22(2), 471-483.

- Bhushan, N., Mohnert, F., Sloot, D., Jans, L., Albers, C., & Steg, L.
  (2019). Using a Gaussian graphical model to explore relationships
  between items and variables in environmental psychology research.
  Frontiers in psychology, 10, 1050.

- Bishara, A. J., & Hittner, J. B. (2017). Confidence intervals for
  correlations when data are not normal. Behavior research methods,
  49(1), 294-309.

- Fieller, E. C., Hartley, H. O., & Pearson, E. S. (1957). Tests for
  rank correlation coefficients. I. Biometrika, 44(3/4), 470-481.

- Langfelder, P., & Horvath, S. (2012). Fast R functions for robust
  correlations and hierarchical clustering. Journal of statistical
  software, 46(11).

- Blomqvist, N. (1950). On a measure of dependence between two random
  variables,Annals of Mathematical Statistics,21, 593–600

- Somers, R. H. (1962). A new asymmetric measure of association for
  ordinal variables. American Sociological Review. 27 (6).

## Examples

``` r
library(correlation)

cor_test(iris, "Sepal.Length", "Sepal.Width")
#> Parameter1   |  Parameter2 |     r |        95% CI | t(148) |     p
#> -------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.12 | [-0.27, 0.04] |  -1.44 | 0.152
#> 
#> Observations: 150
cor_test(iris, "Sepal.Length", "Sepal.Width", method = "spearman")
#> Parameter1   |  Parameter2 |   rho |         95% CI |        S |      p
#> -----------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.17 | [-0.32,  0.00] | 6.56e+05 | 0.041*
#> 
#> Observations: 150
# \donttest{
# Bootstrap confidence interval, standard error, and p-value
cor_test(iris, "Sepal.Length", "Sepal.Width", bootstrap = TRUE, iterations = 200)
#> Parameter1   |  Parameter2 |     r |   SE |        95% CI | t | df |     p
#> --------------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.12 | 0.08 | [-0.27, 0.02] |   |    | 0.109
#> 
#> Confidence intervals: percentile bootstrap (200 iterations)
#> Observations: 150

# Cluster bootstrap: rows of the same cluster are resampled together
data <- iris
data$id <- rep(1:30, 5)
cor_test(data, "Sepal.Length", "Sepal.Width", cluster = "id", iterations = 200)
#> Parameter1   |  Parameter2 |     r |   SE |        95% CI | t | df |     p
#> --------------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.12 | 0.08 | [-0.29, 0.03] |   |    | 0.109
#> 
#> Confidence intervals: percentile cluster bootstrap (200 iterations)
#> Observations: 150

cor_test(iris, "Sepal.Length", "Sepal.Width", method = "kendall")
#> Parameter1   |  Parameter2 |   tau |        95% CI |     z |     p
#> ------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.08 | [-0.18, 0.03] | -1.33 | 0.183
#> 
#> Observations: 150
cor_test(iris, "Sepal.Length", "Sepal.Width", method = "biweight")
#> Parameter1   |  Parameter2 |     r |        95% CI | t(148) |     p
#> -------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.13 | [-0.29, 0.03] |  -1.65 | 0.100
#> 
#> Observations: 150
cor_test(iris, "Sepal.Length", "Sepal.Width", method = "distance")
#> Parameter1   |  Parameter2 |    r |        95% CI | t(11024) |         p
#> ------------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | 0.08 | [-0.08, 0.24] |     8.43 | < .001***
#> 
#> Observations: 150
cor_test(iris, "Sepal.Length", "Sepal.Width", method = "percentage")
#> Parameter1   |  Parameter2 |     r |         95% CI | t(148) |      p
#> ---------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.19 | [-0.34, -0.03] |  -2.40 | 0.018*
#> 
#> Observations: 150

if (require("wdm", quietly = TRUE)) {
  cor_test(iris, "Sepal.Length", "Sepal.Width", method = "blomqvist")
}
#> Parameter1   |  Parameter2 |     r |         95% CI | t(148) |       p
#> ----------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.23 | [-0.37, -0.07] |  -2.83 | 0.005**
#> 
#> Observations: 150

if (require("Hmisc", quietly = TRUE)) {
  cor_test(iris, "Sepal.Length", "Sepal.Width", method = "hoeffding")
}
#> 
#> Attaching package: ‘Hmisc’
#> The following objects are masked from ‘package:base’:
#> 
#>     format.pval, units
#> Parameter1   |  Parameter2 |    r | 95% CI | t(148) |      p
#> ------------------------------------------------------------
#> Sepal.Length | Sepal.Width | 0.01 |        |        | 0.011*
#> 
#> Observations: 150
cor_test(iris, "Sepal.Length", "Sepal.Width", method = "gamma")
#> Parameter1   |  Parameter2 |     r |        95% CI | t(148) |     p
#> -------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.08 | [-0.24, 0.08] |  -0.99 | 0.323
#> 
#> Observations: 150
cor_test(iris, "Sepal.Length", "Sepal.Width", method = "gaussian")
#> Parameter1   |  Parameter2 |     r |        95% CI | t(148) |     p
#> -------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.10 | [-0.26, 0.06] |  -1.21 | 0.229
#> 
#> Observations: 150
cor_test(iris, "Sepal.Length", "Sepal.Width", method = "shepherd")
#> Parameter1   |  Parameter2 |   rho |         95% CI |        S |       p
#> ------------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.23 | [-0.39, -0.07] | 6.02e+05 | 0.005**
#> 
#> Observations: 150
if (require("BayesFactor", quietly = TRUE)) {
  cor_test(iris, "Sepal.Length", "Sepal.Width", bayesian = TRUE)
}
#> ************
#> Welcome to BayesFactor 0.9.12-4.8. If you have questions, please contact Richard Morey (richarddmorey@gmail.com).
#> 
#> Type BFManual() to open the manual.
#> ************
#> Parameter1   |  Parameter2 |   rho |        95% CI |     pd | % in ROPE
#> -----------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.11 | [-0.25, 0.05] | 93.17% |    42.85%
#> 
#> Parameter1   |         Prior |    BF
#> ------------------------------------
#> Sepal.Length | Beta (3 +- 3) | 0.509
#> 
#> Observations: 150

# Robust (these two are equivalent)
cor_test(iris, "Sepal.Length", "Sepal.Width", method = "spearman")
#> Parameter1   |  Parameter2 |   rho |         95% CI |        S |      p
#> -----------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.17 | [-0.32,  0.00] | 6.56e+05 | 0.041*
#> 
#> Observations: 150
cor_test(iris, "Sepal.Length", "Sepal.Width", method = "pearson", ranktransform = TRUE)
#> Parameter1   |  Parameter2 |     r |         95% CI | t(148) |      p
#> ---------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.17 | [-0.32, -0.01] |  -2.06 | 0.041*
#> 
#> Observations: 150

# Winsorized
cor_test(iris, "Sepal.Length", "Sepal.Width", winsorize = 0.2)
#> Parameter1   |  Parameter2 |     r |         95% CI | t(148) |       p
#> ----------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | -0.25 | [-0.39, -0.09] |  -3.14 | 0.002**
#> 
#> Observations: 150

# Tetrachoric
if (require("psych", quietly = TRUE) && require("rstanarm", quietly = TRUE)) {
  data <- iris
  data$Sepal.Width_binary <- ifelse(data$Sepal.Width > 3, 1, 0)
  data$Petal.Width_binary <- ifelse(data$Petal.Width > 1.2, 1, 0)
  cor_test(data, "Sepal.Width_binary", "Petal.Width_binary", method = "tetrachoric")

  # Biserial
  cor_test(data, "Sepal.Width", "Petal.Width_binary", method = "biserial")

  # Polychoric
  data$Petal.Width_ordinal <- as.factor(round(data$Petal.Width))
  data$Sepal.Length_ordinal <- as.factor(round(data$Sepal.Length))
  cor_test(data, "Petal.Width_ordinal", "Sepal.Length_ordinal", method = "polychoric")

  # When one variable is continuous, will run 'polyserial' correlation
  cor_test(data, "Sepal.Width", "Sepal.Length_ordinal", method = "polychoric")
}
#> 
#> Attaching package: ‘psych’
#> The following object is masked from ‘package:Hmisc’:
#> 
#>     describe
#> The following objects are masked from ‘package:ggplot2’:
#> 
#>     %+%, alpha
#> This is rstanarm version 2.32.2
#> - See https://mc-stan.org/rstanarm/articles/priors for changes to default priors!
#> - Default priors may change, so it's safest to specify priors, even if equivalent to the defaults.
#> - For execution on a local, multicore CPU with excess RAM we recommend calling
#>   options(mc.cores = parallel::detectCores())
#> 
#> Attaching package: ‘rstanarm’
#> The following object is masked from ‘package:psych’:
#> 
#>     logit
#> Parameter1  |           Parameter2 |   rho |        95% CI | t(148) |     p
#> ---------------------------------------------------------------------------
#> Sepal.Width | Sepal.Length_ordinal | -0.14 | [-0.30, 0.02] |  -1.77 | 0.079
#> 
#> Observations: 150

# Partial
cor_test(iris, "Sepal.Length", "Sepal.Width", partial = TRUE)
#> Parameter1   |  Parameter2 |    r |       95% CI | t(148) |         p
#> ---------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | 0.43 | [0.29, 0.55] |   5.84 | < .001***
#> 
#> Observations: 150
if (require("lme4", quietly = TRUE)) {
  cor_test(iris, "Sepal.Length", "Sepal.Width", multilevel = TRUE)
}
#> Parameter1   |  Parameter2 |    r |       95% CI | t(148) |         p
#> ---------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | 0.43 | [0.29, 0.55] |   5.85 | < .001***
#> 
#> Observations: 150
if (require("rstanarm", quietly = TRUE)) {
  cor_test(iris, "Sepal.Length", "Sepal.Width", partial_bayesian = TRUE)
}
#> Parameter1   |  Parameter2 |    r |       95% CI | t(148) |         p
#> ---------------------------------------------------------------------
#> Sepal.Length | Sepal.Width | 0.43 | [0.29, 0.55] |   5.84 | < .001***
#> 
#> Observations: 150
# }
```
