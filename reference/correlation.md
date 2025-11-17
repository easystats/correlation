# Correlation Analysis

Performs a correlation analysis. You can easily visualize the result
using
[`plot()`](https://easystats.github.io/correlation/reference/visualisation_recipe.easycormatrix.md)
(see examples
[**here**](https://easystats.github.io/correlation/reference/visualisation_recipe.easycormatrix.html#ref-examples)).

## Usage

``` r
correlation(
  data,
  data2 = NULL,
  select = NULL,
  select2 = NULL,
  rename = NULL,
  method = "pearson",
  missing = "keep_pairwise",
  p_adjust = "holm",
  ci = 0.95,
  bayesian = FALSE,
  bayesian_prior = "medium",
  bayesian_ci_method = "hdi",
  bayesian_test = c("pd", "rope", "bf"),
  redundant = FALSE,
  include_factors = FALSE,
  partial = FALSE,
  partial_bayesian = FALSE,
  multilevel = FALSE,
  ranktransform = FALSE,
  winsorize = FALSE,
  verbose = TRUE,
  standardize_names = getOption("easystats.standardize_names", FALSE),
  ...
)
```

## Arguments

- data:

  A data frame.

- data2:

  An optional data frame. If specified, all pair-wise correlations
  between the variables in `data` and `data2` will be computed.

- select, select2:

  (Ignored if `data2` is specified.) Optional names of variables that
  should be selected for correlation. Instead of providing the data
  frames with those variables that should be correlated, `data` can be a
  data frame and `select` and `select2` are (quoted) names of variables
  (columns) in `data`. `correlation()` will then compute the correlation
  between `data[select]` and `data[select2]`. If only `select` is
  specified, all pairwise correlations between the `select` variables
  will be computed. This is a "pipe-friendly" alternative way of using
  `correlation()` (see 'Examples').

- rename:

  In case you wish to change the names of the variables in the output,
  these arguments can be used to specify these alternative names. Note
  that the number of names should be equal to the number of columns
  selected. Ignored if `data2` is specified.

- method:

  A character string indicating which correlation coefficient is to be
  used for the test. One of `"pearson"` (default), `"kendall"`,
  `"spearman"` (but see also the `robust` argument), `"biserial"`,
  `"polychoric"`, `"tetrachoric"`, `"biweight"`, `"distance"`,
  `"percentage"` (for percentage bend correlation), `"blomqvist"` (for
  Blomqvist's coefficient), `"hoeffding"` (for Hoeffding's D),
  `"gamma"`, `"gaussian"` (for Gaussian Rank correlation) or
  `"shepherd"` (for Shepherd's Pi correlation). Setting `"auto"` will
  attempt at selecting the most relevant method (polychoric when ordinal
  factors involved, tetrachoric when dichotomous factors involved,
  point-biserial if one dichotomous and one continuous and pearson
  otherwise). See below the **details** section for a description of
  these indices.

- missing:

  How should missing values be treated? If `"keep_pairwise"` (default)
  then the correlation between each pair of variables is computed using
  all complete pairs of observations on those variables. If
  `"keep_complete"` then missing values are handled by case-wise
  deletion, and correlations are computed using only observations with
  full data (based on `data2`/`select`/`select2` when applicable).

- p_adjust:

  Correction method for frequentist correlations. Can be one of `"holm"`
  (default), `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`,
  `"fdr"`, `"somers"` or `"none"`. See
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  further details.

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

- redundant:

  Should the data include redundant rows (where each given correlation
  is repeated two times).

- include_factors:

  If `TRUE`, the factors are kept and eventually converted to numeric or
  used as random effects (depending of `multilevel`). If `FALSE`,
  factors are removed upfront.

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
  rank correlation. Thus, using `robust=TRUE` and `method="spearman"` is
  redundant. Nonetheless, it is an easy option to increase the
  robustness of the correlation as well as flexible way to obtain
  Bayesian or multilevel Spearman-like rank correlations.

- winsorize:

  Another way of making the correlation more "robust" (i.e., limiting
  the impact of extreme values). Can be either `FALSE` or a number
  between 0 and 1 (e.g., `0.2`) that corresponds to the desired
  threshold. See the
  [`datawizard::winsorize()`](https://easystats.github.io/datawizard/reference/winsorize.html)
  function for more details.

- verbose:

  Toggle warnings.

- standardize_names:

  This option can be set to `TRUE` to run
  [`insight::standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.html)
  on the output to get standardized column names. This option can also
  be set globally by running
  `options(easystats.standardize_names = TRUE)`.

- ...:

  Additional arguments (e.g., `alternative`) to be passed to other
  methods. See
  [`stats::cor.test`](https://rdrr.io/r/stats/cor.test.html) for further
  details.

## Value

A correlation object that can be displayed using the `print`, `summary`
or `table` methods.

### Multiple tests correction

The `p_adjust` argument can be used to adjust p-values for multiple
comparisons. All adjustment methods available in `p.adjust` function
`stats` package are supported.

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
  estimates (see Shmid & Schimdt, 2006).

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
using
[`cor_test()`](https://easystats.github.io/correlation/reference/cor_test.md)
and `correlation()`: If you set `multilevel=TRUE` in `correlation()` but
`partial` is set to `FALSE` (as per default), then a back-transformation
from partial to non-partial correlation will be attempted (through
[`pcor_to_cor()`](https://easystats.github.io/correlation/reference/cor_to_pcor.md)).
However, this is not possible when using
[`cor_test()`](https://easystats.github.io/correlation/reference/cor_test.md)
so that if you set `multilevel=TRUE` in it, the resulting correlations
are partial one. Note that for Bayesian multilevel correlations, if
`partial = FALSE`, the back transformation will also recompute
*p*-values based on the new *r* scores, and will drop the Bayes factors
(as they are not relevant anymore). To keep Bayesian scores, set
`partial = TRUE`.

### Notes

Kendall and Spearman correlations when `bayesian=TRUE`: These are
technically Pearson Bayesian correlations of rank transformed data,
rather than pure Bayesian rank correlations (which have different
priors).

## References

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
data(iris)

results <- correlation(iris)

results
#> # Correlation Matrix (pearson-method)
#> 
#> Parameter1   |   Parameter2 |     r |         95% CI | t(148) |         p
#> -------------------------------------------------------------------------
#> Sepal.Length |  Sepal.Width | -0.12 | [-0.27,  0.04] |  -1.44 | 0.152    
#> Sepal.Length | Petal.Length |  0.87 | [ 0.83,  0.91] |  21.65 | < .001***
#> Sepal.Length |  Petal.Width |  0.82 | [ 0.76,  0.86] |  17.30 | < .001***
#> Sepal.Width  | Petal.Length | -0.43 | [-0.55, -0.29] |  -5.77 | < .001***
#> Sepal.Width  |  Petal.Width | -0.37 | [-0.50, -0.22] |  -4.79 | < .001***
#> Petal.Length |  Petal.Width |  0.96 | [ 0.95,  0.97] |  43.39 | < .001***
#> 
#> p-value adjustment method: Holm (1979)
#> Observations: 150
summary(results)
#> # Correlation Matrix (pearson-method)
#> 
#> Parameter    | Petal.Width | Petal.Length | Sepal.Width
#> -------------------------------------------------------
#> Sepal.Length |     0.82*** |      0.87*** |       -0.12
#> Sepal.Width  |    -0.37*** |     -0.43*** |            
#> Petal.Length |     0.96*** |              |            
#> 
#> p-value adjustment method: Holm (1979)
summary(results, redundant = TRUE)
#> # Correlation Matrix (pearson-method)
#> 
#> Parameter    | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
#> ----------------------------------------------------------------------
#> Sepal.Length |              |       -0.12 |      0.87*** |     0.82***
#> Sepal.Width  |        -0.12 |             |     -0.43*** |    -0.37***
#> Petal.Length |      0.87*** |    -0.43*** |              |     0.96***
#> Petal.Width  |      0.82*** |    -0.37*** |      0.96*** |            
#> 
#> p-value adjustment method: Holm (1979)

# pipe-friendly usage with  grouped dataframes from {dplyr} package
iris |>
  correlation(select = "Petal.Width", select2 = "Sepal.Length")
#> # Correlation Matrix (pearson-method)
#> 
#> Parameter1  |   Parameter2 |    r |       95% CI | t(148) |         p
#> ---------------------------------------------------------------------
#> Petal.Width | Sepal.Length | 0.82 | [0.76, 0.86] |  17.30 | < .001***
#> 
#> p-value adjustment method: Holm (1979)
#> Observations: 150

# Grouped dataframe
# grouped correlations
iris |>
  datawizard::data_group(Species) |>
  correlation()
#> # Correlation Matrix (pearson-method)
#> 
#> Group      |   Parameter1 |   Parameter2 |    r |        95% CI | t(48) |         p
#> -----------------------------------------------------------------------------------
#> setosa     | Sepal.Length |  Sepal.Width | 0.74 | [ 0.59, 0.85] |  7.68 | < .001***
#> setosa     | Sepal.Length | Petal.Length | 0.27 | [-0.01, 0.51] |  1.92 | 0.202    
#> setosa     | Sepal.Length |  Petal.Width | 0.28 | [ 0.00, 0.52] |  2.01 | 0.202    
#> setosa     |  Sepal.Width | Petal.Length | 0.18 | [-0.11, 0.43] |  1.25 | 0.217    
#> setosa     |  Sepal.Width |  Petal.Width | 0.23 | [-0.05, 0.48] |  1.66 | 0.208    
#> setosa     | Petal.Length |  Petal.Width | 0.33 | [ 0.06, 0.56] |  2.44 | 0.093    
#> versicolor | Sepal.Length |  Sepal.Width | 0.53 | [ 0.29, 0.70] |  4.28 | < .001***
#> versicolor | Sepal.Length | Petal.Length | 0.75 | [ 0.60, 0.85] |  7.95 | < .001***
#> versicolor | Sepal.Length |  Petal.Width | 0.55 | [ 0.32, 0.72] |  4.52 | < .001***
#> versicolor |  Sepal.Width | Petal.Length | 0.56 | [ 0.33, 0.73] |  4.69 | < .001***
#> versicolor |  Sepal.Width |  Petal.Width | 0.66 | [ 0.47, 0.80] |  6.15 | < .001***
#> versicolor | Petal.Length |  Petal.Width | 0.79 | [ 0.65, 0.87] |  8.83 | < .001***
#> virginica  | Sepal.Length |  Sepal.Width | 0.46 | [ 0.20, 0.65] |  3.56 | 0.003**  
#> virginica  | Sepal.Length | Petal.Length | 0.86 | [ 0.77, 0.92] | 11.90 | < .001***
#> virginica  | Sepal.Length |  Petal.Width | 0.28 | [ 0.00, 0.52] |  2.03 | 0.048*   
#> virginica  |  Sepal.Width | Petal.Length | 0.40 | [ 0.14, 0.61] |  3.03 | 0.012*   
#> virginica  |  Sepal.Width |  Petal.Width | 0.54 | [ 0.31, 0.71] |  4.42 | < .001***
#> virginica  | Petal.Length |  Petal.Width | 0.32 | [ 0.05, 0.55] |  2.36 | 0.045*   
#> 
#> p-value adjustment method: Holm (1979)
#> Observations: 50

# selecting specific variables for correlation
data(mtcars)
mtcars |>
  datawizard::data_group(am) |>
  correlation(select = c("cyl", "wt"), select2 = "hp")
#> # Correlation Matrix (pearson-method)
#> 
#> Group | Parameter1 | Parameter2 |    r |       95% CI |    t | df |         p
#> -----------------------------------------------------------------------------
#> 0     |        cyl |         hp | 0.85 | [0.64, 0.94] | 6.53 | 17 | < .001***
#> 0     |         wt |         hp | 0.68 | [0.33, 0.87] | 3.82 | 17 | 0.001**  
#> 1     |        cyl |         hp | 0.90 | [0.69, 0.97] | 6.87 | 11 | < .001***
#> 1     |         wt |         hp | 0.81 | [0.48, 0.94] | 4.66 | 11 | < .001***
#> 
#> p-value adjustment method: Holm (1979)
#> Observations: 13-19

# supplying custom variable names
correlation(anscombe, select = c("x1", "x2"), rename = c("var1", "var2"))
#> # Correlation Matrix (pearson-method)
#> 
#> Parameter1 | Parameter2 | r |       95% CI | t(9) |         p
#> -------------------------------------------------------------
#> var1       |       var2 | 1 | [1.00, 1.00] |  Inf | < .001***
#> 
#> p-value adjustment method: Holm (1979)
#> Observations: 11

# automatic selection of correlation method
correlation(mtcars[-2], method = "auto")
#> # Correlation Matrix (auto-method)
#> 
#> Parameter1 | Parameter2 |     r |         95% CI | t(30) |         p
#> --------------------------------------------------------------------
#> mpg        |       disp | -0.85 | [-0.92, -0.71] | -8.75 | < .001***
#> mpg        |         hp | -0.78 | [-0.89, -0.59] | -6.74 | < .001***
#> mpg        |       drat |  0.68 | [ 0.44,  0.83] |  5.10 | < .001***
#> mpg        |         wt | -0.87 | [-0.93, -0.74] | -9.56 | < .001***
#> mpg        |       qsec |  0.42 | [ 0.08,  0.67] |  2.53 | 0.222    
#> mpg        |         vs |  0.66 | [ 0.41,  0.82] |  4.86 | < .001***
#> mpg        |         am |  0.60 | [ 0.32,  0.78] |  4.11 | 0.007**  
#> mpg        |       gear |  0.48 | [ 0.16,  0.71] |  3.00 | 0.097    
#> mpg        |       carb | -0.55 | [-0.75, -0.25] | -3.62 | 0.021*   
#> disp       |         hp |  0.79 | [ 0.61,  0.89] |  7.08 | < .001***
#> disp       |       drat | -0.71 | [-0.85, -0.48] | -5.53 | < .001***
#> disp       |         wt |  0.89 | [ 0.78,  0.94] | 10.58 | < .001***
#> disp       |       qsec | -0.43 | [-0.68, -0.10] | -2.64 | 0.197    
#> disp       |         vs | -0.71 | [-0.85, -0.48] | -5.53 | < .001***
#> disp       |         am | -0.59 | [-0.78, -0.31] | -4.02 | 0.009**  
#> disp       |       gear | -0.56 | [-0.76, -0.26] | -3.66 | 0.020*   
#> disp       |       carb |  0.39 | [ 0.05,  0.65] |  2.35 | 0.303    
#> hp         |       drat | -0.45 | [-0.69, -0.12] | -2.75 | 0.170    
#> hp         |         wt |  0.66 | [ 0.40,  0.82] |  4.80 | 0.001**  
#> hp         |       qsec | -0.71 | [-0.85, -0.48] | -5.49 | < .001***
#> hp         |         vs | -0.72 | [-0.86, -0.50] | -5.73 | < .001***
#> hp         |         am | -0.24 | [-0.55,  0.12] | -1.37 | > .999   
#> hp         |       gear | -0.13 | [-0.45,  0.23] | -0.69 | > .999   
#> hp         |       carb |  0.75 | [ 0.54,  0.87] |  6.21 | < .001***
#> drat       |         wt | -0.71 | [-0.85, -0.48] | -5.56 | < .001***
#> drat       |       qsec |  0.09 | [-0.27,  0.43] |  0.50 | > .999   
#> drat       |         vs |  0.44 | [ 0.11,  0.68] |  2.69 | 0.187    
#> drat       |         am |  0.71 | [ 0.48,  0.85] |  5.57 | < .001***
#> drat       |       gear |  0.70 | [ 0.46,  0.84] |  5.36 | < .001***
#> drat       |       carb | -0.09 | [-0.43,  0.27] | -0.50 | > .999   
#> wt         |       qsec | -0.17 | [-0.49,  0.19] | -0.97 | > .999   
#> wt         |         vs | -0.55 | [-0.76, -0.26] | -3.65 | 0.020*   
#> wt         |         am | -0.69 | [-0.84, -0.45] | -5.26 | < .001***
#> wt         |       gear | -0.58 | [-0.77, -0.29] | -3.93 | 0.011*   
#> wt         |       carb |  0.43 | [ 0.09,  0.68] |  2.59 | 0.205    
#> qsec       |         vs |  0.74 | [ 0.53,  0.87] |  6.11 | < .001***
#> qsec       |         am | -0.23 | [-0.54,  0.13] | -1.29 | > .999   
#> qsec       |       gear | -0.21 | [-0.52,  0.15] | -1.19 | > .999   
#> qsec       |       carb | -0.66 | [-0.82, -0.40] | -4.76 | 0.001**  
#> vs         |         am |  0.26 | [-0.09,  0.56] |  1.50 | > .999   
#> vs         |       gear |  0.21 | [-0.15,  0.52] |  1.15 | > .999   
#> vs         |       carb | -0.57 | [-0.77, -0.28] | -3.80 | 0.015*   
#> am         |       gear |  0.79 | [ 0.62,  0.89] |  7.16 | < .001***
#> am         |       carb |  0.06 | [-0.30,  0.40] |  0.32 | > .999   
#> gear       |       carb |  0.27 | [-0.08,  0.57] |  1.56 | > .999   
#> 
#> p-value adjustment method: Holm (1979)
#> Observations: 32
```
