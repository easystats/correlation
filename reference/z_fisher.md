# Fisher z-transformation

The Fisher z-transformation converts the standard Pearson's *r* to a
normally distributed variable z'. It is used to compute confidence
intervals to correlations. The z' variable is different from the
*z*-statistic.

## Usage

``` r
z_fisher(r = NULL, z = NULL)
```

## Arguments

- r, z:

  The r or the z' value to be converted.

## Value

The transformed value.

## References

Zar, J.H., (2014). Spearman Rank Correlation: Overview. Wiley StatsRef:
Statistics Reference Online. doi:10.1002/9781118445112.stat05964

## Examples

``` r
z_fisher(r = 0.7)
#> [1] 0.8673005
z_fisher(z = 0.867)
#> [1] 0.6998467
```
