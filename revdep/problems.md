# effectsize

<details>

* Version: 0.7.0
* GitHub: https://github.com/easystats/effectsize
* Source code: https://github.com/cran/effectsize
* Date/Publication: 2022-05-26 13:20:02 UTC
* Number of recursive dependencies: 234

Run `revdep_details(, "effectsize")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       7.       └─effectsize (local) FUN(newX[, i], ...)
       8.         └─datawizard::ranktransform(x, method = "average", ..., verbose = FALSE)
      ── Error (test-rankES.R:76:5): kendalls_w ──────────────────────────────────────
      Error in `UseMethod("ranktransform")`: no applicable method for 'ranktransform' applied to an object of class "character"
      Backtrace:
    ...
          ▆
       1. └─effectsize::kendalls_w(M1) at test-rankES.R:76:4
       2.   └─effectsize:::.kendalls_w(data, verbose = verbose)
       3.     └─base::apply(data, 1, .safe_ranktransform, verbose = verbose)
       4.       └─effectsize (local) FUN(newX[, i], ...)
       5.         └─datawizard::ranktransform(x, method = "average", ..., verbose = FALSE)
      
      [ FAIL 3 | WARN 3 | SKIP 11 | PASS 494 ]
      Error: Test failures
      Execution halted
    ```

# see

<details>

* Version: 0.7.1
* GitHub: https://github.com/easystats/see
* Source code: https://github.com/cran/see
* Date/Publication: 2022-06-20 14:20:02 UTC
* Number of recursive dependencies: 210

Run `revdep_details(, "see")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘see-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: coord_radar
    > ### Title: Radar coordinate system
    > ### Aliases: coord_radar
    > 
    > ### ** Examples
    > 
    > # Create a radar/spider chart with ggplot:
    ...
    Attaching package: ‘poorman’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    Loading required package: ggplot2
    Error in FUN(X[[i]], ...) : object 'Name' not found
    Calls: <Anonymous> ... <Anonymous> -> f -> scales_add_defaults -> lapply -> FUN
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘datawizard::data_rescale’
    ```

# statsExpressions

<details>

* Version: 1.3.2
* GitHub: https://github.com/IndrajeetPatil/statsExpressions
* Source code: https://github.com/cran/statsExpressions
* Date/Publication: 2022-05-20 19:50:02 UTC
* Number of recursive dependencies: 158

Run `revdep_details(, "statsExpressions")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   └─effectsize:::.kendalls_w(data, verbose = verbose)
        6. │     └─base::apply(data, 1, .safe_ranktransform, verbose = verbose)
        7. │       └─effectsize (local) FUN(newX[, i], ...)
        8. │         └─datawizard::ranktransform(x, method = "average", ..., verbose = FALSE)
        9. ├─statsExpressions:::tidy_model_effectsize(.)
    ...
       10. │ ├─dplyr::bind_cols(...)
       11. │ │ └─rlang::list2(...)
       12. │ └─... %>% select(-contains("term"))
       13. ├─dplyr::select(., -contains("term"))
       14. ├─insight::standardize_names(., style = "broom")
       15. └─dplyr::mutate(., effectsize = stats::na.omit(effectsize::get_effectsize_label(colnames(.))))
      
      [ FAIL 2 | WARN 2 | SKIP 52 | PASS 33 ]
      Error: Test failures
      Execution halted
    ```

