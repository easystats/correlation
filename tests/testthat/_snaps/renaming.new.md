# renaming columns

    Code
      correlation(anscombe, select = c("x1", "x2"), rename = c("var1"))
    Condition
      Warning:
      Mismatch between number of variables and names.
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter1 | Parameter2 |    r |       95% CI | t(9) |         p
      ----------------------------------------------------------------
      x1         |         x2 | 1.00 | [1.00, 1.00] |  Inf | < .001***
      
      p-value adjustment method: Holm (1979)

---

    Code
      correlation(anscombe, select = c("x1", "x2"), rename = c("var1", "var2"))
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter1 | Parameter2 |    r |       95% CI | t(9) |         p
      ----------------------------------------------------------------
      var1       |       var2 | 1.00 | [1.00, 1.00] |  Inf | < .001***
      
      p-value adjustment method: Holm (1979)

---

    Code
      correlation(anscombe, select = c("x1", "x2"), select2 = c("y1", "y2"), rename = c(
        "var1", "var2"))
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter1 | Parameter2 |    r |       95% CI | t(9) |       p
      --------------------------------------------------------------
      var1       |         y1 | 0.82 | [0.42, 0.95] | 4.24 | 0.009**
      var1       |         y2 | 0.82 | [0.42, 0.95] | 4.24 | 0.009**
      var2       |         y1 | 0.82 | [0.42, 0.95] | 4.24 | 0.009**
      var2       |         y2 | 0.82 | [0.42, 0.95] | 4.24 | 0.009**
      
      p-value adjustment method: Holm (1979)

