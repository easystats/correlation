# correlation output with zap_small

    Code
      summary(r)
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter |    z |    y
      -----------------------
      x         | 0.02 | 0.01
      y         | 0.00 |     
      
      p-value adjustment method: Holm (1979)

---

    Code
      summary(r, zap_small = FALSE)
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter |         z |        y
      --------------------------------
      x         |      0.02 | 6.02e-03
      y         | -3.07e-03 |         
      
      p-value adjustment method: Holm (1979)

