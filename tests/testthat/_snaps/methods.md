# summary.correlation - target column

    Code
      summary(correlation(ggplot2::msleep), target = "t")
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter   |   bodywt | brainwt |       awake | sleep_cycle | sleep_rem
      ------------------------------------------------------------------------
      sleep_total |   -2.96* |  -2.84* | -5328.71*** |      -2.95* |   8.76***
      sleep_rem   |   -2.66* |   -1.54 |    -8.76*** |       -1.97 |          
      sleep_cycle |     2.52 | 8.60*** |       2.95* |             |          
      awake       |    2.96* |   2.84* |             |             |          
      brainwt     | 19.18*** |         |             |             |          
      
      p-value adjustment method: Holm (1979)

---

    Code
      summary(correlation(ggplot2::msleep), target = "df_error")
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter   |   bodywt |  brainwt |    awake | sleep_cycle | sleep_rem
      ----------------------------------------------------------------------
      sleep_total |   81.00* |   54.00* | 81.00*** |      30.00* |  59.00***
      sleep_rem   |   59.00* |    46.00 | 59.00*** |       30.00 |          
      sleep_cycle |    30.00 | 28.00*** |   30.00* |             |          
      awake       |   81.00* |   54.00* |          |             |          
      brainwt     | 54.00*** |          |          |             |          
      
      p-value adjustment method: Holm (1979)

---

    Code
      summary(correlation(ggplot2::msleep), target = "p")
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter   | bodywt | brainwt | awake | sleep_cycle | sleep_rem
      ----------------------------------------------------------------
      sleep_total |   0.04 |    0.05 |  0.00 |        0.05 |      0.00
      sleep_rem   |   0.05 |    0.13 |  0.00 |        0.12 |          
      sleep_cycle |   0.05 |    0.00 |  0.05 |             |          
      awake       |   0.04 |    0.05 |       |             |          
      brainwt     |   0.00 |         |       |             |          
      
      p-value adjustment method: Holm (1979)

