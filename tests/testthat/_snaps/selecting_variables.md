# selecting specific variables works

    Code
      list(df1, df2, df3, df4)
    Output
      [[1]]
      # Correlation Matrix (pearson-method)
      
      Parameter1 | Parameter2 |    r |       95% CI | t(30) |         p
      -----------------------------------------------------------------
      cyl        |         hp | 0.83 | [0.68, 0.92] |  8.23 | < .001***
      wt         |         hp | 0.66 | [0.40, 0.82] |  4.80 | < .001***
      
      p-value adjustment method: Holm (1979)
      Observations: 32
      
      [[2]]
      # Correlation Matrix (pearson-method)
      
      Group | Parameter1 | Parameter2 |    r |       95% CI |    t | df |         p
      -----------------------------------------------------------------------------
      0     |        cyl |         hp | 0.85 | [0.64, 0.94] | 6.53 | 17 | < .001***
      0     |         wt |         hp | 0.68 | [0.33, 0.87] | 3.82 | 17 | 0.001**  
      1     |        cyl |         hp | 0.90 | [0.69, 0.97] | 6.87 | 11 | < .001***
      1     |         wt |         hp | 0.81 | [0.48, 0.94] | 4.66 | 11 | < .001***
      
      p-value adjustment method: Holm (1979)
      Observations: 13-19
      
      [[3]]
      # Correlation Matrix (pearson-method)
      
      Parameter1 | Parameter2 |    r |       95% CI | t(30) |         p
      -----------------------------------------------------------------
      wt         |         hp | 0.66 | [0.40, 0.82] |  4.80 | < .001***
      
      p-value adjustment method: Holm (1979)
      Observations: 32
      
      [[4]]
      # Correlation Matrix (pearson-method)
      
      Parameter1 | Parameter2 |    r |       95% CI | t(30) |         p
      -----------------------------------------------------------------
      wt         |         hp | 0.66 | [0.40, 0.82] |  4.80 | < .001***
      
      p-value adjustment method: Holm (1979)
      Observations: 32
      

