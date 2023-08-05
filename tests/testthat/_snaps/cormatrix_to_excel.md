# cormatrix_to_excel select

    Code
      cormatrix_to_excel(mtcars, filename = "cormatrix1", overwrite = TRUE, p_adjust = "none",
        print.mat = TRUE, select = c("mpg", "cyl", "disp", "hp", "carb"))
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter |      mpg |      cyl |     disp |       hp |    carb
      ---------------------------------------------------------------
      mpg       |          | -0.85*** | -0.85*** | -0.78*** | -0.55**
      cyl       | -0.85*** |          |  0.90*** |  0.83*** |  0.53**
      disp      | -0.85*** |  0.90*** |          |  0.79*** |   0.39*
      hp        | -0.78*** |  0.83*** |  0.79*** |          | 0.75***
      carb      |  -0.55** |   0.53** |    0.39* |  0.75*** |        
      
      p-value adjustment method: none
      
      
       [Correlation matrix 'cormatrix1.xlsx' has been saved to working directory (or where specified).]
    Warning <simpleWarning>
      will not open file when not interactive
    Output
      NULL

# cormatrix_to_excel p_adjust

    Code
      cormatrix_to_excel(airquality, filename = "cormatrix1", overwrite = FALSE,
        p_adjust = "holm", print.mat = FALSE, method = "spearman")
    Output
      
      
       [Correlation matrix 'cormatrix1.xlsx' has been saved to working directory (or where specified).]
    Warning <simpleWarning>
      will not open file when not interactive
    Output
      NULL

