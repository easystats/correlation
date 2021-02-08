# display and print method works - markdown

    Code
      print(summary(correlation(iris)))
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter    | Petal.Width | Petal.Length | Sepal.Width
      -------------------------------------------------------
      Sepal.Length |     0.82*** |      0.87*** |       -0.12
      Sepal.Width  |    -0.37*** |     -0.43*** |            
      Petal.Length |     0.96*** |              |            
      
      p-value adjustment method: Holm (1979)

# display and print method works - html

    Code
      print(summary(correlation(iris)), format = "html")
    Output
      # Correlation Matrix (pearson-method)
      
      Parameter    | Petal.Width | Petal.Length | Sepal.Width
      -------------------------------------------------------
      Sepal.Length |     0.82*** |      0.87*** |       -0.12
      Sepal.Width  |    -0.37*** |     -0.43*** |            
      Petal.Length |     0.96*** |              |            
      
      p-value adjustment method: Holm (1979)

