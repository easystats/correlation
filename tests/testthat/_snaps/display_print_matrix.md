# display and print method works - markdown

    Code
      display(summary(correlation(iris)))
    Output
      
      
      Table: Correlation Matrix (pearson-method)
      
      |Parameter    | Petal.Width | Petal.Length | Sepal.Width |
      |:------------|:-----------:|:------------:|:-----------:|
      |Sepal.Length |     0.82*** |      0.87*** |       -0.12 |
      |Sepal.Width  |    -0.37*** |     -0.43*** |             |
      |Petal.Length |     0.96*** |              |             |
      p-value adjustment method: Holm (1979)

# as.matrix works

    Code
      list(mat1, mat2)
    Output
      [[1]]
                 am         wt         hp
      am  1.0000000 -0.6924953 -0.2432043
      wt -0.6924953  1.0000000  0.6587479
      hp -0.2432043  0.6587479  1.0000000
      
      [[2]]
                    wt        hp
      0 - wt 1.0000000 0.6797596
      0 - hp 0.6797596 1.0000000
      1 - wt 1.0000000 0.8145279
      1 - hp 0.8145279 1.0000000
      

