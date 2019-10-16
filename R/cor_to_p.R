cor_to_p <- function(cor, n, method = "pearson"){

  # N-variable
  nvar <- ncol(cor)

  # Statistic
  if(method == "kendall"){
    statistic <- (3 * cor * sqrt(n * (n - 1))) / sqrt(2 * (2 * n + 5))
  } else if (method == "spearman") {
    statistic = cor * sqrt((n-3)/(1-cor^2))
  } else{
    statistic <- cor * sqrt((n - 2 - (nvar - 2)) / (1 - cor^2))
  }
  diag(statistic) <- 0


  # p-value
  if(method == "kendall"){
    p <- 2 * pnorm(-abs(statistic))
  } else{
    p <- 2*pt(-abs(statistic), df = n - nvar)
  }

  list(p = p, statistic = statistic)
}

