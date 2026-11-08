exp_series <- function(x, n) {
  result <- 0
  for (k in 0:n) {
    result <- result + x^k / factorial(k)
  }
  return(result)
}

exp_series(0.5, 5)

