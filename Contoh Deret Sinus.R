sin_series <- function(x, n) {
  result <- 0
  for (k in 0:n) {
    power <- 2 * k + 1
    result <- result + (-1)^k * x^power / factorial(power)
  }
  return(result)
}

sin_series(0.5, 3)
sin(0.5)
