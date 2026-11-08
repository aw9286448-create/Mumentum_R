exp_series_tol <- function(x, tol = 1e-10, max_iter = 100) {
  sum <- 1
  term <- 1
  
  for (k in 1:max_iter) {
    term <- term * x / k
    new_sum <- sum + term
    
    if (abs(new_sum - sum) < tol) {
      return(list(
        approximation = new_sum,
        terms = k + 1
      ))
    }
    
    sum <- new_sum
  }
  
  return(list(
    approximation = sum,
    terms = max_iter + 1
  ))
}

exp_series_tol(0.5)
