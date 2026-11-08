x <- 0.5
n <- 0:10
approximation <- sapply(
  n,
  function(k) exp_series(x, k)
)

data.frame(
  n = n,
  approximation = approximation
)

true_value <- exp(x)
error <- abs(true_value - approximation)

data.frame(
  n = n,
  approximation = approximation,
  error = error
)
