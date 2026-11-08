regula_falsi <- function(f, a, b, tol = 1e-6, max_iter = 100) {
  if (f(a) * f(b) > 0) {
    stop("Interval tidak mengandung perubahan tanda.")
  }
  
  hasil <- data.frame()
  
  for (i in 1:max_iter) {
    c <- (a * f(b) - b * f(a)) / (f(b) - f(a))
    fc <- f(c)
    
    hasil <- rbind(
      hasil,
      data.frame(
        iterasi = i,
        a = a,
        b = b,
        c = c,
        f_c = fc
      )
    )
    
    if (abs(fc) < tol) {
      break
    }
    
    if (f(a) * fc < 0) {
      b <- c
    } else {
      a <- c
    }
  }
  
  list(
    akar = c,
    iterasi = hasil
  )
}