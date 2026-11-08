fixed_point <- function(g, x0, tol = 1e-6, max_iter = 100) {
  hasil <- data.frame()
  
  x <- x0
  
  for (i in 1:max_iter) {
    x_new <- g(x)
    error <- abs(x_new - x)
    
    hasil <- rbind(
      hasil,
      data.frame(
        iterasi = i,
        x_lama = x,
        x_baru = x_new,
        error = error
      )
    )
    
    if (error < tol) {
      x <- x_new
      break
    }
    
    x <- x_new
  }
  
  list(
    akar = x,
    iterasi = hasil
  )
}

g <- function(x) {
  sqrt(2 - x)
}


hasil_fp <- fixed_point(
  g = g,
  x0 = 0
)

hasil_fp$akar
hasil_fp$iterasi

