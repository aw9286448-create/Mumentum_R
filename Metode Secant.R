secant <- function(f, x0, x1, tol = 1e-6, max_iter = 100) {
  hasil <- data.frame()
  
  for (i in 1:max_iter) {
    f0 <- f(x0)
    f1 <- f(x1)
    
    if (f1 - f0 == 0) {
      stop("Penyebut bernilai nol.")
    }
    
    x2 <- x1 - f1 * (x1 - x0) / (f1 - f0)
    
    error <- abs(x2 - x1)
    
    hasil <- rbind(
      hasil,
      data.frame(
        iterasi = i,
        x_sebelumnya = x0,
        x_sekarang = x1,
        x_baru = x2,
        error = error
      )
    )
    
    if (error < tol) {
      x1 <- x2
      break
    }
    
    x0 <- x1
    x1 <- x2
  }
  
  list(
    akar = x1,
    iterasi = hasil
  )
}

# Jalankan Secant
hasil_secant <- secant(
  f = f,
  x0 = 1,
  x1 = 2
)

hasil_secant$akar
hasil_secant$iterasi