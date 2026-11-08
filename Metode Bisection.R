bisection <- function(f, a, b, tol = 1e-6, max_iter = 100) {
  if (f(a) * f(b) > 0) {
    stop("Interval tidak mengandung perubahan tanda.")
  }
  
  tabel <- data.frame()
  
  for (i in 1:max_iter) {
    c <- (a + b) / 2
    fc <- f(c)
    
    tabel <- rbind(
      tabel,
      data.frame(
        iterasi = i,
        a = a,
        b = b,
        c = c,
        f_c = fc,
        error = abs(b - a)
      )
    )
    
    if (abs(b - a) < tol || abs(fc) < tol) {
      break
    }
    
    if (f(a) * fc < 0) {
      b <- c
    } else {
      a <- c
    }
  }
  
  return(list(akar = c, tabel = tabel))
}


f <- function(x) {
  return(x^4 + 2*x^2 - 3)
}


hasil_biseksi <- bisection(
  f = f, 
  a = 0.5,
  b = 2
)


hasil <- hasil_biseksi$tabel


plot(
  hasil$iterasi, hasil$c,
  type = "b",
  pch = 19,
  col = "black",
  xlab = "Iterasi",
  ylab = "Estimasi akar (c)",
  main = expression(paste("Konvergensi Metode Biseksi\n", f(x) == x^4 + 2*x^2 - 3, " on [0.5, 2]")),
  panel.first = grid()
)


abline(h = hasil_biseksi$akar, col = "red", lty = 2, lwd = 1.5)
text(0.3, 1.25, "c = 1.25", pos = 4)
points(max(hasil$iterasi), hasil_biseksi$akar, col = "blue", pch = 19, cex = 1.8)
text(18.5, 1.1, "Akar Eksak (x=1)", col = "black", font = 2)
text(18.5, 0.9, paste("Hasil (Iterasi", max(hasil$iterasi), ")"), col = "black", font = 2)

