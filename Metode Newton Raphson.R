newton_raphson <- function(f, df, x0, tol = 1e-6, max_iter = 100) {
  tabel <- data.frame()
  x_curr <- x0
  
  for (i in 1:max_iter) {
    fx <- f(x_curr)
    dfx <- df(x_curr)
    
    if (dfx == 0) {
      stop("Turunan bernilai nol. Metode gagal.")
    }
    
    x_next <- x_curr - (fx / dfx)
    err <- abs(x_next - x_curr)
    
    tabel <- rbind(
      tabel,
      data.frame(
        iterasi = i,
        x_lama = x_curr,
        f_x = fx,
        f_prime = dfx,
        x_baru = x_next,
        error = err
      )
    )
    
    if (err < tol) {
      break
    }
    
    x_curr <- x_next
  }
  
  return(list(akar = x_next, tabel = tabel))
}


f <- function(x) {
  return(x^4 + 2*x^2 - 3)
}

df <- function(x) {
  return(4*x^3 + 4*x)
}


hasil_newraph <- newton_raphson(
  f = f, 
  df = df, 
  x0 = 1.5)


hasil_nr <- hasil_newraph$tabel


plot(
  hasil_nr$iterasi, hasil_nr$x_baru,
  type = "b",
  pch = 19,
  col = "black",
  xlab = "Iterasi",
  ylab = "Estimasi akar (x)",
  main = expression(paste("Konvergensi Metode Newton-Raphson\n", f(x) == x^4 + 2*x^2 - 3, " dengan ", x[0] == 1.5)),
  panel.first = grid()
)


abline(h = hasil_newraph$akar, col = "red", lty = 2, lwd = 1.5)
points(max(hasil_nr$iterasi), hasil_newraph$akar, col = "blue", pch = 19, cex = 1.8)
text(max(hasil_nr$iterasi) - 0.8, hasil_newraph$akar + 0.05, "Akar Eksak (x=1)", col = "black", font = 2)
text(max(hasil_nr$iterasi) - 0.8, hasil_newraph$akar - 0.05, paste("Hasil (Iterasi", max(hasil_nr$iterasi), ")"), col = "black", font = 2)
