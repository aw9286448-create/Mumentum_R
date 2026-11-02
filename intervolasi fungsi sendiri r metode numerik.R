intervolasi_linear <- function(x0, y0, x1, y1, x) {
  y <- y0 + ((y1 - y0) / (x1 - x0)) * (x - x0)
  return(y)
}

intervolasi_linear(
  x0 = 1,
  y0 = 2,
  x1 = 2,
  y1 = 5,
  x  = 1.5
)

x_data <- c(0, 1, 2, 3)
y_data <- c(1, 2, 5, 10)

hasil <- approx(x = x_data, y = y_data, xout = 1.5, method = "linear")
hasil

-