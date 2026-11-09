forward_difference <- function(f, x, h) {
  (f(x+h) - f(x))/h
}

f <- function(x) {
  x^2
}

forward_difference(f, 2, 0.1)



backward_difference <- function(f, x, h) {
  (f(x) - f(x-h)) / h
}

f <- function(x) {
  x^2
}



backward_difference(f, 2, 0.1)


central_difference <- function(f, x, h) {(f(x + h) - f(x - h)) / (2*h)
}

f <- function(x) {
  x^2
}



central_difference(f, 2, 0.1)
