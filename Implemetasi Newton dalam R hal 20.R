divided_difference <- function(x, y) {
  
  n <- length(x)
  
  table <- matrix(
    0,
    nrow = n,
    ncol = n
  )
  
  table[, 1] <- y
  
  for (j in 2:n) {
    
    for (i in 1:(n-j+1)) {
      
      table[i, j] <- 
        (table[i+1, j-1] - 
           table[i, j-1]) / 
        (x[i+j-1] - x[i])
    }
  }
  
  return(table)
}