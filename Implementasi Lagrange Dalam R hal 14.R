lagrange <- function(x_data, y_data, x) {
  
  n <- length(x_data)
  result <- 0 
  
  for (i in 1:n) {
    
    L <- 1
    
    for (j in 1:n) {
      
      if (j !=i) {
        L <- L * 
          (x - x_data[j]) /
          (x_data[i] - x_data[j])
      }
    }
    
    result <- result + y_data[i] * L
  }
  
  return(result)
}