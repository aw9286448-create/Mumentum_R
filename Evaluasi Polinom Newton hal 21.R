newton <- function(x_data, y_data, x) {
  
  n <- length(x_data)
  
  dd <- divided_difference(
    x_data,
    y_data
  )
  
  result <- dd[1, 1]
  product <- 1
  
  if (n >= 2) {
    
    for (j in 2:n) {
      
      product <- product *
        (x - x_data[j-1])
      
      result <- result +
        dd[1, j] * product
    }
  }
  
  return(result)
}