actual <- 2*x

actual

error_forward <- abs (forward - actual)
error_backward <- abs (backward - actual)
error_central <- abs (central - actual)

error_forward
error_backward
error_central