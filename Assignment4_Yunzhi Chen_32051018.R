generate_arma <- function(n = 100, c = 0, phi = NULL, theta = NULL, sigma = 1) {
  
  if(any(abs(polyroot(c(1,-phi))) <= 1)) {
    stop("The model is not stationary")
  }
  
  if(any(abs(polyroot(c(1,theta))) <= 1)){
    stop("The MA parameters is not invertible")
  }
  
  # Generate errors
  error <- rnorm(n, mean = 0, sd = sigma)
  
  # Set up vector for the response with initial values set to 0
  y <- rep(0, n)
  # Generate remaining observations
  for(i in seq(length(phi) + 1, length = n-length(phi))) {
    y[i] <- c + sum(phi * rev(y[(i - length(phi)):(i - 1)])) + sum(theta * rev(error[(i - length(theta)):(i - 1)])) + error[i]
  }
  
  return(y)
}


library(fpp3)

generate_arma(n = 50, c = 2, phi = 1, theta = c(0.4, 0.2))

generate_arma(n = 50, c = 1, phi = c(0.8, 0.3, -0.2), theta = c(0.4, -0.2))

tsibble(time = 1:50, y = generate_arma(n = 50, c = 2, phi = c(0.4, -0.6)), index = time) %>%
  autoplot(y)

tsibble(time = 1:50, y = generate_arma(n = 50, c = 1, phi = c(0.2, 0.3, -1.2), theta = 0), index = time) %>%
  autoplot(y)



              