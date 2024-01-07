library(tidyverse)
library(fpp3)


generate_ar1 <- function(n = 100, c = 0, phi, sigma = 1) {
  # Generate errors
  error <- rnorm(n, mean = 0, sd = sigma)
  
  # Set up vector for the response with initial values set to 0
  y <- rep(0, n)
  # Generate remaining observations
  for(i in seq(2, length = n-1)) {
    y[i] <- c + phi * y[i-1] + error[i]
  }
  return(y)
}

tsibble(time = 1:50, y = generate_ar1(n=50, c=1, phi=0.8), index = time) %>%
  autoplot(y)

generate_arma <- function(n = 100, c = 0, phi = NULL, theta = NULL, sigma = 1)
#The noise should be generated using the rnorm() function.
#You can use the stop() function to generate an error.

generate_arma(n = 50, c = 2, phi = c(0.4, -0.6)) # yt = 2 + 0.4y(t-1) - 0.6y(t-2) + et

!any(abs(polyroot(c(1,-phi))) <= 1)
!any(abs(polyroot(c(1,theta))) <= 1)







generate_arma <- function(n = 100, c = 0, phi = NULL, theta = NULL, sigma = 1) {
  
  if(any(abs(polyroot(c(1,-phi))) <= 1)) {
    stop("The model is not stationary”)
  }
  
  if(any(abs(polyroot(c(1,theta))) <= 1)) {
  stop(The MA parameters is not invertible”)
    }
  
  # Generate errors
  error <- rnorm(n, mean = 0, sd = sigma)
  
  # Set up vector for the response with initial values set to 0
  y <- rep(0, n)
  # Generate remaining observations
  for(i in seq(3, length = n-2)) {
    y[i] <- c + phi * y[i-1] + theta * y[i-2] + error[i]
  }
  
  return(y)
}
  

    


library(fpp3)
tsibble(time = 1:100, y = generate_arma(n = 50, c = 2, phi = c(0.4, -0.6))) %>%
  autoplot(y)





generate_arma <- function(n = 100, c = 0, phi = NULL, theta = NULL, sigma = 1) {


# Generate errors
  error <- rnorm(n, mean = 0, sd = sigma)
  
  # Set up vector for the response with initial values set to 0
  y <- rep(0, n)
  # Generate remaining observations
  for(i in seq(3, length = n-2)) {
    y[i] <- c + phi * y[i-1] + theta * y[i-2] + error[i]
  }
  
  return(y)



}


