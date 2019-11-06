# Write a program that simulates a discrete time stochastic process
# Output plots of the distribution.

discrete_markov <- function(iterations) {
  # Initial Distribution
  p_0 = c(0.5, 0, 0.5)
  
  # Transition Matrix
  P = matrix(c(1/2, 1/2, 0,
               1/2, 1/4, 1/4,
               0, 1/3, 2/3), nrow=3, ncol=3, byrow=TRUE)
  # P is the wrong way round
  P <- t(P)
  
  for (i in vector(length=iterations)) {
    # Reshape for the barplot
    p_reshape <- matrix(p_0, nrow=3, ncol=1)
    barplot(p_reshape, col=c("red", "green", "blue"), axes=FALSE)
    
    # Make the transition
    p_0 <- p_0 %*% P
    Sys.sleep(0.1)
  }
}

discrete_markov(100)
