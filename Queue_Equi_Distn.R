# For stationary M/M/k/N systems, write a program that takes
# as input any value of λ, μ (with their usual meanings), k and N,
# and outputs a description of the equilibrium distribution (if it exists)
# together with its mean.

eq_distribution <- function(k, N=Inf, lambda, mu) {
  if (N < k) {
    print("N cannot be less than k")
    return()
  }
  
  rho <- lambda / mu
  
  if (N == Inf) {
    if ((k == 1) && (rho < 1)) {
      mean <- rho / (1 - rho)
      print(paste("The equilibrium distribution is geometric with mean", mean))
      return()
    } else if ((k == 1) && (rho >= 1)) {
      print("No equilibrium distribution exists")
      return()
    } else if (k == Inf) {
      print(paste("The equilibrium distribution is Poisson with mean", rho))
      return()
    } else {
      # Here k is finite, and N is infinite. The equilibrium distribution
      # is infinite, so we'll return its pmf as a function.
      if (rho >= k) {
        print("There is no equilibrium distribution with N infinite and rho >= k")
        return()
      } else {
        # First work out pi_0
        finite_sum <- 0
        for (i in 0:k) {
          finite_sum <- finite_sum + ((1 / factorial(i)) * (rho ^ i))
        }
        infinite_sum <- (1 / factorial(k)) * (rho ^ k) * (rho / (k - rho))
        
        pi_0 <- 1 / (finite_sum + infinite_sum)
        
        # Then work out the expectation
        exp_finite_sum <- 0
        for (i in 1:k) {
          exp_finite_sum <- exp_finite_sum + (1 / factorial(i - 1)) * (rho ^ i)
        }
        exp_infinite_sum <- ((rho ^ k) * (k + 1 - rho)) / ((factorial(k)) * ((1 - (rho / k)) ^ 2))
        
        expectation <- pi_0 * (exp_finite_sum + exp_infinite_sum)
        print(paste("The mean is", expectation))
        
        # Now construct the pmf
        distribution_function <- function(val) {
          if ((1 <= val) && (val <= k)) {
            ((1/factorial(val)) * (rho ^ val)) * pi_0
          } else {
            (((rho / k) ^ (val - k)) * (1 / factorial(k)) * (rho ^ k) * pi_0)
          }
        }
        
        # and return it.
        distribution_function
      }
    }
  } else if (N != Inf) {
    sum1 <- 0
    sum2 <- 0
    
    for (i in 0:k) {
      sum1 <- sum1 + ((1/factorial(i)) * (rho ^ i))
    }
    for (i in (k + 1):N) {
      sum2 <- sum2 + (((rho / k) ^ (i - k)) * (1 / factorial(k)) * (rho ^ k))
    }
    
    pi_0 <- 1/(sum1 + sum2)
    
    pi_dist <- pi_0
    
    for (i in 1:k) {
      pi_dist <- c(pi_dist, (1 / factorial(i)) * (rho ^ i) * pi_0)
    }
    for (i in (k+1):N) {
      pi_dist <- c(pi_dist, (((rho / k) ^ (i-k)) * (1 / factorial(k)) * (rho ^ k) * pi_0))
    }
    
    mean <- 0
    for (i in 0:(length(pi_dist) - 1)) {
      mean <- mean + i * pi_dist[i + 1]
    }
    
    print(paste("The mean is", mean))
    pi_dist
  }
}
