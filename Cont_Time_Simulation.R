# Write a program that simulates Markov chains from a continuous time
# process with p^(0) = (3, 4, 1)/8 and the Q matrix as follows:

Q <- matrix(c(-4, 4, 0,
              3, -4, 1,
              0, 4, 4), nrow=3, ncol=3, byrow=TRUE)

# Get the initial state from p^(0)
X_prelim <- sample(c(1, 2, 3, 4, 5, 6, 7, 8), 1)
if (X_prelim <= 3) {
  X_0 <- 0
} else if ((X_prelim > 3) && (X_prelim != 8)) {
  X_0 <- 1
} else {
  X_0 <- 2
}
# Now run the chain for 10,000 steps, and retain how much time was spent
# in each state

# Xs holds the state chain, times holds the proportion of time in each state
Xs <- X_0
total_time <- 0
times <- c(0, 0, 0)

while (length(Xs) < 10000) {
  if (Xs[length(Xs)] == 0) {
    # We always go to state 1 from state 0
    time <- rexp(1, rate = 4)
    total_time <- total_time + time
    times[1] <- times[1] + time
    
    Xs <- c(Xs, 1)
  } else if (Xs[length(Xs)] == 1) {
    # The matrix stipulates that we either go into state 0
    # with rate 3, or state 2 with rate 1
    time0 <- rexp(1, rate = 3)
    time2 <- rexp(1, rate = 1)
    total_time <- total_time + min(time0, time2)
    times[2] <- times[2] + min(time0, time2)
    
    if (time0 < time2) {
      Xs <- c(Xs, 0)
    } else {
      Xs <- c(Xs, 2)
    }
  } else {
    # We always go to state 1 from state 2
    time <- rexp(1, rate = 4)
    total_time <- total_time + time
    times[3] <- times[3] + time
    
    Xs <- c(Xs, 1)
  }
}

for (i in seq(length(times))) {
  times[i] <- times[i] / total_time
}



