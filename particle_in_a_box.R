# A simulation of n particles in a box with 2 chambers and a permeable partition
library(R.utils)

# 2 ^ n states can be enumerated by the numbers {0,..., 2 ^ n - 1} and transitions
# exist between states a Hamming distance 1 apart when their index in binary is
# a Hamming distance 1 apart

Hamming_dist <- function(state_1, state_2) {
  if (length(state_1) != length(state_2)) {
    print("States must be the same length")
    return(-1)
  }
  
  dist <- 0
  
  for (i in 1:length(state_1)) {
    if (state_1[i] != state_2[i]) {
      dist <- dist + 1
    }
  }
  return(dist)
}

volume <- function(state) {
  # this only works with 2 chambers, need to compute multinomial coeffs otherwise
  return(choose(length(state), sum(state)))
}

entropy <- function(state) {
  return(log(volume(state)))
}

get_states <- function(n) {
  # Outputs an array of states of length n (i.e. of boxes with n particles in them)
  states <- array(dim = c(1, n, 2 ^ n))
  for (i in 0:(2 ^ n - 1)) {
    # this onyl works in binary, can implement a generic to_base function
    new_state_string <- intToBin(i)
    while (nchar(new_state_string) != n) {
      new_state_string <- paste("0", new_state_string, sep = "")
    }
    for (j in 1:n) {
      states[1, j, i + 1] <- as.integer(substr(new_state_string, j, j))
    }
  }
  return(states)
}

next_state <- function(states, current) {
  out <- NULL
  n <- length(states[, , 1])
  
  while (is.null(out)) {
    number <- sample.int(2 ^ n, 1)
    proposal <- states[, , number]
    
    if (Hamming_dist(proposal, current) == 1) {
      out <- proposal
    }
  }
  return(list(state = out,
              number = number))
}

box <- function(n,
                nits) {
  # Simulates n particles in a box with a partition using a continuous time Markov chain
  # Always starts from the 0 state.
  times <- rexp((nits + 1))
  history <- array(dim = c(1, n, (nits + 1)))
  state_number_history <- vector(length = nits)
  entropies <- vector(length = nits)
  
  states <- get_states(n)
  
  # Start the box in the 0 state (i.e. all the particles in one chamber)
  state <- states[, , 1]
  state_number_history[1] <- 1
  entropies[1] <- entropy(state)
  
  history[, , 1] <- state
  
  for (i in 1:nits) {
    # choose a new state
    new_list <- next_state(states, state)
    state <- new_list[['state']]
    
    state_number_history[(i + 1)] <- new_list[['number']]
    history[, , (i + 1)] <- state
    entropies[(i + 1)] <- entropy(state)
  }
  return(list(times = times,
              history = history,
              state_number_history = state_number_history,
              entropies = entropies))
}

n <- 17
nits <- 200
bx <- box(n, nits)

df <- data.frame(time = cumsum(bx$times), entropy = bx$entropies[1:(length(bx$entropies) - 1)])

entropy_vs_time <- ggplot(df, aes(x = time, y = entropy, color = entropy)) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red") +
  ggtitle("Entropy vs Time (17 particles)") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = 'beige'),
        plot.background = element_rect(fill = 'azure'))
entropy_vs_time




