# Write an R function that accepts arrival
# times and service times as input and outputs a plot of
# the cumulative amount of people in the system, amount of people the queue,
# and the amount of people that have left 

stepwiseQueueGraph <- function(arrivals, times) {
  # Create a list of times at which items enter the service regime
  # and a list of times at which items leave the service regime.
  for (i in seq(length(arrivals))) {
    if (i == 1) {
      entries <- arrivals[i]
      departures <- entries[i] + times[i]
    } else {
      entries <- c(entries, max(arrivals[i], departures[i - 1]))
      departures <- c(departures, entries[i] + times[i])
    }
  }
  
  # Now create a plot with the step functions on it
  
  # First create a dummy vector as the 'y's (dummy, because each line only
  # increases by one each time)
  number <- seq(arrivals)
  
  # Need to add a 0 point to each line
  arrivals <- c(0, arrivals)
  entries <- c(0, entries)
  departures <- c(0, departures)
  number <- c(0, number)
  
  df <- data.frame(arrivals, entries, departures, number)
  
  # Make the plot, with departures plotted first, because the departures
  # vector will span the longest time, so the graph should easily accommodate
  # the other lines.
  ggplot(df, aes(y=number)) + geom_step(aes(x=departures), color='darkred') +
    geom_step(aes(x=arrivals), color='darkblue') + geom_step(aes(x=entries), color='darkgreen') +
    xlab('time') + scale_x_continuous(breaks = seq(0, max(departures) + 1, by=2)) +
    scale_y_continuous(breaks = seq(0, max(number) + 1, by=1))
}

# Tests
stepwiseQueueGraph(c(5,13,25,28,38,40,60, 63), c(14,16,7,3,8,8,4,3))

