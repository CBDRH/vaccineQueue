# Functions to generate arrival times based on different booking systems

arrivals <- function(n, interval, mean, sd, ns, startMin = 120, stopMin = 590){
  
    #(arrivals run from 120 mins because the reconsitution process starts at time zero, i.e. two hours before first appointment)
  appointment <- sort(rep(seq(startMin, stopMin, by = interval), n))
  
  nApp <- length(appointment)
  
  # Punctuality (mostly early or on time/occasionally late)
  punctuality <- rnorm(nApp, mean, sd) 
  
  # Occasional No shows
  noShow <- rbinom(nApp, 1, ns)
  
  # Net arrival time (must be positive)
  arrival <- sort(pmax(startMin, queuecomputer::lag_step(appointment, punctuality)[!noShow]))
  
  return(arrival)
}


## Arrivals every 10 minutes for 8 hours
arrivalsA <- function(rate, mean, sd, ns){
  
  # Scheduled appointment every 10 mins
  #(arrivals run from 120 mins because the reconsitution process starts at time zero, i.e. two hours before first appointment)
  appointment <- sort(rep(seq(120, 590, by = 10), rate))
  
  nApp <- length(appointment)
  
  # Punctuality (mostly early or on time/occasionally late)
  punctuality <- rnorm(nApp, mean, sd) 
  
  # Occasional No shows
  noShow <- rbinom(nApp, 1, ns)
  
  # Net arrival time (must be positive)
  arrival <- sort(pmax(0, queuecomputer::lag_step(appointment, punctuality)[!noShow]))
  
  return(arrival)
}


## Arrivals every 10 minutes for 8 hours
arrivalsB <- function(rate, disp, ns){
  
  # Scheduled appointment every 10 mins
  appointment <- sort(rep(seq(120, 590, by = 60), rate))
  
  nApp <- length(appointment)
  
  # Punctuality (mostly early or on time/occasionally late)
  punctuality <- rexp(nApp, 1/disp) 
  
  # Occasional No shows
  noShow <- rbinom(nApp, 1, ns)
  
  # Net arrival time (must be positive)
  arrival <- sort(pmax(0, queuecomputer::lag_step(appointment, punctuality)[!noShow]))
  
  return(arrival)
}

