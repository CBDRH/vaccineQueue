# Purpose: Explore the `queuecomputer` package with a basic four-node queue

# Load the package
library(queuecomuter)
library(dplyr)
library(ggplot2)
library(visNetwork)


# For reproducibility
set.seed(1641)

# Queue with four nodes: Registration -> Assessment -> Vaccination -> Observation

n <- 100 # Number of customers

rate <- c(rep(0.1, 20), rep(0.5, 10), rep(0.1, 40), rep(0.5, 20), rep(0.1, 10))

arrivals <- cumsum(rexp(n, rate = rate))

arrivalsDF <- data.frame(
  t = parse_date_time("01-03-2021 09:00", "dmY HM") + arrivals*60
)

ggplot(data = arrivalsDF, aes(x = t)) + 
        geom_histogram(color = 'black', fill = 'pink', binwidth = 60*60, boundary = 0) +
        scale_x_datetime(name = 'Time', breaks = 'hours', date_labels = '%H:%M') +
        scale_y_continuous(name = 'Number of arrivals') + 
        theme_dark()
 
regTime <- rexp(n, 1/5) # Time in registration queue
assTime <- rexp(n, 1/10) # Time in assessment
vacTime <- rexp(n, 1/5) # Time for vaccination
obsTime <- rnorm(n, 15, 2) # Observation time

registration <- queue_step(arrivals = arrivals, service = regTime, servers = 1) 
assessment <- queue_step(arrivals = registration$departures, service = assTime, servers = 1)  
vaccination <- queue_step(arrivals = assessment$departures, service = vacTime, servers = 1)  
observation <- lag_step(arrivals = vaccination$departures, service = obsTime)

ggplot(registration$queuelength_df, aes(times, queuelength)) + geom_point()
ggplot(assessment$queuelength_df, aes(times, queuelength)) + geom_point()
ggplot(vaccination$queuelength_df, aes(times, queuelength)) + geom_point()


