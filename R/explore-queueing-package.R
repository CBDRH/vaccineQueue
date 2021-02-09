# Purpose: Explore the `queueing` package with a basic four-node queue

# Load the package
library(queueing)
library(dplyr)
library(visNetwork)

# Queue with four nodes: Registration -> Assessment -> Vaccination -> Observation

# 99% of people proceed from registration to asessment
# 99% of people proceed from assessment to vaccination 
# 100% of people proceed from vaccination to Observation 
# 1% of people remain in the observation area

# Define these transitions:
prob <- matrix(c(0, 0.99, 0, 0,
                 0, 0, 0.99, 0,
                 0, 0, 0, 1.0,
                 0, 0, 0, 0.01), 
               byrow = TRUE, 
               nrow = 4, 
               ncol=4)

# Assumptions about arrivals
regLambda <- 0.25 # Average number of arrivals to vaccine facility per minute

# Assumptions about processing times
regMuInv <- 2       # Average time in minutes spent at registration
assMuInv <- 4       # Average time in minutes spent at assessment
vacMuInv <- 2       # Average time in minutes spent at vaccination
obsMuInv <- 15      # Average time in minutes spent in observation

# Assumptions about available servers at each node
regC <- 1           # Number of medical admin staff to check in at reception desk (or terminals in case of self-check in)
assC <- 4           # Number of healthcare staff to perform pre-vaccination health assessment
vacC <- 4           # Number of healthcare staff to perform administer vaccinations
obsC <- 10          # Capacity of observation area  

# Specify the parameters for each node in the queue
# (lambda = 0 means you can't arrive into the assessment/vaccination or observation areas directly 
#   from outside, i.e. must go through queue)
reg <- NewInput.MMC(lambda = regLambda, mu = 1/regMuInv, n = 0, c = regC)
ass <- NewInput.MMC(lambda = 0, mu = 1/assMuInv, c = assC)
vac <- NewInput.MMC(lambda = 0, mu = 1/vacMuInv, c = vacC)
obs <- NewInput.MMC(lambda = 0, mu = 1/obsMuInv, c = obsC)

# Specify all inputs to the queue (An Open Jackson Network)
vaccineHub <- NewInput.OJN(prob=prob, reg, ass, vac, obs)

# Run the model
vaccineModel <- QueueingModel(vaccineHub)

# Summarise the model
summary(vaccineModel)

# Print some key statistics
cat("long-run average number of customers in the system:", round(vaccineModel$L), "patients")
cat("long-run average time spent in the system:", round(vaccineModel$W), "minutes")
cat("Probability that a vaccinator is idle: ", round(100*vaccineModel$Pn[3]), '%' ,sep = '')
cat("Long run average of patients in observation area:", round(vaccineModel$Lk[4]))


# Visualise the hypotesised queue

## Nodes
nodes <- data.frame(id = 1:6, 
                    label = c('Entry', 'Registration', 'Assessment', 'Vaccination', 'Observation', 'Exit'),
                    group = c("Outside", "Admin", "Clinical", "Clinical", 'Admin', "Outside"),
                    level = c(1:6)) 
## Edges
edges <- data.frame(from = c(1, 2, 3, 4, 5), 
                    to = c(2, 3, 4, 5, 6),
                    title = c(paste(regLambda, "per minute"), 
                              paste("p =", prob[1,2]), 
                              paste("p =", prob[2,3]), 
                              paste("p =", prob[3,4]), 
                              paste('leave after', obsMuInv, "minutes"))
)

## Visualise the network
visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
  visGroups(groupname = "Admin", color = "orange", shape = "square") %>%
  visGroups(groupname = "Clinical", color = "blue", shape = "triangle") %>% 
  visGroups(groupname = "Outside", color = "lightgrey", shape = "rectangle") %>% 
  visEdges(arrows = "to") %>%
  visHierarchicalLayout(direction = "LR") %>%
  visLegend(position = "right")


