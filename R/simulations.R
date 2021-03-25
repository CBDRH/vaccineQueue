######################################################################################################################################
# Purpose:  Run queue networt simulations for mass vaccination models (arenaModel.R) and GP vaccination clinics (gpModel.R)
# Outputs:  Data/queueTimesGP.Rda
#           Data/queueTimesRPA.Rda  
# Author:   Mark Hanly
######################################################################################################################################

# libraries
library(tidyverse)
library(queuecomputer)

# Source functions
source('R/arenaModel.R')
source('R/gpModel.R')
source('R/arrivals.R')
source('R/helperFunctions.R')


# Set the number of simulations for each scenario
simN <- 20

# Large scale 'arena' style models, based on the RPA

# 21 staff

rpa21a <- list()
rpa21b <- list()
rpa21c <- list()
rpa21d <- list()
rpa21e <- list()
rpa21f <- list()
rpa21g <- list()
rpa21h <- list()
rpa21i <- list()
rpa21j <- list()
rpa21k <- list()
rpa21l <- list()
rpa21m <- list()
rpa21n <- list()
rpa21o <- list()

for(i in 1:simN){
  ##  Delays in vaccination time
  rpa21a[[i]] <- arenaModel(arrivals = arrivals(10, 60, 0, 5, 0.02), nRsi = 2, nEnt = 4, nReg = 6, nAss = 4, nVac = 5)
  rpa21b[[i]] <- arenaModel(arrivals = arrivals(35, 60, 0, 5, 0.02), nRsi = 2, nEnt = 4, nReg = 6, nAss = 4, nVac = 5)
  rpa21c[[i]] <- arenaModel(arrivals = arrivals(60, 60, 0, 5, 0.02), nRsi = 2, nEnt = 4, nReg = 6, nAss = 4, nVac = 5)
  rpa21d[[i]] <- arenaModel(arrivals = arrivals(85, 60, 0, 5, 0.02), nRsi = 2, nEnt = 4, nReg = 6, nAss = 4, nVac = 5)
  rpa21e[[i]] <- arenaModel(arrivals = arrivals(110, 60, 0, 5, 0.02), nRsi = 2, nEnt = 4, nReg = 6, nAss = 4, nVac = 5)

  ## Decreases in staff numbers
  rpa21f[[i]] <- arenaModel(arrivals = arrivals(60, 60, 0, 5, 0.02), nRsi = 2, nEnt = 4, nReg = 6, nAss = 4, nVac = 5)
  rpa21g[[i]] <- arenaModel(arrivals = arrivals(60, 60, 0, 5, 0.02),  nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5)
  rpa21h[[i]] <- arenaModel(arrivals = arrivals(60, 60, 0, 5, 0.02),  nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 4)
  rpa21i[[i]] <- arenaModel(arrivals = arrivals(60, 60, 0, 5, 0.02),  nRsi = 2, nEnt = 3, nReg = 5, nAss = 4, nVac = 4)
  rpa21j[[i]] <- arenaModel(arrivals = arrivals(60, 60, 0, 5, 0.02),  nRsi = 2, nEnt = 3, nReg = 5, nAss = 3, nVac = 4)

  ## increases in arrival rate
  rpa21k[[i]] <- arenaModel(arrivals = arrivals(60+0, 60, 0, 5, 0.02), nRsi = 2, nEnt = 4, nReg = 6, nAss = 4, nVac = 5)
  rpa21l[[i]] <- arenaModel(arrivals = arrivals(60+10, 60, 0, 5, 0.02), nRsi = 2, nEnt = 4, nReg = 6, nAss = 4, nVac = 5)
  rpa21m[[i]] <- arenaModel(arrivals = arrivals(60+20, 60, 0, 5, 0.02), nRsi = 2, nEnt = 4, nReg = 6, nAss = 4, nVac = 5)
  rpa21n[[i]] <- arenaModel(arrivals = arrivals(60+30, 60, 0, 5, 0.02), nRsi = 2, nEnt = 4, nReg = 6, nAss = 4, nVac = 5)
  rpa21o[[i]] <- arenaModel(arrivals = arrivals(60+40, 60, 0, 5, 0.02), nRsi = 2, nEnt = 4, nReg = 6, nAss = 4, nVac = 5)
}


# 42 staff

rpa42a <- list()
rpa42b <- list()
rpa42c <- list()
rpa42d <- list()
rpa42e <- list()
rpa42f <- list()
rpa42g <- list()
rpa42h <- list()
rpa42i <- list()
rpa42j <- list()
rpa42k <- list()
rpa42l <- list()
rpa42m <- list()
rpa42n <- list()
rpa42o <- list()

for(i in 1:simN){
  
  ##  Delays in vaccination time 
  rpa42a[[i]] <- arenaModel(arrivals = arrivals(20, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, nVac = 10)
  rpa42b[[i]] <- arenaModel(arrivals = arrivals(70, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, nVac = 10)
  rpa42c[[i]] <- arenaModel(arrivals = arrivals(120, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, nVac = 10)
  rpa42d[[i]] <- arenaModel(arrivals = arrivals(170, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, nVac = 10)
  rpa42e[[i]] <- arenaModel(arrivals = arrivals(220, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, nVac = 10)

  ## Decreases in staff numbers
  rpa42f[[i]] <- arenaModel(arrivals = arrivals(120, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, nVac = 10)
  rpa42g[[i]] <- arenaModel(arrivals = arrivals(120, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 11, nAss = 8, nVac = 10)
  rpa42h[[i]] <- arenaModel(arrivals = arrivals(120, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 11, nAss = 8, nVac = 9)
  rpa42i[[i]] <- arenaModel(arrivals = arrivals(120, 60, 0, 5, 0.02), nRsi = 4, nEnt = 7, nReg = 11, nAss = 8, nVac = 9)
  rpa42j[[i]] <- arenaModel(arrivals = arrivals(120, 60, 0, 5, 0.02), nRsi = 4, nEnt = 7, nReg = 11, nAss = 7, nVac = 9)
  
  ## increases in arrival rate
  rpa42k[[i]] <- arenaModel(arrivals = arrivals(120+0, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, nVac = 10)
  rpa42l[[i]] <- arenaModel(arrivals = arrivals(120+10, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, nVac = 10)
  rpa42m[[i]] <- arenaModel(arrivals = arrivals(120+20, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, nVac = 10)
  rpa42n[[i]] <- arenaModel(arrivals = arrivals(120+30, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, nVac = 10)
  rpa42o[[i]] <- arenaModel(arrivals = arrivals(120+40, 60, 0, 5, 0.02), nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, nVac = 10)
}


# 63 staff

rpa63a <- list()
rpa63b <- list()
rpa63c <- list()
rpa63d <- list()
rpa63e <- list()
rpa63f <- list()
rpa63g <- list()
rpa63h <- list()
rpa63i <- list()
rpa63j <- list()
rpa63k <- list()
rpa63l <- list()
rpa63m <- list()
rpa63n <- list()
rpa63o <- list()

for(i in 1:simN){
  ## Delays in vaccination time
  rpa63a[[i]] <- arenaModel(arrivals = arrivals(60, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 18, nAss = 12, nVac = 15)
  rpa63b[[i]] <- arenaModel(arrivals = arrivals(120, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 18, nAss = 12, nVac = 15)
  rpa63c[[i]] <- arenaModel(arrivals = arrivals(180, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 18, nAss = 12, nVac = 15)
  rpa63d[[i]] <- arenaModel(arrivals = arrivals(240, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 18, nAss = 12, nVac = 15)
  rpa63e[[i]] <- arenaModel(arrivals = arrivals(300, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 18, nAss = 12, nVac = 15)
  
  ## Decreases in staff numbers
  rpa63f[[i]] <- arenaModel(arrivals = arrivals(180, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 18, nAss = 12, nVac = 15)
  rpa63g[[i]] <- arenaModel(arrivals = arrivals(180, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 17, nAss = 12, nVac = 15)
  rpa63h[[i]] <- arenaModel(arrivals = arrivals(180, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 17, nAss = 12, nVac = 14)
  rpa63i[[i]] <- arenaModel(arrivals = arrivals(180, 60, 0, 5, 0.02), nRsi = 5, nEnt = 11, nReg = 17, nAss = 12, nVac = 14)
  rpa63j[[i]] <- arenaModel(arrivals = arrivals(180, 60, 0, 5, 0.02), nRsi = 5, nEnt = 11, nReg = 17, nAss = 11, nVac = 14)
  
  ## increases in arrival rate
  rpa63k[[i]] <- arenaModel(arrivals = arrivals(180+0, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 18, nAss = 12, nVac = 15)
  rpa63l[[i]] <- arenaModel(arrivals = arrivals(180+10, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 18, nAss = 12, nVac = 15)
  rpa63m[[i]] <- arenaModel(arrivals = arrivals(180+20, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 18, nAss = 12, nVac = 15)
  rpa63n[[i]] <- arenaModel(arrivals = arrivals(180+30, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 18, nAss = 12, nVac = 15)
  rpa63o[[i]] <- arenaModel(arrivals = arrivals(180+40, 60, 0, 5, 0.02), nRsi = 6, nEnt = 12, nReg = 18, nAss = 12, nVac = 15)
}


## Compile all the queue time results
queueTimesRPA <- bind_rows(
                getQueueTime(rpa21a), getQueueTime(rpa21b), getQueueTime(rpa21c), getQueueTime(rpa21d), getQueueTime(rpa21e),
                getQueueTime(rpa21f), getQueueTime(rpa21g), getQueueTime(rpa21h), getQueueTime(rpa21i), getQueueTime(rpa21j),
                getQueueTime(rpa21k), getQueueTime(rpa21l), getQueueTime(rpa21m), getQueueTime(rpa21n), getQueueTime(rpa21o),
                getQueueTime(rpa42a), getQueueTime(rpa42b), getQueueTime(rpa42c), getQueueTime(rpa42d), getQueueTime(rpa42e),
                getQueueTime(rpa42f), getQueueTime(rpa42g), getQueueTime(rpa42h), getQueueTime(rpa42i), getQueueTime(rpa42j),
                getQueueTime(rpa42k), getQueueTime(rpa42l), getQueueTime(rpa42m), getQueueTime(rpa42n), getQueueTime(rpa42o),
                getQueueTime(rpa63a), getQueueTime(rpa63b), getQueueTime(rpa63c), getQueueTime(rpa63d), getQueueTime(rpa63e),
                getQueueTime(rpa63f), getQueueTime(rpa63g), getQueueTime(rpa63h), getQueueTime(rpa63i), getQueueTime(rpa63j),
                getQueueTime(rpa63k), getQueueTime(rpa63l), getQueueTime(rpa63m), getQueueTime(rpa63n), getQueueTime(rpa63o),
                .id = 'sim') %>% 
              mutate(
                size = 21*(floor(as.numeric(sim)/15.01) + 1),
                level = (as.numeric(sim) -1) %% 5 + 1,
                test = ceiling((as.numeric(sim) - 15*(floor(as.numeric(sim)/15.01)))/5)
              )  

# Compile all the utilisation time results
utilTimesRPA <- bind_rows(
                getUtilTime(rpa21a), getUtilTime(rpa21b), getUtilTime(rpa21c), getUtilTime(rpa21d), getUtilTime(rpa21e),
                getUtilTime(rpa21f), getUtilTime(rpa21g), getUtilTime(rpa21h), getUtilTime(rpa21i), getUtilTime(rpa21j),
                getUtilTime(rpa21k), getUtilTime(rpa21l), getUtilTime(rpa21m), getUtilTime(rpa21n), getUtilTime(rpa21o),
                getUtilTime(rpa42a), getUtilTime(rpa42b), getUtilTime(rpa42c), getUtilTime(rpa42d), getUtilTime(rpa42e),
                getUtilTime(rpa42f), getUtilTime(rpa42g), getUtilTime(rpa42h), getUtilTime(rpa42i), getUtilTime(rpa42j),
                getUtilTime(rpa42k), getUtilTime(rpa42l), getUtilTime(rpa42m), getUtilTime(rpa42n), getUtilTime(rpa42o),
                getUtilTime(rpa63a), getUtilTime(rpa63b), getUtilTime(rpa63c), getUtilTime(rpa63d), getUtilTime(rpa63e),
                getUtilTime(rpa63f), getUtilTime(rpa63g), getUtilTime(rpa63h), getUtilTime(rpa63i), getUtilTime(rpa63j),
                getUtilTime(rpa63k), getUtilTime(rpa63l), getUtilTime(rpa63m), getUtilTime(rpa63n), getUtilTime(rpa63o),
                .id = 'sim') %>% 
              mutate(
                size = 21*(floor(as.numeric(sim)/15.01) + 1),
                level = (as.numeric(sim) -1) %% 5 + 1,
                test = ceiling((as.numeric(sim) - 15*(floor(as.numeric(sim)/15.01)))/5)
              )   


# Save all the data objects
save(queueTimesRPA, file = 'Data/queueTimesRPA.Rda')
save(utilTimesRPA, file = 'Data/utilTimesRPA.Rda')


########################################################################################################

# Small scale GP or community pharmacy models

# 3 staff

gp4a <- list()
gp4b <- list()
gp4c <- list()
gp4d <- list()
gp4e <- list()
gp4f <- list()
gp4g <- list()
gp4h <- list()
gp4i <- list()
gp4j <- list()
gp4k <- list()
gp4l <- list()
gp4m <- list()
gp4n <- list()
gp4o <- list()

for(i in 1:simN){
  ##  Delays in vaccination time
  gp4a[[i]] <- gpModel(arrivals = arrivals(1, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
  gp4b[[i]] <- gpModel(arrivals = arrivals(2, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
  gp4c[[i]] <- gpModel(arrivals = arrivals(3, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
  gp4d[[i]] <- gpModel(arrivals = arrivals(4, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
  gp4e[[i]] <- gpModel(arrivals = arrivals(5, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
  
  ## Decreases in staff numbers
  gp4f[[i]] <- gpModel(arrivals = arrivals(2, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
  gp4g[[i]] <- gpModel(arrivals = arrivals(2, 10, -3, 1, 0.02),  nRsi = 1, nReg = 1, nVac = 1)
  gp4h[[i]] <- gpModel(arrivals = arrivals(2, 10, -3, 1, 0.02),  nRsi = 1, nReg = 1, nVac = 1)
  gp4i[[i]] <- gpModel(arrivals = arrivals(2, 10, -3, 1, 0.02),  nRsi = 1, nReg = 1, nVac = 1)
  gp4j[[i]] <- gpModel(arrivals = arrivals(2, 10, -3, 1, 0.02),  nRsi = 1, nReg = 1, nVac = 1)
  
  ## increases in arrival rate
  gp4k[[i]] <- gpModel(arrivals = arrivals(2+0, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
  gp4l[[i]] <- gpModel(arrivals = arrivals(2+1, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
  gp4m[[i]] <- gpModel(arrivals = arrivals(2+2, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
  gp4n[[i]] <- gpModel(arrivals = arrivals(2+3, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
  gp4o[[i]] <- gpModel(arrivals = arrivals(2+4, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
}


# 6 staff

gp8a <- list()
gp8b <- list()
gp8c <- list()
gp8d <- list()
gp8e <- list()
gp8f <- list()
gp8g <- list()
gp8h <- list()
gp8i <- list()
gp8j <- list()
gp8k <- list()
gp8l <- list()
gp8m <- list()
gp8n <- list()
gp8o <- list()

for(i in 1:simN){
  
  ##  Delays in vaccination time 
  gp8a[[i]] <- gpModel(arrivals = arrivals(1, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
  gp8b[[i]] <- gpModel(arrivals = arrivals(2, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
  gp8c[[i]] <- gpModel(arrivals = arrivals(4, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
  gp8d[[i]] <- gpModel(arrivals = arrivals(6, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
  gp8e[[i]] <- gpModel(arrivals = arrivals(10, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
  
  ## Decreases in staff numbers
  gp8f[[i]] <- gpModel(arrivals = arrivals(4, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
  gp8g[[i]] <- gpModel(arrivals = arrivals(4, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 3)
  gp8h[[i]] <- gpModel(arrivals = arrivals(4, 10, -3, 1, 0.02), nRsi = 1, nReg = 2, nVac = 3)
  gp8i[[i]] <- gpModel(arrivals = arrivals(4, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 3)
  gp8j[[i]] <- gpModel(arrivals = arrivals(4, 10, -3, 1, 0.02), nRsi = 1, nReg = 1, nVac = 2)
  
  ## increases in arrival rate
  gp8k[[i]] <- gpModel(arrivals = arrivals(4+0, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
  gp8l[[i]] <- gpModel(arrivals = arrivals(4+1, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
  gp8m[[i]] <- gpModel(arrivals = arrivals(4+2, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
  gp8n[[i]] <- gpModel(arrivals = arrivals(4+3, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
  gp8o[[i]] <- gpModel(arrivals = arrivals(4+4, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
}


# 9 staff

gp12a <- list()
gp12b <- list()
gp12c <- list()
gp12d <- list()
gp12e <- list()
gp12f <- list()
gp12g <- list()
gp12h <- list()
gp12i <- list()
gp12j <- list()
gp12k <- list()
gp12l <- list()
gp12m <- list()
gp12n <- list()
gp12o <- list()

for(i in 1:simN){
  ## Delays in vaccination time
  gp12a[[i]] <- gpModel(arrivals = arrivals(2, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 6)
  gp12b[[i]] <- gpModel(arrivals = arrivals(4, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 6)
  gp12c[[i]] <- gpModel(arrivals = arrivals(6, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 6)
  gp12d[[i]] <- gpModel(arrivals = arrivals(8, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 6)
  gp12e[[i]] <- gpModel(arrivals = arrivals(12, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 6)
  
  ## Decreases in staff numbers
  gp12f[[i]] <- gpModel(arrivals = arrivals(6, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 6)
  gp12g[[i]] <- gpModel(arrivals = arrivals(6, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 5)
  gp12h[[i]] <- gpModel(arrivals = arrivals(6, 10, -3, 1, 0.02), nRsi = 2, nReg = 3, nVac = 5)
  gp12i[[i]] <- gpModel(arrivals = arrivals(6, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 4)
  gp12j[[i]] <- gpModel(arrivals = arrivals(6, 10, -3, 1, 0.02), nRsi = 2, nReg = 2, nVac = 3)
  
  ## increases in arrival rate
  gp12k[[i]] <- gpModel(arrivals = arrivals(6+0, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 6)
  gp12l[[i]] <- gpModel(arrivals = arrivals(6+1, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 6)
  gp12m[[i]] <- gpModel(arrivals = arrivals(6+2, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 6)
  gp12n[[i]] <- gpModel(arrivals = arrivals(6+3, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 6)
  gp12o[[i]] <- gpModel(arrivals = arrivals(6+4, 10, -3, 1, 0.02), nRsi = 3, nReg = 3, nVac = 6)
}


## Compile all the queue time results
queueTimesGP <- bind_rows(
  getQueueTime(gp4a), getQueueTime(gp4b), getQueueTime(gp4c), getQueueTime(gp4d), getQueueTime(gp4e),
  getQueueTime(gp4f), getQueueTime(gp4g), getQueueTime(gp4h), getQueueTime(gp4i), getQueueTime(gp4j),
  getQueueTime(gp4k), getQueueTime(gp4l), getQueueTime(gp4m), getQueueTime(gp4n), getQueueTime(gp4o),
  getQueueTime(gp8a), getQueueTime(gp8b), getQueueTime(gp8c), getQueueTime(gp8d), getQueueTime(gp8e),
  getQueueTime(gp8f), getQueueTime(gp8g), getQueueTime(gp8h), getQueueTime(gp8i), getQueueTime(gp8j),
  getQueueTime(gp8k), getQueueTime(gp8l), getQueueTime(gp8m), getQueueTime(gp8n), getQueueTime(gp8o),
  getQueueTime(gp12a), getQueueTime(gp12b), getQueueTime(gp12c), getQueueTime(gp12d), getQueueTime(gp12e),
  getQueueTime(gp12f), getQueueTime(gp12g), getQueueTime(gp12h), getQueueTime(gp12i), getQueueTime(gp12j),
  getQueueTime(gp12k), getQueueTime(gp12l), getQueueTime(gp12m), getQueueTime(gp12n), getQueueTime(gp12o),
  .id = 'sim') %>% 
  mutate(
    size = 4*(floor(as.numeric(sim)/15.01) + 1),
    level = (as.numeric(sim) -1) %% 5 + 1,
    test = ceiling((as.numeric(sim) - 15*(floor(as.numeric(sim)/15.01)))/5)
  )  

# Compile all the utilisation time results
utilTimesGP <- bind_rows(
  getUtilTime(gp4a), getUtilTime(gp4b), getUtilTime(gp4c), getUtilTime(gp4d), getUtilTime(gp4e),
  getUtilTime(gp4f), getUtilTime(gp4g), getUtilTime(gp4h), getUtilTime(gp4i), getUtilTime(gp4j),
  getUtilTime(gp4k), getUtilTime(gp4l), getUtilTime(gp4m), getUtilTime(gp4n), getUtilTime(gp4o),
  getUtilTime(gp8a), getUtilTime(gp8b), getUtilTime(gp8c), getUtilTime(gp8d), getUtilTime(gp8e),
  getUtilTime(gp8f), getUtilTime(gp8g), getUtilTime(gp8h), getUtilTime(gp8i), getUtilTime(gp8j),
  getUtilTime(gp8k), getUtilTime(gp8l), getUtilTime(gp8m), getUtilTime(gp8n), getUtilTime(gp8o),
  getUtilTime(gp12a), getUtilTime(gp12b), getUtilTime(gp12c), getUtilTime(gp12d), getUtilTime(gp12e),
  getUtilTime(gp12f), getUtilTime(gp12g), getUtilTime(gp12h), getUtilTime(gp12i), getUtilTime(gp12j),
  getUtilTime(gp12k), getUtilTime(gp12l), getUtilTime(gp12m), getUtilTime(gp12n), getUtilTime(gp12o),
  .id = 'sim') %>% 
  mutate(
    size = 4*(floor(as.numeric(sim)/15.01) + 1),
    level = (as.numeric(sim) -1) %% 5 + 1,
    test = ceiling((as.numeric(sim) - 15*(floor(as.numeric(sim)/15.01)))/5)
  )   


# Save all the data objects
save(queueTimesGP, file = 'Data/queueTimesGP.Rda')
save(utilTimesGP, file = 'Data/utilTimesGP.Rda')
# ### JAGO ### #