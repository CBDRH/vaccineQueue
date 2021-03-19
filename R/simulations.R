# Purpose: Run simulations using the RPA arena-style model increasing the length of the registration time


# Source functions
source('R/arenaModel.R')
source('R/arrivals.R')
source('R/helperFunctions.R')


# Set the number of simulations for each scenario
simN <- 5 


# 10 staff

rpa10a <- list()
rpa10b <- list()
rpa10c <- list()
rpa10d <- list()
rpa10e <- list()
rpa10f <- list()
rpa10g <- list()
rpa10h <- list()
rpa10i <- list()
rpa10j <- list()
rpa10k <- list()
rpa10l <- list()
rpa10m <- list()
rpa10n <- list()
rpa10o <- list()

for(i in 1:simN){
  ##  Delays in registration time
  rpa10a[[i]] <- arenaModel(arrivals = arrivalsB(26, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 3, nAss = 2, nVac = 2, regRate = 1/2)
  rpa10b[[i]] <- arenaModel(arrivals = arrivalsB(26, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 3, nAss = 2, nVac = 2, regRate = 1/3)
  rpa10c[[i]] <- arenaModel(arrivals = arrivalsB(26, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 3, nAss = 2, nVac = 2, regRate = 1/4)
  rpa10d[[i]] <- arenaModel(arrivals = arrivalsB(26, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 3, nAss = 2, nVac = 2, regRate = 1/5)
  rpa10e[[i]] <- arenaModel(arrivals = arrivalsB(26, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 3, nAss = 2, nVac = 2, regRate = 1/6)

  ## Decreases in staff numbers
  rpa10f[[i]] <- arenaModel(arrivals = arrivalsB(26, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 3, nAss = 2, nVac = 2)
  rpa10g[[i]] <- arenaModel(arrivals = arrivalsB(26, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 2, nAss = 2, nVac = 2)
  rpa10h[[i]] <- arenaModel(arrivals = arrivalsB(26, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 2, nAss = 2, nVac = 1)
  rpa10i[[i]] <- arenaModel(arrivals = arrivalsB(26, 10, 0.02), nRsi = 2, nEnt = 1, nReg = 2, nAss = 2, nVac = 1)
  rpa10j[[i]] <- arenaModel(arrivals = arrivalsB(26, 10, 0.02), nRsi = 2, nEnt = 1, nReg = 2, nAss = 1, nVac = 1)

  ## increases in arrival rate
  rpa10k[[i]] <- arenaModel(arrivals = arrivalsB(26*1.0, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 3, nAss = 2, nVac = 2)
  rpa10l[[i]] <- arenaModel(arrivals = arrivalsB(26*1.1, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 3, nAss = 2, nVac = 2)
  rpa10m[[i]] <- arenaModel(arrivals = arrivalsB(26*1.2, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 3, nAss = 2, nVac = 2)
  rpa10n[[i]] <- arenaModel(arrivals = arrivalsB(26*1.3, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 3, nAss = 2, nVac = 2)
  rpa10o[[i]] <- arenaModel(arrivals = arrivalsB(26*1.4, 10, 0.02), nRsi = 2, nEnt = 2, nReg = 3, nAss = 2, nVac = 2)
}




# 20 staff


rpa20a <- list()
rpa20b <- list()
rpa20c <- list()
rpa20d <- list()
rpa20e <- list()
rpa20f <- list()
rpa20g <- list()
rpa20h <- list()
rpa20i <- list()
rpa20j <- list()
rpa20k <- list()
rpa20l <- list()
rpa20m <- list()
rpa20n <- list()
rpa20o <- list()

for(i in 1:simN){
  ##  Delays in registration time
  rpa20a[[i]] <- arenaModel(arrivals = arrivalsB(52, 10, 0.02), nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5, regRate = 1/2)
  rpa20b[[i]] <- arenaModel(arrivals = arrivalsB(52, 10, 0.02), nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5, regRate = 1/3)
  rpa20c[[i]] <- arenaModel(arrivals = arrivalsB(52, 10, 0.02), nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5, regRate = 1/4)
  rpa20d[[i]] <- arenaModel(arrivals = arrivalsB(52, 10, 0.02), nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5, regRate = 1/5)
  rpa20e[[i]] <- arenaModel(arrivals = arrivalsB(52, 10, 0.02), nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5, regRate = 1/6)

  ## Decreases in staff numbers
  rpa20f[[i]] <- arenaModel(arrivals = arrivalsB(52, 10, 0.02), nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5)
  rpa20g[[i]] <- arenaModel(arrivals = arrivalsB(52, 10, 0.02),  nRsi = 2, nEnt = 4, nReg = 4, nAss = 4, nVac = 5)
  rpa20h[[i]] <- arenaModel(arrivals = arrivalsB(52, 10, 0.02),  nRsi = 2, nEnt = 4, nReg = 4, nAss = 4, nVac = 4)
  rpa20i[[i]] <- arenaModel(arrivals = arrivalsB(52, 10, 0.02),  nRsi = 2, nEnt = 3, nReg = 4, nAss = 4, nVac = 4)
  rpa20j[[i]] <- arenaModel(arrivals = arrivalsB(52, 10, 0.02),  nRsi = 2, nEnt = 3, nReg = 4, nAss = 3, nVac = 4)

  ## increases in arrival rate
  rpa20k[[i]] <- arenaModel(arrivals = arrivalsB(52*1.0, 10, 0.02), nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5)
  rpa20l[[i]] <- arenaModel(arrivals = arrivalsB(52*1.1, 10, 0.02), nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5)
  rpa20m[[i]] <- arenaModel(arrivals = arrivalsB(52*1.2, 10, 0.02), nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5)
  rpa20n[[i]] <- arenaModel(arrivals = arrivalsB(52*1.3, 10, 0.02), nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5)
  rpa20o[[i]] <- arenaModel(arrivals = arrivalsB(52*1.4, 10, 0.02), nRsi = 2, nEnt = 4, nReg = 5, nAss = 4, nVac = 5)
}





# 30 staff

##  Delays in registration time
rpa30a <- list()
rpa30b <- list()
rpa30c <- list()
rpa30d <- list()
rpa30e <- list()
rpa30f <- list()
rpa30g <- list()
rpa30h <- list()
rpa30i <- list()
rpa30j <- list()
rpa30k <- list()
rpa30l <- list()
rpa30m <- list()
rpa30n <- list()
rpa30o <- list()

for(i in 1:simN){
  ##  Delays in registration time
  rpa30a[[i]] <- arenaModel(arrivals = arrivalsB(80, 10, 0.02), nRsi = 4, nEnt = 6, nReg = 8, nAss = 5, nVac = 7, regRate = 1/2)
  rpa30b[[i]] <- arenaModel(arrivals = arrivalsB(80, 10, 0.02), nRsi = 4, nEnt = 6, nReg = 8, nAss = 5, nVac = 7, regRate = 1/3)
  rpa30c[[i]] <- arenaModel(arrivals = arrivalsB(80, 10, 0.02), nRsi = 4, nEnt = 6, nReg = 8, nAss = 5, nVac = 7, regRate = 1/4)
  rpa30d[[i]] <- arenaModel(arrivals = arrivalsB(80, 10, 0.02), nRsi = 4, nEnt = 6, nReg = 8, nAss = 5, nVac = 7, regRate = 1/5)
  rpa30e[[i]] <- arenaModel(arrivals = arrivalsB(80, 10, 0.02), nRsi = 4, nEnt = 6, nReg = 8, nAss = 5, nVac = 7, regRate = 1/6)
  
  ## Decreases in staff numbers
  rpa30f[[i]] <- arenaModel(arrivals = arrivalsB(80, 10, 0.02), nRsi = 4, nEnt = 6, nReg = 8, nAss = 5, nVac = 7)
  rpa30g[[i]] <- arenaModel(arrivals = arrivalsB(80, 10, 0.02),  nRsi = 4, nEnt = 6, nReg = 7, nAss = 5, nVac = 7)
  rpa30h[[i]] <- arenaModel(arrivals = arrivalsB(80, 10, 0.02),  nRsi = 4, nEnt = 6, nReg = 7, nAss = 5, nVac = 6)
  rpa30i[[i]] <- arenaModel(arrivals = arrivalsB(80, 10, 0.02),  nRsi = 4, nEnt = 5, nReg = 7, nAss = 5, nVac = 6)
  rpa30j[[i]] <- arenaModel(arrivals = arrivalsB(80, 10, 0.02),  nRsi = 4, nEnt = 5, nReg = 7, nAss = 4, nVac = 6)
  
  ## increases in arrival rate
  rpa30k[[i]] <- arenaModel(arrivals = arrivalsB(80*1.0, 10, 0.02), nRsi = 4, nEnt = 6, nReg = 8, nAss = 5, nVac = 7)
  rpa30l[[i]] <- arenaModel(arrivals = arrivalsB(80*1.1, 10, 0.02), nRsi = 4, nEnt = 6, nReg = 8, nAss = 5, nVac = 7)
  rpa30m[[i]] <- arenaModel(arrivals = arrivalsB(80*1.2, 10, 0.02), nRsi = 4, nEnt = 6, nReg = 8, nAss = 5, nVac = 7)
  rpa30n[[i]] <- arenaModel(arrivals = arrivalsB(80*1.3, 10, 0.02), nRsi = 4, nEnt = 6, nReg = 8, nAss = 5, nVac = 7)
  rpa30o[[i]] <- arenaModel(arrivals = arrivalsB(80*1.4, 10, 0.02), nRsi = 4, nEnt = 6, nReg = 8, nAss = 5, nVac = 7)
}



# 40 staff

rpa40a <- list()
rpa40b <- list()
rpa40c <- list()
rpa40d <- list()
rpa40e <- list()
rpa40f <- list()
rpa40g <- list()
rpa40h <- list()
rpa40i <- list()
rpa40j <- list()
rpa40k <- list()
rpa40l <- list()
rpa40m <- list()
rpa40n <- list()
rpa40o <- list()

for(i in 1:simN){
  
  ##  Delays in registration time 
  rpa40a[[i]] <- arenaModel(arrivals = arrivalsB(110, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 11, nAss = 7, nVac = 9, regRate = 1/2)
  rpa40b[[i]] <- arenaModel(arrivals = arrivalsB(110, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 11, nAss = 7, nVac = 9, regRate = 1/3)
  rpa40c[[i]] <- arenaModel(arrivals = arrivalsB(110, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 11, nAss = 7, nVac = 9, regRate = 1/4)
  rpa40d[[i]] <- arenaModel(arrivals = arrivalsB(110, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 11, nAss = 7, nVac = 9, regRate = 1/5)
  rpa40e[[i]] <- arenaModel(arrivals = arrivalsB(110, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 11, nAss = 7, nVac = 9, regRate = 1/6)

  ## Decreases in staff numbers
  rpa40f[[i]] <- arenaModel(arrivals = arrivalsB(110, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 11, nAss = 7, nVac = 9)
  rpa40g[[i]] <- arenaModel(arrivals = arrivalsB(110, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 10, nAss = 7, nVac = 9)
  rpa40h[[i]] <- arenaModel(arrivals = arrivalsB(110, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 10, nAss = 7, nVac = 8)
  rpa40i[[i]] <- arenaModel(arrivals = arrivalsB(110, 10, 0.02), nRsi = 5, nEnt = 7, nReg = 10, nAss = 7, nVac = 8)
  rpa40j[[i]] <- arenaModel(arrivals = arrivalsB(110, 10, 0.02), nRsi = 5, nEnt = 7, nReg = 10, nAss = 6, nVac = 8)
  
  ## increases in arrival rate
  rpa40k[[i]] <- arenaModel(arrivals = arrivalsB(110*1.0, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 11, nAss = 7, nVac = 9)
  rpa40l[[i]] <- arenaModel(arrivals = arrivalsB(110*1.1, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 11, nAss = 7, nVac = 9)
  rpa40m[[i]] <- arenaModel(arrivals = arrivalsB(110*1.2, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 11, nAss = 7, nVac = 9)
  rpa40n[[i]] <- arenaModel(arrivals = arrivalsB(110*1.3, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 11, nAss = 7, nVac = 9)
  rpa40o[[i]] <- arenaModel(arrivals = arrivalsB(110*1.4, 10, 0.02), nRsi = 5, nEnt = 8, nReg = 11, nAss = 7, nVac = 9)
}




# 50 staff

rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()
rpa50a <- list()

for(i in 1:simN){
  ## Delays in registration time
  rpa50a[[i]] <- arenaModel(arrivals = arrivalsB(135, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 13, nAss = 9, nVac = 13, regRate = 1/2)
  rpa50b[[i]] <- arenaModel(arrivals = arrivalsB(135, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 13, nAss = 9, nVac = 13, regRate = 1/3)
  rpa50c[[i]] <- arenaModel(arrivals = arrivalsB(135, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 13, nAss = 9, nVac = 13, regRate = 1/4)
  rpa50d[[i]] <- arenaModel(arrivals = arrivalsB(135, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 13, nAss = 9, nVac = 13, regRate = 1/5)
  rpa50e[[i]] <- arenaModel(arrivals = arrivalsB(135, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 13, nAss = 9, nVac = 13, regRate = 1/6)
  
  ## Decreases in staff numbers
  rpa50f[[i]] <- arenaModel(arrivals = arrivalsB(135, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 13, nAss = 9, nVac = 13)
  rpa50g[[i]] <- arenaModel(arrivals = arrivalsB(135, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 12, nAss = 9, nVac = 13)
  rpa50h[[i]] <- arenaModel(arrivals = arrivalsB(135, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 12, nAss = 9, nVac = 12)
  rpa50i[[i]] <- arenaModel(arrivals = arrivalsB(135, 10, 0.02), nRsi = 6, nEnt = 8, nReg = 12, nAss = 9, nVac = 12)
  rpa50j[[i]] <- arenaModel(arrivals = arrivalsB(135, 10, 0.02), nRsi = 6, nEnt = 8, nReg = 12, nAss = 8, nVac = 12)
  
  ## increases in arrival rate
  rpa50k[[i]] <- arenaModel(arrivals = arrivalsB(135*1.0, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 12, nAss = 9, nVac = 13)
  rpa50l[[i]] <- arenaModel(arrivals = arrivalsB(135*1.1, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 13, nAss = 9, nVac = 13)
  rpa50m[[i]] <- arenaModel(arrivals = arrivalsB(135*1.2, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 13, nAss = 9, nVac = 13)
  rpa50n[[i]] <- arenaModel(arrivals = arrivalsB(135*1.3, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 13, nAss = 9, nVac = 13)
  rpa50o[[i]] <- arenaModel(arrivals = arrivalsB(135*1.4, 10, 0.02), nRsi = 6, nEnt = 9, nReg = 13, nAss = 9, nVac = 13)
}


## Compile all the queue time results
queueTimes <- bind_rows(
                getQueueTime(rpa10a), getQueueTime(rpa10b), getQueueTime(rpa10c), getQueueTime(rpa10d), getQueueTime(rpa10e),
                getQueueTime(rpa10f), getQueueTime(rpa10g), getQueueTime(rpa10h), getQueueTime(rpa10i), getQueueTime(rpa10j),
                getQueueTime(rpa10k), getQueueTime(rpa10l), getQueueTime(rpa10m), getQueueTime(rpa10n), getQueueTime(rpa10o),
                getQueueTime(rpa20a), getQueueTime(rpa20b), getQueueTime(rpa20c), getQueueTime(rpa20d), getQueueTime(rpa20e),
                getQueueTime(rpa20f), getQueueTime(rpa20g), getQueueTime(rpa20h), getQueueTime(rpa20i), getQueueTime(rpa20j),
                getQueueTime(rpa20k), getQueueTime(rpa20l), getQueueTime(rpa20m), getQueueTime(rpa20n), getQueueTime(rpa20o),
                getQueueTime(rpa30a), getQueueTime(rpa30b), getQueueTime(rpa30c), getQueueTime(rpa30d), getQueueTime(rpa30e),
                getQueueTime(rpa30f), getQueueTime(rpa30g), getQueueTime(rpa30h), getQueueTime(rpa30i), getQueueTime(rpa30j),
                getQueueTime(rpa30k), getQueueTime(rpa30l), getQueueTime(rpa30m), getQueueTime(rpa30n), getQueueTime(rpa30o),
                getQueueTime(rpa40a), getQueueTime(rpa40b), getQueueTime(rpa40c), getQueueTime(rpa40d), getQueueTime(rpa40e),
                getQueueTime(rpa40f), getQueueTime(rpa40g), getQueueTime(rpa40h), getQueueTime(rpa40i), getQueueTime(rpa40j),
                getQueueTime(rpa40k), getQueueTime(rpa40l), getQueueTime(rpa40m), getQueueTime(rpa40n), getQueueTime(rpa40o),
                getQueueTime(rpa50a), getQueueTime(rpa50b), getQueueTime(rpa50c), getQueueTime(rpa50d), getQueueTime(rpa50e),
                getQueueTime(rpa50f), getQueueTime(rpa50g), getQueueTime(rpa50h), getQueueTime(rpa50i), getQueueTime(rpa50j),
                getQueueTime(rpa50k), getQueueTime(rpa50l), getQueueTime(rpa50m), getQueueTime(rpa50n), getQueueTime(rpa50o),
                .id = 'sim') %>% 
              mutate(
                size = 10*(floor(as.numeric(sim)/15.01) + 1),
                level = (as.numeric(sim) -1) %% 5 + 1,
                test = ceiling((as.numeric(sim) - 15*(floor(as.numeric(sim)/15.01)))/5)
              )  

# Compile all the utilisation time results
utilTimes <- bind_rows(
                getArenaUtilTime(rpa10a), getArenaUtilTime(rpa10b), getArenaUtilTime(rpa10c), getArenaUtilTime(rpa10d), getArenaUtilTime(rpa10e),
                getArenaUtilTime(rpa10f), getArenaUtilTime(rpa10g), getArenaUtilTime(rpa10h), getArenaUtilTime(rpa10i), getArenaUtilTime(rpa10j),
                getArenaUtilTime(rpa10k), getArenaUtilTime(rpa10l), getArenaUtilTime(rpa10m), getArenaUtilTime(rpa10n), getArenaUtilTime(rpa10o),
                getArenaUtilTime(rpa20a), getArenaUtilTime(rpa20b), getArenaUtilTime(rpa20c), getArenaUtilTime(rpa20d), getArenaUtilTime(rpa20e),
                getArenaUtilTime(rpa20f), getArenaUtilTime(rpa20g), getArenaUtilTime(rpa20h), getArenaUtilTime(rpa20i), getArenaUtilTime(rpa20j),
                getArenaUtilTime(rpa20k), getArenaUtilTime(rpa20l), getArenaUtilTime(rpa20m), getArenaUtilTime(rpa20n), getArenaUtilTime(rpa20o),
                getArenaUtilTime(rpa30a), getArenaUtilTime(rpa30b), getArenaUtilTime(rpa30c), getArenaUtilTime(rpa30d), getArenaUtilTime(rpa30e),
                getArenaUtilTime(rpa30f), getArenaUtilTime(rpa30g), getArenaUtilTime(rpa30h), getArenaUtilTime(rpa30i), getArenaUtilTime(rpa30j),
                getArenaUtilTime(rpa30k), getArenaUtilTime(rpa30l), getArenaUtilTime(rpa30m), getArenaUtilTime(rpa30n), getArenaUtilTime(rpa30o),
                getArenaUtilTime(rpa40a), getArenaUtilTime(rpa40b), getArenaUtilTime(rpa40c), getArenaUtilTime(rpa40d), getArenaUtilTime(rpa40e),
                getArenaUtilTime(rpa40f), getArenaUtilTime(rpa40g), getArenaUtilTime(rpa40h), getArenaUtilTime(rpa40i), getArenaUtilTime(rpa40j),
                getArenaUtilTime(rpa40k), getArenaUtilTime(rpa40l), getArenaUtilTime(rpa40m), getArenaUtilTime(rpa40n), getArenaUtilTime(rpa40o),
                getArenaUtilTime(rpa50a), getArenaUtilTime(rpa50b), getArenaUtilTime(rpa50c), getArenaUtilTime(rpa50d), getArenaUtilTime(rpa50e),
                getArenaUtilTime(rpa50f), getArenaUtilTime(rpa50g), getArenaUtilTime(rpa50h), getArenaUtilTime(rpa50i), getArenaUtilTime(rpa50j),
                getArenaUtilTime(rpa50k), getArenaUtilTime(rpa50l), getArenaUtilTime(rpa50m), getArenaUtilTime(rpa50n), getArenaUtilTime(rpa50o),
                .id = 'sim') %>% 
              mutate(
                size = 10*(floor(as.numeric(sim)/15.01) + 1),
                level = (as.numeric(sim) -1) %% 5 + 1,
                test = ceiling((as.numeric(sim) - 15*(floor(as.numeric(sim)/15.01)))/5)
              )   


# Save all the data objects
save(queueTimes, file = 'Data/queueTimes.Rda')
save(utilTimes, file = 'Data/utilTimes.Rda')

# ### JAGO ### #